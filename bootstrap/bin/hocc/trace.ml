(* This implementation of automaton reachability tracing traces reduce-goto paths through the graph.
 * A reduce action on a lookahead symbol logically induces edges back to corresponding goto entries
 * within one or more already-traced predecessors with distance equal to the number of symbols on
 * the RHS of the production. Those gotos encode transits to successor states' actions corresponding
 * to the lookahead symbol.
 *
 * There are some tricky issues glossed over in the above description:
 *
 * - Tracing must be at {state,{action symbol,goto}} granularity rather than simply states, lest a goto
 *   inadvertently keep other actions live, which could transitively keep states live that are
 *   unreachable during parsing.
 * - In order for a goto to be reachable, its containing state must be reachable, and there must
 *   exist a reachable lane from that state to a successor that contains a corresponding reduce
 *   action. This implementation filters out states that do not meet these requirements, but no
 *   mechanism is implemented for recognizing which states to re-trace when a goto becomes
 *   reachable. The solution is to repeatedly scan all traced states that contain reduce actions
 *   associated with previously untraced gotos until no new frontier edges are discovered.
 * - It's really expensive to find N-distant predecessors via graph traversal. This implementation
 *   transitions to reduce-goto tracing only when action tracing cannot make tracing progress on its
 *   own, and caches the graph traversal results to the degree possible. *)

open Basis
open! Basis.Rudiments

module DepthPredLookahead = struct
  module T = struct
    type t = {
      depth: uns;
      pred_state_index: State.Index.t;
      lookahead_index: Symbol.Index.t;
    }

    let hash_fold {depth; pred_state_index; lookahead_index} state =
      state
      |> Uns.hash_fold depth
      |> State.Index.hash_fold pred_state_index
      |> Symbol.Index.hash_fold lookahead_index

    let cmp {depth=d0; pred_state_index=psi0; lookahead_index=li0}
      {depth=d1; pred_state_index=psi1; lookahead_index=li1} =
      let open Cmp in
      match Uns.cmp d0 d1 with
      | Lt -> Lt
      | Eq -> begin
          match State.Index.cmp psi0 psi1 with
          | Lt -> Lt
          | Eq -> Symbol.Index.cmp li0 li1
          | Gt -> Gt
        end
      | Gt -> Gt

    let pp {depth; pred_state_index; lookahead_index} formatter =
      formatter
      |> Fmt.fmt "{depth=" |> Uns.pp depth
      |> Fmt.fmt "; pred_state_index=" |> State.Index.pp pred_state_index
      |> Fmt.fmt "; lookahead_index=" |> Symbol.Index.pp lookahead_index
      |> Fmt.fmt "}"
  end
  include T
  include Identifiable.Make(T)

  let init ~depth ~pred_state_index ~lookahead_index =
    {depth; pred_state_index; lookahead_index}
end

module Reach = struct
  (* Shift can lead to all actions, whereas each reduce-goto can lead to precisely the one action
   * corresponding to the lookahead symbol. This type records `Shift` if a shift ipred transit has
   * been traced, the union of follow sets from traced reduce-goto ipred transits otherwise. *)
  type actions =
    | Shift
    | Follows of (Symbol.Index.t, Symbol.Index.cmper_witness) Ordset.t

  type t = {
    actions: actions;
    (* Record which gotos have been traced to facilitate filtering untraced goto transits. *)
    gotos: (State.Index.t, State.Index.cmper_witness) Ordset.t;
  }

  let actions_shift = {actions=Shift; gotos=Ordset.empty (module State.Index)}

  let init_actions_follow follow =
    {
      actions=Follows (Ordset.singleton (module Symbol.Index) follow);
      gotos=Ordset.empty (module State.Index)
    }

  let insert_actions_follow follow ({actions; _} as t) =
    match actions with
    | Shift -> not_reached ()
    | Follows follows -> {t with actions=Follows (Ordset.insert follow follows)}

  let insert_gotos gotos ({gotos=gotos_existing; _} as t) =
    let gotos = Ordset.union gotos gotos_existing in
    {t with gotos}

  let union {actions=a0; gotos=g0} {actions=a1; gotos=g1} =
    let actions = match a0, a1 with
      | Shift, _
      | _, Shift -> Shift
      | Follows actions0, Follows actions1 -> Follows (Ordset.union actions0 actions1)
    in
    let gotos = Ordset.union g0 g1 in
    {actions; gotos}
end

(* Cache filtered preds for each distance; computing it is really expensive. *)
let rec filtered_preds_of_state_index adjs ~filter state_index d cache =
  match d < (Array.length cache) with
  | true -> Array.get d cache, cache
  | false -> begin
      let reachable_preds, cache = match d with
        | 0L -> begin
            let filter' = begin
              let reachable_isuccs = Ordset.empty (module State.Index) in
              filter reachable_isuccs
            end in
            Ordset.singleton (module State.Index) state_index
            |> Ordset.filter ~f:filter', cache
          end
        | _ -> begin
            let reachable_preds_pred, cache =
              filtered_preds_of_state_index adjs ~filter state_index (pred d) cache in
            let filter' = begin
              let reachable_isuccs = Array.get (pred d) cache in
              filter reachable_isuccs
            end in
            let reachable_preds =
              reachable_preds_pred
              |> Ordset.fold ~init:(Ordset.empty (module State.Index))
                ~f:(fun reachable_preds pred_state_index ->
                  Adjs.ipreds_of_state_index pred_state_index adjs
                  |> Array.fold ~init:reachable_preds ~f:(fun reachable_preds state_index ->
                    Ordset.insert state_index reachable_preds
                  )
                )
              |> Ordset.filter ~f:filter'
            in
            reachable_preds, cache
          end
      in
      let cache = Array.append reachable_preds cache in
      reachable_preds, cache
    end

(* Compute a {depth,pred,lookahead}->gotos map based on backward traversal through traced states. *)
let depth_pred_lookahead_gotos prods states adjs ~filter state_index =
  let depth_pred_lookahead_gotos = Ordmap.empty (module DepthPredLookahead) in
  let reachable_preds_cache = [||] in
  let State.{actions; _} = Array.get state_index states in
  let depth_pred_lookahead_gotos, _reachable_preds_cache =
    Ordmap.fold ~init:(depth_pred_lookahead_gotos, reachable_preds_cache)
      ~f:(fun (depth_pred_lookahead_gotos, reachable_preds_cache) (symbol_index, action_set) ->
        Ordset.fold ~init:(depth_pred_lookahead_gotos, reachable_preds_cache)
          ~f:(fun (depth_pred_lookahead_gotos, reachable_preds_cache) action ->
            let open State.Action in
            match action with
            | ShiftPrefix _isucc_state_index
            | ShiftAccept _isucc_state_index -> depth_pred_lookahead_gotos, reachable_preds_cache
            | Reduce prod_index -> begin
                let Prod.{lhs_index; rhs_indexes; _} =
                  Prods.prod_of_prod_index prod_index prods in
                let depth = Array.length rhs_indexes in
                let pred_state_indexes, reachable_preds_cache =
                  filtered_preds_of_state_index adjs ~filter state_index depth
                    reachable_preds_cache in
                let depth_pred_lookahead_gotos =
                  Ordset.fold ~init:depth_pred_lookahead_gotos
                    ~f:(fun depth_pred_lookahead_gotos pred_state_index ->
                      let State.{gotos; _} = Array.get pred_state_index states in
                      match Ordmap.get lhs_index gotos with
                      | None -> depth_pred_lookahead_gotos
                      | Some goto_state_index -> begin
                          let depth_pred_lookahead = DepthPredLookahead.init ~depth
                              ~pred_state_index ~lookahead_index:symbol_index in
                          Ordmap.amend depth_pred_lookahead ~f:(fun gotos_opt ->
                            match gotos_opt with
                            | None -> Some (Ordset.singleton (module State.Index) goto_state_index)
                            | Some gotos -> Some (Ordset.insert goto_state_index gotos)
                          ) depth_pred_lookahead_gotos
                        end
                    ) pred_state_indexes in
                depth_pred_lookahead_gotos, reachable_preds_cache
              end
          ) action_set
      ) actions
  in
  depth_pred_lookahead_gotos

(* Filter unreachable entries out of `untraced_depth_pred_lookahead_gotos`. *)
let reachable_depth_pred_lookahead_gotos states adjs ~traced untraced_depth_pred_lookahead_gotos
    state_index =
  let filter reachable_isuccs state_index = begin
    match Ordmap.get state_index traced with
    | None -> false
    | Some Reach.{actions=actions_reach; gotos} -> begin
        let State.{actions; _} = Array.get state_index states in
        (* Look for a reachable action/goto that transits to a reachable isucc. *)
        match Ordset.is_empty reachable_isuccs with
        | true -> true (* Current state is reduce-containing start of backward trace. *)
        | _ -> begin
            actions |> Ordmap.for_any ~f:(fun (symbol_index, action_set) ->
              (match actions_reach with
                | Shift -> true
                | Follows follows -> Ordset.mem symbol_index follows
              )
              && action_set |> Ordset.for_any ~f:(fun action ->
                let open State.Action in
                match action with
                | ShiftPrefix isucc_state_index
                | ShiftAccept isucc_state_index -> Ordset.mem isucc_state_index reachable_isuccs
                | Reduce _ -> false
              )
            )
            || gotos |> Ordset.for_any ~f:(fun isucc_state_index ->
              Ordset.mem isucc_state_index reachable_isuccs
            )
          end
      end
  end in
  let reachable_depth_pred_lookahead_gotos = Ordmap.empty (module DepthPredLookahead) in
  let reachable_preds_cache = [||] in
  let reachable_depth_pred_lookahead_gotos, _reachable_preds_cache =
    Ordmap.fold ~init:(reachable_depth_pred_lookahead_gotos, reachable_preds_cache)
      ~f:(fun (reachable_depth_pred_lookahead_gotos, reachable_preds_cache)
        (DepthPredLookahead.{depth; pred_state_index; _} as depth_pred_lookahead, gotos) ->
        let reachable_preds, reachable_preds_cache =
          filtered_preds_of_state_index adjs ~filter state_index depth reachable_preds_cache in
        let reachable_depth_pred_lookahead_gotos =
          match Ordset.mem pred_state_index reachable_preds with
          | false -> reachable_depth_pred_lookahead_gotos
          | true ->
            Ordmap.insert_hlt ~k:depth_pred_lookahead ~v:gotos reachable_depth_pred_lookahead_gotos
        in
        reachable_depth_pred_lookahead_gotos, reachable_preds_cache
      ) untraced_depth_pred_lookahead_gotos
  in
  reachable_depth_pred_lookahead_gotos

let reached_actions State.{actions; _} reach =
  actions
  |> fun actions -> (match reach with
    | Reach.{actions=Shift; _} -> actions
    | {actions=Follows reached_actions; _} -> begin
        actions
        |> Ordmap.filter ~f:(fun (symbol_index, _action_set) ->
          Ordset.mem symbol_index reached_actions
        )
      end
  )

let trace_actions states ~traced ~frontier =
  Ordmap.fold ~init:(Ordmap.empty (module State.Index)) ~f:(fun frontier (state_index, reach) ->
    let state = Array.get state_index states in
    let reached_actions = reached_actions state reach in
    Ordmap.fold ~init:frontier ~f:(fun frontier (_symbol_index, action_set) ->
      Ordset.fold ~init:frontier ~f:(fun frontier action ->
        let open State.Action in
        match action with
        | ShiftPrefix isucc_state_index
        | ShiftAccept isucc_state_index -> begin
            match Ordmap.get isucc_state_index traced with
            | Some Reach.{actions=Shift; _}
              -> frontier
            | None
            | Some {actions=Follows _; _}
              -> Ordmap.upsert ~k:isucc_state_index ~v:Reach.actions_shift frontier
          end
        | Reduce _prod_index -> frontier
      ) action_set
    ) reached_actions
  ) frontier

let reached_lookaheads states state_index reach =
  let state = Array.get state_index states in
  let reached_actions = reached_actions state reach in
  Ordmap.fold ~init:(Ordset.empty (module Symbol.Index))
    ~f:(fun reached_lookaheads (symbol_index, action_set) ->
      Ordset.fold ~init:reached_lookaheads ~f:(fun reached_lookaheads action ->
        let open State.Action in
        match action with
        | ShiftPrefix _isucc_state_index
        | ShiftAccept _isucc_state_index -> reached_lookaheads
        | Reduce _prod_index -> Ordset.insert symbol_index reached_lookaheads
      ) action_set
    ) reached_actions

let trace_gotos states adjs ~untraced_gotos ~traced =
  let frontier = Ordmap.empty (module State.Index) in
  Ordmap.fold ~init:(untraced_gotos, traced, frontier)
    ~f:(fun (untraced_gotos, traced, frontier) (state_index, untraced_depth_pred_lookahead_gotos) ->
      match Ordmap.get state_index traced with
      | None -> untraced_gotos, traced, frontier
      | Some reach -> begin
          let reachable_depth_pred_lookahead_gotos =
            reachable_depth_pred_lookahead_gotos states adjs ~traced
              untraced_depth_pred_lookahead_gotos state_index in
          match Ordmap.is_empty reachable_depth_pred_lookahead_gotos with
          | true -> untraced_gotos, traced, frontier
          | false -> begin
              let reached_lookaheads = reached_lookaheads states state_index reach in
              let untraced_gotos, traced, frontier =
                reachable_depth_pred_lookahead_gotos
                |> Ordmap.fold ~init:(untraced_gotos, traced, frontier)
                  ~f:(fun (untraced_gotos, traced, frontier)
                    (DepthPredLookahead.{pred_state_index; lookahead_index; _} as
                      depth_pred_lookahead, reachable_gotos)
                    ->
                      match Ordset.mem lookahead_index reached_lookaheads with
                      | false -> untraced_gotos, traced, frontier
                      | true -> begin
                          (* Merge goto into reach. *)
                          let traced = Ordmap.amend pred_state_index ~f:(fun reach_opt ->
                            match reach_opt with
                            | None -> not_reached ()
                            | Some reach ->
                              Some (Reach.insert_gotos reachable_gotos reach)
                          ) traced in
                          let frontier = Ordset.fold ~init:frontier ~f:(fun frontier goto ->
                            (* Merge goto isucc state into frontier if not already traced. *)
                            let in_traced =
                              match Ordmap.get goto traced with
                              | None -> false
                              | Some Reach.{actions=Shift; _} -> true
                              | Some {actions=Follows reached_actions; _} ->
                                Ordset.mem lookahead_index reached_actions
                            in
                            let frontier = match in_traced with
                              | true -> frontier
                              | false -> begin
                                  let frontier = Ordmap.amend goto ~f:(fun reach_opt ->
                                    match reach_opt with
                                    | None -> Some (Reach.init_actions_follow lookahead_index)
                                    | Some Reach.{actions=Shift; _} -> reach_opt
                                    | Some ({actions=Follows _; _} as reach) ->
                                      Some (Reach.insert_actions_follow lookahead_index reach)
                                  ) frontier in
                                  frontier
                                end
                            in
                            frontier
                          ) reachable_gotos
                          in
                          (* Remove traced gotos to avoid repeated tracing. *)
                          let untraced_gotos =
                            Ordmap.amend state_index ~f:(fun depth_pred_lookahead_gotos_opt ->
                              match depth_pred_lookahead_gotos_opt with
                              | None -> None
                              | Some depth_pred_lookahead_gotos -> begin
                                  let depth_pred_lookahead_gotos =
                                    Ordmap.amend depth_pred_lookahead ~f:(fun gotos_opt ->
                                      match gotos_opt with
                                      | None -> None
                                      | Some gotos -> begin
                                          let gotos = Ordset.diff gotos reachable_gotos in
                                          match Ordset.is_empty gotos with
                                          | true -> None
                                          | false -> Some gotos
                                        end
                                    ) depth_pred_lookahead_gotos in
                                  match Ordmap.is_empty depth_pred_lookahead_gotos with
                                  | true -> None
                                  | false -> Some depth_pred_lookahead_gotos
                                end
                            ) untraced_gotos
                          in
                          untraced_gotos, traced, frontier
                        end
                  ) in
              untraced_gotos, traced, frontier
            end
        end
    ) untraced_gotos

let rec trace io prods states adjs ~untraced_gotos ~traced ~frontier =
  let traced = Ordmap.union ~vunion:(fun _state_index -> Reach.union) frontier traced in
  let frontier = trace_actions states ~traced ~frontier in
  match Ordmap.is_empty frontier with
  | false -> begin
      let io = io.log |> Fmt.fmt "." |> Io.with_log io in
      trace io prods states adjs ~untraced_gotos ~traced ~frontier
    end
  | true -> begin
      let io = io.log |> Fmt.fmt "+" |> Io.with_log io in
      let untraced_gotos, traced, frontier = trace_gotos states adjs ~untraced_gotos ~traced in
      match Ordmap.is_empty frontier with
      | true -> io, traced
      | false -> trace io prods states adjs ~untraced_gotos ~traced ~frontier
    end

(* Trace the automaton in `t` from its roots (start states) and return the set of reachable states
 * and their corresponding reachable actions. *)
let reachable io prods states adjs =
  (* Compute a state->{depth,pred,lookahead}->gotos map that contains entries for all reduce
   * actions, such that it is possible to look up the set of untraced reduce-gotos for a reduce
   * action. Map entries are removed during goto tracing when they are reached via a reduce-goto
   * trace. *)
  let io = io.log |> Fmt.fmt "+" |> Io.with_log io in (* Tracing all untraced gotos. *)
  let untraced_gotos = Array.fold ~init:(Ordmap.empty (module State.Index))
    ~f:(fun untraced_gotos state ->
      let state_index = State.index state in
      let depth_pred_lookahead_gotos =
        depth_pred_lookahead_gotos prods states adjs ~filter:(fun _ _ -> true) state_index in
      match Ordmap.is_empty depth_pred_lookahead_gotos with
      | true -> untraced_gotos
      | false -> Ordmap.insert_hlt ~k:state_index ~v:depth_pred_lookahead_gotos untraced_gotos
    ) states in
  let traced = Ordmap.empty (module State.Index) in
  (* Gather the start states as roots. *)
  let frontier = Array.fold ~init:(Ordmap.empty (module State.Index)) ~f:(fun frontier state ->
    match State.is_start state with
    | false -> frontier
    | true -> begin
        let reach = Reach.actions_shift in
        Ordmap.insert_hlt ~k:(State.index state) ~v:reach frontier
      end
  ) states in
  trace io prods states adjs ~untraced_gotos ~traced ~frontier

let gc_states io prods isocores states =
  let io = io.log |> Fmt.fmt "hocc: Tracing automaton (.+=actions[+gotos])" |> Io.with_log io in
  let adjs = Adjs.init states in
  let io, reachable = reachable io prods states adjs in
  let io = io.log |> Fmt.fmt "\n" |> Io.with_log io in
  let unreachable_statenubs = Array.fold ~init:(Ordset.empty (module StateNub))
    ~f:(fun unreachable State.{statenub; _} ->
      let index = StateNub.index statenub in
      match Ordmap.mem index reachable with
      | true -> unreachable
      | false -> Ordset.insert statenub unreachable
    ) states in
  let nreachable = Ordmap.length reachable in
  let nunreachable = Ordset.length unreachable_statenubs in
  assert (Uns.(nreachable + nunreachable = Array.length states));
  let io =
    io.log
    |> Fmt.fmt "hocc: " |> Uns.pp nunreachable |> Fmt.fmt " unreachable state"
    |> (fun formatter ->
      match nunreachable with 1L -> formatter | _ -> formatter |> Fmt.fmt "s"
    )
    |> Fmt.fmt "\n"
    |> Io.with_log io
  in
  match nunreachable with
  | 0L -> io, isocores, states
  | _ -> begin
      let io =
        io.log
        |> Fmt.fmt "hocc: Reindexing " |> Uns.pp nreachable |> Fmt.fmt " LR(1) state"
        |> (fun formatter -> match nreachable with 1L -> formatter | _ -> formatter |> Fmt.fmt "s")
        |> Io.with_log io
      in
      let remaining_state_indexes = Ordmap.fold ~init:(Ordset.empty (module State.Index))
        ~f:(fun remaining_state_indexes (state_index, _reach) ->
          Ordset.insert state_index remaining_state_indexes
        ) reachable in
      (* Create a map of pre-GC state indexes to post-GC state indexes. *)
      let state_index_map = StateIndexMap.init ~remaining_state_indexes
          ~remergeable_index_map:(Ordmap.empty (module StateIndex))
          ~isocores_sn_of_state_index:(fun state_index ->
            let State.{statenub={isocores_sn; _}; _} = Array.get state_index states in
            isocores_sn
          ) in
      (* Create a new set of reindexed isocores. *)
      let reindexed_isocores =
        Ordset.fold ~init:isocores ~f:(fun isocores statenub ->
          Isocores.remove_hlt statenub isocores
        ) unreachable_statenubs
        |> Isocores.reindex state_index_map in
      (* Create a new set of reindexed states. *)
      let reindexed_states =
        Array.fold ~init:(Ordset.empty (module State)) ~f:(fun reindexed_states state ->
          let state_index = State.index state in
          match Ordmap.get state_index reachable with
          | None -> reindexed_states
          | Some {actions; _} -> begin
              let follows = match actions with
                | Shift -> None
                | Follows follows -> Some follows
              in
              let reindexed_state = State.reindex state_index_map follows state in
              Ordset.insert reindexed_state reindexed_states
            end
        ) states
        |> Ordset.to_array in
      let io = io.log |> Fmt.fmt "\n" |> Io.with_log io in
      io, reindexed_isocores, reindexed_states
    end
