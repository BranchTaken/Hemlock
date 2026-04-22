(* This implementation of automaton reachability tracing traces reduce-goto paths through the graph.
 * A reduce action on a lookahead symbol induces edges back to corresponding goto entries within one
 * or more already-traced predecessors with distance equal to the number of symbols on the RHS of
 * the production. Those gotos induce edges to successor states' actions corresponding to the
 * lookahead symbol.
 *
 * There are some tricky issues glossed over in the above description:
 *
 * - Tracing must be at {state,action symbol} granularity rather than simply states, lest a goto
 *   inadvertently keep other actions live, which could transitively keep states live that are
 *   unreachable during parsing.
 * - In order for a goto to be reachable, its containing state must be reachable, and there must
 *   exist a reachable lane from that state to a successor that contains a corresponding reduce
 *   action. This implementation filters out states that do not meet these requirements, but no
 *   mechanism is implemented for recognizing which states to re-trace when a goto becomes
 *   reachable. The solution is to repeatedly scan all traced states until no new frontier edges are
 *   discovered.
 * - It's really expensive to find N-distant predecessors via graph traversal. This implementation
 *   transitions to reduce-goto tracing only when action tracing cannot make tracing progress on its
 *   own, and caches the graph traversal results to the degree possible. *)

open Basis
open! Basis.Rudiments

(* Shift can lead to all actions, whereas each reduce-goto can lead to precisely the one action
 * corresponding to the lookahead symbol. This type records `Shift` if a shift ipred edge is
 * present, the union of follow sets from reduce-goto ipred edges otherwise. *)
type reach =
  | Shift
  | Follows of (Symbol.Index.t, Symbol.Index.cmper_witness) Ordset.t

let reach_union _state_index reach0 reach1 =
  match reach0, reach1 with
  | Shift, _
  | _, Shift -> Shift
  | Follows actions0, Follows actions1 ->
    Follows (Ordset.union actions0 actions1)

(* `cache` contains `d=1` result in element 0, `d=2` result in element 1, etc. *)
let rec reachable_preds_of_state_index adjs ~traced state_index d cache =
  match d with
  | 0L -> Ordset.singleton (module State.Index) state_index, cache
  | _ -> begin
      match d <= (Array.length cache) with
      | true -> Array.get (pred d) cache, cache
      | false -> begin
          match d with
          | 0L -> not_reached ()
          | 1L -> begin
              let reachable_preds =
                Adjs.ipreds_of_state_index state_index adjs
                |> Array.fold ~init:(Ordset.empty (module State.Index))
                  ~f:(fun reachable_preds state_index ->
                    match Ordmap.mem state_index traced with
                    | false -> reachable_preds
                    | true -> Ordset.insert state_index reachable_preds
                  ) in
              let cache = [|reachable_preds|] in
              reachable_preds, cache
            end
          | _ -> begin
              let reachable_preds_prev, cache =
                reachable_preds_of_state_index adjs ~traced state_index (pred d) cache in
              let reachable_preds = Ordset.fold ~init:(Ordset.empty (module State.Index))
                ~f:(fun reachable_preds pred_state_index ->
                  Adjs.ipreds_of_state_index pred_state_index adjs
                  |> Array.filter ~f:(fun state_index ->
                    Ordmap.mem state_index traced
                  )
                  |> Ordset.of_array (module State.Index)
                  |> Ordset.union reachable_preds
                ) reachable_preds_prev in
              let cache = Array.init (0L =:< d) ~f:(fun i ->
                match i < (Array.length cache) with
                | true -> Array.get i cache
                | false -> reachable_preds
              ) in
              reachable_preds, cache
            end
        end
    end

(* Compute a pred->lookahead->gotos map based on backward traversal through traced states. *)
let pred_lookahead_gotos prods states adjs ~traced state_index =
  let pred_lookahead_gotos = Map.empty (module State.Index) in
  (* Cache reachable_preds for each distance; computing it is really expensive. *)
  let reachable_preds_cache = [||] in
  let State.{actions; _} = Array.get state_index states in
  let pred_lookahead_gotos, _reachable_preds_cache =
    Ordmap.fold ~init:(pred_lookahead_gotos, reachable_preds_cache)
      ~f:(fun (pred_lookahead_gotos, reachable_preds_cache) (symbol_index, action_set) ->
        Ordset.fold ~init:(pred_lookahead_gotos, reachable_preds_cache)
          ~f:(fun (pred_lookahead_gotos, reachable_preds_cache) action ->
            let open State.Action in
            match action with
            | ShiftPrefix _isucc_state_index
            | ShiftAccept _isucc_state_index -> pred_lookahead_gotos, reachable_preds_cache
            | Reduce prod_index -> begin
                let Prod.{lhs_index; rhs_indexes; _} =
                  Prods.prod_of_prod_index prod_index prods in
                let rhs_length = Array.length rhs_indexes in
                let pred_state_indexes, reachable_preds_cache =
                  reachable_preds_of_state_index adjs ~traced state_index rhs_length
                    reachable_preds_cache in
                let pred_lookahead_gotos =
                  Ordset.fold ~init:pred_lookahead_gotos
                    ~f:(fun pred_lookahead_gotos pred_state_index ->
                      let State.{gotos; _} = Array.get pred_state_index states in
                      match Ordmap.get lhs_index gotos with
                      | None -> pred_lookahead_gotos
                      | Some goto_state_index -> begin
                          Map.amend pred_state_index ~f:(fun lookahead_gotos_opt ->
                            match lookahead_gotos_opt with
                            | None -> begin
                                Some (Map.singleton (module Symbol.Index) ~k:symbol_index
                                  ~v:(Ordset.singleton (module State.Index) goto_state_index))
                              end
                            | Some lookahead_gotos ->
                              Some (Map.amend symbol_index ~f:(fun gotos_opt ->
                                match gotos_opt with
                                | None ->
                                  Some (Ordset.singleton (module State.Index) goto_state_index)
                                | Some gotos -> Some (Ordset.insert goto_state_index gotos)
                              ) lookahead_gotos)
                          ) pred_lookahead_gotos
                        end
                    ) pred_state_indexes in
                pred_lookahead_gotos, reachable_preds_cache
              end
          ) action_set
      ) actions
  in
  pred_lookahead_gotos

let reached_actions states state_index reach =
  let State.{actions; _} = Array.get state_index states in
  actions
  |> fun actions -> (match reach with
    | Shift -> actions
    | Follows reached_actions -> begin
        actions
        |> Ordmap.filter ~f:(fun (symbol_index, _action_set) ->
          Ordset.mem symbol_index reached_actions
        )
      end
  )

let trace_actions states ~traced ~frontier =
  Ordmap.fold ~init:(Ordmap.empty (module State.Index)) ~f:(fun frontier (state_index, reach) ->
    let reached_actions = reached_actions states state_index reach in
    Ordmap.fold ~init:frontier ~f:(fun frontier (_symbol_index, action_set) ->
      Ordset.fold ~init:frontier ~f:(fun frontier action ->
        let open State.Action in
        match action with
        | ShiftPrefix isucc_state_index
        | ShiftAccept isucc_state_index -> begin
            match Ordmap.get isucc_state_index traced with
            | Some Shift
              -> frontier
            | None
            | Some (Follows _)
              -> Ordmap.upsert ~k:isucc_state_index ~v:Shift frontier
          end
        | Reduce _prod_index -> frontier
      ) action_set
    ) reached_actions
  ) frontier

let trace_gotos prods states adjs ~traced ~frontier =
  Ordmap.fold ~init:(Ordmap.empty (module State.Index)) ~f:(fun frontier (state_index, reach) ->
    let reached_actions = reached_actions states state_index reach in
    let reached_lookaheads = Ordmap.fold ~init:(Ordset.empty (module Symbol.Index))
      ~f:(fun reached_lookaheads (symbol_index, action_set) ->
        Ordset.fold ~init:reached_lookaheads ~f:(fun reached_lookaheads action ->
          let open State.Action in
          match action with
          | ShiftPrefix _isucc_state_index
          | ShiftAccept _isucc_state_index -> reached_lookaheads
          | Reduce _prod_index -> Ordset.insert symbol_index reached_lookaheads
        ) action_set
      ) reached_actions in
    (* Merge into frontier. *)
    pred_lookahead_gotos prods states adjs ~traced state_index
    |> Map.fold ~init:frontier ~f:(fun frontier (_pred_state_index, lookahead_gotos) ->
      Map.fold ~init:frontier ~f:(fun frontier (lookahead, gotos) ->
        match Ordset.mem lookahead reached_lookaheads with
        | false -> frontier
        | true -> begin
            Ordset.fold ~init:frontier ~f:(fun frontier goto ->
              let in_traced =
                match Ordmap.get goto traced with
                | None -> false
                | Some Shift -> true
                | Some (Follows reach) -> Ordset.mem lookahead reach
              in
              match in_traced with
              | true -> frontier
              | false -> begin
                  Ordmap.amend goto ~f:(fun reach_opt ->
                    match reach_opt with
                    | None -> Some (Follows (Ordset.singleton (module Symbol.Index) lookahead))
                    | Some Shift -> reach_opt
                    | Some (Follows follows) -> Some (Follows (Ordset.insert lookahead follows))
                  ) frontier
                end
            ) gotos
          end
      ) lookahead_gotos
    )
  ) (Ordmap.union ~vunion:reach_union traced frontier)

let rec trace io prods states adjs ~traced ~frontier =
  let traced = Ordmap.union ~vunion:reach_union frontier traced in
  let frontier = trace_actions states ~traced ~frontier in
  match Ordmap.is_empty frontier with
  | false -> begin
      let io = io.log |> Fmt.fmt "." |> Io.with_log io in
      trace io prods states adjs ~traced ~frontier
    end
  | true -> begin
      let io = io.log |> Fmt.fmt "+" |> Io.with_log io in
      let frontier = trace_gotos prods states adjs ~traced ~frontier in
      match Ordmap.is_empty frontier with
      | true -> io, traced
      | false -> trace io prods states adjs ~traced ~frontier
    end

(* Trace the automaton in `t` from its roots (start states) and return the set of reachable states
 * and their corresponding reachable actions. *)
let reachable io prods states adjs =
  (* Gather the start states as roots. *)
  let frontier = Array.fold ~init:(Ordmap.empty (module State.Index)) ~f:(fun frontier state ->
    match State.is_start state with
    | false -> frontier
    | true -> Ordmap.insert_hlt ~k:(State.index state) ~v:Shift frontier
  ) states in
  trace io prods states adjs ~traced:(Ordmap.empty (module State.Index)) ~frontier

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
          | Some reach -> begin
              let follows = match reach with
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
