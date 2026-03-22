(* This implementation of automaton reachability tracing traces reduce-goto paths through the graph.
 * A reduce action on a lookahead symbol induces edges back to corresponding goto entries within one
 * or more already-traced predecessors with distance equal to the number of symbols on the RHS of
 * the production. Those gotos induce edges to successor states' actions corresponding to the
 * lookahead symbol.
 *
 * There are some tricky issues glossed over in the above description:
 *
 * - Otherwise-relevant gotos in not-yet-traced states must not be traced. This implementation
 *   filters out such states, but no mechanism is implemented for recognizing which states to
 *   re-trace when a goto's containing state becomes reachable. The solution is to repeatedly scan
 *   all traced states until no new frontier edges are discovered.
 * - It's really expensive to find N-distant predecessors via graph traversal. This implementation
 *   memoizes the results of graph traversal in a 4-level map to mitigate the cost.
 * - Tracing must be at {state,action symbol} granularity rather than simply states, lest a goto
 *   inadvertently keep other actions live, which could transitively keep states live that are
 *   unreachable during parsing. *)

open Basis
open! Basis.Rudiments

type t = {
  states: State.t array;
  (* Map of state->pred->lookahead->gotos that is used instead of repeated backward traversals of
   * the state graph during goto tracing passes. *)
  state_pred_lookahead_gotos:
    (State.Index.t,
      (State.Index.t,
        (Symbol.Index.t, (State.Index.t, State.Index.cmper_witness) Ordset.t,
          Symbol.Index.cmper_witness) Map.t,
        State.Index.cmper_witness) Map.t,
      State.Index.cmper_witness) Map.t;
}

type reach =
  | Shift
  | Follows of (Symbol.Index.t, Symbol.Index.cmper_witness) Ordset.t

let reach_union _state_index reach0 reach1 =
  match reach0, reach1 with
  | Shift, _
  | _, Shift -> Shift
  | Follows actions0, Follows actions1 ->
    Follows (Ordset.union actions0 actions1)

let init_state_pred_lookahead_gotos prods states =
  let adjs = Adjs.init states in
  Map.empty (module State.Index)
  |> fun state_pred_lookahead_gotos -> Array.fold ~init:state_pred_lookahead_gotos
    ~f:(fun state_pred_lookahead_gotos
      State.{statenub={lr1itemsetclosure={index=state_index; _}; _}; actions; _} ->
      let pred_lookahead_gotos = Map.empty (module State.Index)
        |> fun pred_lookahead_gotos -> Ordmap.fold ~init:pred_lookahead_gotos
          ~f:(fun pred_lookahead_gotos (symbol_index, action_set) ->
            Ordset.fold ~init:pred_lookahead_gotos ~f:(fun pred_lookahead_gotos action ->
              let open State.Action in
              match action with
              | ShiftPrefix _isucc_state_index
              | ShiftAccept _isucc_state_index -> pred_lookahead_gotos
              | Reduce prod_index -> begin
                  let Prod.{lhs_index; rhs_indexes; _} =
                    Prods.prod_of_prod_index prod_index prods in
                  let rhs_length = Array.length rhs_indexes in
                  let pred_state_indexes = (match rhs_length with
                    | 0L -> Ordset.singleton (module State.Index) state_index
                    | _ -> Adjs.preds_of_state_index ~d:rhs_length state_index adjs
                  ) in
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
                    ) pred_state_indexes
                end
            ) action_set
          ) actions in
      match Map.is_empty pred_lookahead_gotos with
      | true -> state_pred_lookahead_gotos
      | false -> Map.insert_hlt ~k:state_index ~v:pred_lookahead_gotos state_pred_lookahead_gotos
    ) states

let init prods states =
  let state_pred_lookahead_gotos = init_state_pred_lookahead_gotos prods states in
  {states; state_pred_lookahead_gotos}

let reached_actions state_index reach {states; _} =
  (Array.get state_index states).actions
  |> fun actions -> (match reach with
    | Shift -> actions
    | Follows reached_actions -> begin
        actions
        |> Ordmap.filter ~f:(fun (symbol_index, _action_set) ->
          Ordset.mem symbol_index reached_actions
        )
      end
  )

let trace_actions ~traced ~frontier t =
  Ordmap.fold ~init:(Ordmap.empty (module State.Index)) ~f:(fun frontier (state_index, reach) ->
    let reached_actions = reached_actions state_index reach t in
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

let trace_gotos ~traced ~frontier ({state_pred_lookahead_gotos; _} as t) =
  Ordmap.fold ~init:(Ordmap.empty (module State.Index)) ~f:(fun frontier (state_index, reach) ->
    let reached_actions = reached_actions state_index reach t in
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
    Map.get state_index state_pred_lookahead_gotos
    |> Option.value ~default:(Map.empty (module State.Index))
    |> Map.fold ~init:frontier ~f:(fun frontier (pred_state_index, lookahead_gotos) ->
      (* Only trace gotos within already-traced states. *)
      match Ordmap.mem pred_state_index traced with
      | false -> frontier
      | true -> begin
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
        end
    )
  ) (Ordmap.union ~f:reach_union traced frontier)

let rec trace io t ~traced ~frontier =
  let io =
    io.log
    |> Fmt.fmt "."
    |> Io.with_log io
  in
  let traced = Ordmap.union ~f:reach_union frontier traced in
  let actions_frontier = trace_actions ~traced ~frontier t in
  let gotos_frontier = trace_gotos ~traced ~frontier t in
  let frontier = Ordmap.union ~f:reach_union actions_frontier gotos_frontier in
  match Ordmap.is_empty frontier with
  | true -> io, traced
  | false -> trace io t ~traced ~frontier

let reachable io ({states; _} as t) =
  (* Gather the start states as roots. *)
  let frontier = Array.fold ~init:(Ordmap.empty (module State.Index)) ~f:(fun frontier state ->
    match State.is_start state with
    | false -> frontier
    | true -> Ordmap.insert ~k:(State.index state) ~v:Shift frontier
  ) states in
  trace io t ~traced:(Ordmap.empty (module State.Index)) ~frontier
