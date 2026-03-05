open Basis
open! Basis.Rudiments

type t = {
  states: State.t array;
  (* Map of state->pred->gotos that is used instead of repeated backward traversals of the state
   * graph during goto tracing passes. *)
  state_pred_gotos:
    (State.Index.t,
      (State.Index.t,
        (State.Index.t, State.Index.cmper_witness) Ordset.t,
        State.Index.cmper_witness) Map.t,
      State.Index.cmper_witness) Map.t;
}

let init_state_pred_gotos prods states =
  let adjs = Adjs.init states in
  Map.empty (module State.Index)
  |> (fun state_pred_gotos -> Array.fold ~init:state_pred_gotos ~f:(fun state_pred_gotos
    State.{statenub={lr1itemsetclosure={index=state_index; _}; _}; actions; _} ->
    let pred_gotos = Map.empty (module State.Index)
      |> (fun pred_gotos -> Ordmap.fold ~init:pred_gotos
          ~f:(fun pred_gotos (_symbol_index, action_set) ->
            Ordset.fold ~init:pred_gotos ~f:(fun pred_gotos action ->
              let open State.Action in
              match action with
              | ShiftPrefix _isucc_state_index
              | ShiftAccept _isucc_state_index -> pred_gotos
              | Reduce prod_index -> begin
                  let Prod.{lhs_index; rhs_indexes; _} =
                    Prods.prod_of_prod_index prod_index prods in
                  let rhs_length = Array.length rhs_indexes in
                  let pred_state_indexes = (match rhs_length with
                    | 0L -> Ordset.singleton (module State.Index) state_index
                    | _ -> Adjs.preds_of_state_index ~d:rhs_length state_index adjs
                  ) in
                  Ordset.fold ~init:pred_gotos ~f:(fun pred_gotos pred_state_index ->
                    let State.{gotos; _} = Array.get pred_state_index states in
                    match Ordmap.get lhs_index gotos with
                    | None -> pred_gotos
                    | Some goto_state_index -> begin
                        Map.amend pred_state_index ~f:(fun gotos_opt ->
                          let gotos = match gotos_opt with
                            | None -> Ordset.singleton (module State.Index) goto_state_index
                            | Some gotos -> Ordset.insert goto_state_index gotos
                          in
                          Some gotos
                        ) pred_gotos
                      end
                  ) pred_state_indexes
                end
            ) action_set
          ) actions) in
    Map.insert_hlt ~k:state_index ~v:pred_gotos state_pred_gotos
  ) states)

let init prods states =
  let state_pred_gotos = init_state_pred_gotos prods states in
  {states; state_pred_gotos}

let trace_actions ~traced ~frontier {states; _} =
  Ordset.fold ~init:(Ordset.empty (module State.Index)) ~f:(fun frontier state_index ->
    let State.{actions; _} = Array.get state_index states in
    Ordmap.fold ~init:frontier ~f:(fun frontier (_symbol_index, action_set) ->
      Ordset.fold ~init:frontier ~f:(fun frontier action ->
        let open State.Action in
        match action with
        | ShiftPrefix isucc_state_index
        | ShiftAccept isucc_state_index -> begin
            match Ordset.mem isucc_state_index traced with
            | true -> frontier
            | false -> Ordset.insert isucc_state_index frontier
          end
        | Reduce _prod_index -> frontier
      ) action_set
    ) actions
  ) frontier

let trace_gotos ~traced ~frontier {state_pred_gotos; _} =
  Ordset.fold ~init:(Ordset.empty (module State.Index)) ~f:(fun frontier state_index ->
    Map.get_hlt state_index state_pred_gotos
    |> Map.fold ~init:frontier ~f:(fun frontier (pred_state_index, gotos) ->
      (* Only trace gotos within already-traced states. *)
      match Ordset.mem pred_state_index traced with
      | false -> frontier
      | true -> begin
          (* Filter out already-traced gotos. *)
          let filtered_gotos = Ordset.diff gotos traced in
          Ordset.union filtered_gotos frontier
        end
    )
  ) (Ordset.union traced frontier)

let rec trace io t ~traced ~frontier =
  let io =
    io.log
    |> Fmt.fmt "."
    |> Io.with_log io
  in
  let traced = Ordset.union frontier traced in
  let actions_frontier = trace_actions ~traced ~frontier t in
  let gotos_frontier = trace_gotos ~traced ~frontier t in
  let frontier = Ordset.union actions_frontier gotos_frontier in
  match Ordset.is_empty frontier with
  | true -> io, traced
  | false -> trace io t ~traced ~frontier

let reachable_state_indexes io ({states; _} as t) =
  (* Gather the start states as roots. *)
  let frontier = Array.fold ~init:(Ordset.empty (module State.Index)) ~f:(fun frontier state ->
    match State.is_start state with
    | false -> frontier
    | true -> Ordset.insert (State.index state) frontier
  ) states in
  trace io t ~traced:(Ordset.empty (module State.Index)) ~frontier
