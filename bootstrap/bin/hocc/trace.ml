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
 *   exist a reachable path from that state to a successor that contains a corresponding reduce
 *   action. This implementation filters out states that do not meet these requirements, but no
 *   mechanism is implemented for recognizing which states to re-trace when a goto becomes
 *   reachable. The solution is to repeatedly scan all traced states until no new frontier edges are
 *   discovered.
 * - It's really expensive to find N-distant predecessors via graph traversal. This implementation
 *   transitions to reduce-goto tracing only when action tracing cannot make tracing progress on its
 *   own, and caches the graph traversal results to the degree possible. *)

open Basis
open! Basis.Rudiments

type t = {
  prods: Prods.t;
  states: State.t array;
  adjs: Adjs.t;
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

let rec reachable_preds_of_state_index ~d ~traced state_index adjs =
  match d with
  | 0L -> not_reached ()
  | 1L -> begin
      Adjs.ipreds_of_state_index state_index adjs
      |> Array.fold ~init:(Ordset.empty (module State.Index))
        ~f:(fun reachable_preds state_index ->
          match Ordmap.mem state_index traced with
          | false -> reachable_preds
          | true -> Ordset.insert state_index reachable_preds
        )
    end
  | _ -> begin
      Adjs.ipreds_of_state_index state_index adjs
      |> Array.filter ~f:(fun state_index ->
        Ordmap.mem state_index traced
      )
      |> Array.map ~f:(fun ipred_state_index ->
        reachable_preds_of_state_index ~d:(pred d) ~traced ipred_state_index adjs
      )
      |> Array.reduce ~f:Ordset.union
      |> Option.value ~default:(Ordset.empty (module State.Index))
    end

(* Compute a pred->lookahead->gotos map based on backward traversal through traced states. *)
let pred_lookahead_gotos ~traced state_index {prods; states; adjs; _} =
  let pred_lookahead_gotos = Map.empty (module State.Index) in
  (* Cache reachable_preds for each distance; computing it is really expensive. *)
  let d_reachable_preds = Map.empty (module Uns) in
  let State.{actions; _} = Array.get state_index states in
  let pred_lookahead_gotos, _d_reachable_preds =
    Ordmap.fold ~init:(pred_lookahead_gotos, d_reachable_preds)
      ~f:(fun (pred_lookahead_gotos, d_reachable_preds) (symbol_index, action_set) ->
        Ordset.fold ~init:(pred_lookahead_gotos, d_reachable_preds)
          ~f:(fun (pred_lookahead_gotos, d_reachable_preds) action ->
            let open State.Action in
            match action with
            | ShiftPrefix _isucc_state_index
            | ShiftAccept _isucc_state_index -> pred_lookahead_gotos, d_reachable_preds
            | Reduce prod_index -> begin
                let Prod.{lhs_index; rhs_indexes; _} =
                  Prods.prod_of_prod_index prod_index prods in
                let rhs_length = Array.length rhs_indexes in
                let pred_state_indexes, d_reachable_preds = (match rhs_length with
                  | 0L -> Ordset.singleton (module State.Index) state_index, d_reachable_preds
                  | _ -> begin
                      match Map.get rhs_length d_reachable_preds with
                      | Some reachable_preds -> reachable_preds, d_reachable_preds
                      | None -> begin
                          let reachable_preds =
                            reachable_preds_of_state_index ~d:rhs_length ~traced state_index adjs in
                          let d_reachable_preds =
                            Map.insert_hlt ~k:rhs_length ~v:reachable_preds d_reachable_preds in
                          reachable_preds, d_reachable_preds
                        end
                    end
                ) in
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
                pred_lookahead_gotos, d_reachable_preds
              end
          ) action_set
      ) actions
  in
  pred_lookahead_gotos

let init prods states =
  let adjs = Adjs.init states in
  {prods; states; adjs}

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

let trace_gotos ~traced ~frontier t =
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
    pred_lookahead_gotos ~traced state_index t
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
  ) (Ordmap.union ~f:reach_union traced frontier)

let rec trace io ~traced ~frontier t =
  let traced = Ordmap.union ~f:reach_union frontier traced in
  let frontier = trace_actions ~traced ~frontier t in
  match Ordmap.is_empty frontier with
  | false -> begin
      let io =
        io.log
        |> Fmt.fmt "."
        |> Io.with_log io
      in
      trace io ~traced ~frontier t
    end
  | true -> begin
      let io =
        io.log
        |> Fmt.fmt "+"
        |> Io.with_log io
      in
      let frontier = trace_gotos ~traced ~frontier t in
      match Ordmap.is_empty frontier with
      | true -> io, traced
      | false -> trace io ~traced ~frontier t
    end

let reachable io ({states; _} as t) =
  (* Gather the start states as roots. *)
  let frontier = Array.fold ~init:(Ordmap.empty (module State.Index)) ~f:(fun frontier state ->
    match State.is_start state with
    | false -> frontier
    | true -> Ordmap.insert ~k:(State.index state) ~v:Shift frontier
  ) states in
  trace io t ~traced:(Ordmap.empty (module State.Index)) ~frontier
