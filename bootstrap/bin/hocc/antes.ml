open Basis
open! Basis.Rudiments

(* The index of each element in t corresponds to a state index, and the array at each index contains
 * the corresponding antecedents' state indices. *)
type t = State.Index.t array array

let pp t formatter =
  let states =
    Ordmap.of_alist (module StateIndex) (Array.to_list (Array.mapi t ~f:(fun i elm -> i, elm))) in
  formatter |> Ordmap.fmt ~alt:true (Array.pp StateIndex.pp) states

let length t =
  Array.length t

let init states =
  let insert_ante ~state_index ~ante_state_index antes = begin
    Map.amend state_index ~f:(fun antes_opt ->
      let antes' = match antes_opt with
        | None -> Ordset.singleton (module State.Index) ante_state_index
        | Some antes -> Ordset.insert ante_state_index antes
      in
      Some antes'
    ) antes
  end in
  (* Incrementally initialaze a map of (state index -> antecedent index set). *)
  let antes_map = Array.fold ~init:(Map.empty (module State.Index))
    ~f:(fun antes
      State.{statenub={lr1itemsetclosure={index=ante_state_index; _}; _}; actions; gotos; _} ->
      let antes = Ordmap.fold ~init:antes ~f:(fun antes (_, action_set) ->
        Ordset.fold ~init:antes ~f:(fun antes action ->
          let open State.Action in
          match action with
          | ShiftPrefix state_index
          | ShiftAccept state_index -> insert_ante ~state_index ~ante_state_index antes
          | Reduce _ -> antes
        ) action_set
      ) actions in
      let antes = Ordmap.fold ~init:antes ~f:(fun antes (_, goto) ->
        insert_ante ~state_index:goto ~ante_state_index antes
      ) gotos in
      antes
    ) states
  in
  (* Convert the map to an array, which is sufficient for all lookup needs. *)
  Array.init (0L =:< Array.length states) ~f:(fun state_index ->
    match Map.get state_index antes_map with
    | None -> [||]
    | Some antes_set -> Ordset.to_array antes_set
  )

let antes_of_state_index state_index t =
  Array.get state_index t

let antes_of_state state t =
  antes_of_state_index (State.index state) t
