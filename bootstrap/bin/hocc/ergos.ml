open Basis
open! Basis.Rudiments

(* The index of each element in t corresponds to a state index, and the array at each index contains
 * the corresponding successors' state indices. *)
type t = State.Index.t array array

let pp t formatter =
  let states =
    Ordmap.of_alist (module StateIndex) (Array.to_list (Array.mapi t ~f:(fun i elm -> i, elm))) in
  formatter |> Ordmap.fmt ~alt:true (Array.pp StateIndex.pp) states

let init antes =
  let ergos_map = Range.Uns.fold (0L =:< Antes.length antes) ~init:(Map.empty (module State.Index))
    ~f:(fun ergos_map state_index ->
      let ante_indexes = Antes.antes_of_state_index state_index antes in
      Array.fold ~init:ergos_map ~f:(fun ergos_map ante_index ->
        Map.amend ante_index ~f:(function
          | None -> Some (Ordset.singleton (module State.Index) state_index)
          | Some ergos_set -> Some (Ordset.insert state_index ergos_set)
        ) ergos_map
      ) ante_indexes
    ) in
  Array.init (0L =:< Antes.length antes) ~f:(fun state_index ->
    match Map.get state_index ergos_map with
    | None -> [||]
    | Some state_index_set -> Ordset.to_array state_index_set
  )

let ergos_of_state_index state_index t =
  Array.get state_index t

let ergos_of_state state t =
  ergos_of_state_index (State.index state) t
