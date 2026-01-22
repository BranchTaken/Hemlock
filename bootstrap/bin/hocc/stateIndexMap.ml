open Basis
open! Basis.Rudiments

type v = {
  (* Reindexed state index. *)
  reindexed_state_index: StateIndex.t;
  (* Reindexed isocore set serial number. *)
  reindexed_isocore_set_sn: uns;
}

type t = (StateIndex.t, v, StateIndex.cmper_witness) Map.t

let init ~remaining_state_indexes:remaining_state_indexes ~remergeable_index_map
    ~isocores_sn_of_state_index =
  let isocore_set_sizes = Map.empty (module Uns) in
  let t = Map.empty (module StateIndex) in
  let _, t = Ordset.fold ~init:(isocore_set_sizes, t) ~f:(fun (isocore_set_sizes, t) state_index ->
    let isocores_sn = isocores_sn_of_state_index state_index in
    let isocore_set_sizes = Map.amend isocores_sn ~f:(fun set_size_opt ->
      match set_size_opt with
      | None -> Some 1L
      | Some set_size -> Some (succ set_size)
    ) isocore_set_sizes in
    let reindexed_state_index = Map.length t in
    let reindexed_isocore_set_sn = pred (Map.get_hlt isocores_sn isocore_set_sizes) in
    let t = Map.insert_hlt ~k:state_index ~v:{reindexed_state_index; reindexed_isocore_set_sn} t in
    isocore_set_sizes, t
  ) remaining_state_indexes in
  Ordmap.fold ~init:t ~f:(fun t (state_index_alias, state_index) ->
    let v = Map.get_hlt state_index t in
    Map.insert_hlt ~k:state_index_alias ~v t
  ) remergeable_index_map

let reindexed_state_index state_index t =
  let {reindexed_state_index; _} = Map.get_hlt state_index t in
  assert (reindexed_state_index <= state_index);
  reindexed_state_index

let reindexed_state_index_opt state_index t =
  match Map.get state_index t with
  | None -> None
  | Some {reindexed_state_index; _} -> begin
      assert (reindexed_state_index <= state_index);
      Some reindexed_state_index
    end

let reindexed_isocore_set_sn state_index t =
  let {reindexed_state_index; reindexed_isocore_set_sn} = Map.get_hlt state_index t in
  assert (reindexed_state_index <= state_index);
  reindexed_isocore_set_sn
