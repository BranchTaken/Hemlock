open Basis
open! Basis.Rudiments

(* Logical set of remergeable state nub sets. Remergeability is associative, so each state nub is a
 * member of at most one remergeable set. *)
type v = ((StateNub.t, StateNub.cmper_witness) Ordset.t) List.t

type t = {
  (* Core-keyed map of remergeable state nub sets. *)
  remergeable_map: (Lr0Itemset.t, v, Lr0Itemset.cmper_witness) Map.t;
  (* Map of state nub indexes, where keys are to be remerged into values. For each remergeable set,
   * mappings exist for all but the lowest-numbered state nub, and all the mappings are to the
   * lowest-numbered state nub.
   *
   * Example: Given remergeable set {1, 2, 3}, the map contains [(2, 1); [3, 1)]. *)
  index_map: (StateNub.Index.t, StateNub.Index.t, StateNub.Index.cmper_witness) Ordmap.t;
}

let empty =
  {
    remergeable_map=Map.empty (module Lr0Itemset);
    index_map=Ordmap.empty (module StateNub.Index);
  }

let mem statenub {remergeable_map; _} =
  let core = Lr1Itemset.core StateNub.(statenub.lr1itemsetclosure).kernel in
  match Map.get core remergeable_map with
  | None -> false
  | Some v -> begin
      List.fold_until ~init:false ~f:(fun _mem remergeable_set ->
        let mem = Ordset.mem statenub remergeable_set in
        mem, mem
      ) v
    end

let insert statenub0 statenub1 ({remergeable_map; index_map} as t) =
  assert (not ((mem statenub0 t) && mem statenub1 t));
  let core = Lr1Itemset.core StateNub.(statenub0.lr1itemsetclosure).kernel in
  let remergeable_map, remergeable_set = match Map.get core remergeable_map with
    | None -> begin
        let remergeable_set = Ordset.of_list (module StateNub) [statenub0; statenub1] in
        let remergeable_map = Map.insert_hlt ~k:core ~v:[remergeable_set] remergeable_map in
        remergeable_map, remergeable_set
      end
    | Some v -> begin
        let remergeable_set_opt, v' = List.fold ~init:(None, [])
          ~f:(fun (remergeable_set_opt, v') remergeable_set ->
            match remergeable_set_opt with
            | Some _ -> remergeable_set_opt, remergeable_set :: v'
            | None -> begin
                match Ordset.mem statenub0 remergeable_set, Ordset.mem statenub1 remergeable_set
                with
                | false, false -> None, remergeable_set :: v'
                | false, true -> begin
                    let remergeable_set = Ordset.insert statenub0 remergeable_set in
                    Some remergeable_set, remergeable_set :: v'
                  end
                | true, false -> begin
                    let remergeable_set = Ordset.insert statenub1 remergeable_set in
                    Some remergeable_set, remergeable_set :: v'
                  end
                | true, true -> not_reached ()
              end
          ) v in
        let remergeable_set, v' = match remergeable_set_opt with
          | Some remergeable_set -> remergeable_set, v'
          | None -> begin
              let remergeable_set = Ordset.of_list (module StateNub) [statenub0; statenub1] in
              remergeable_set, remergeable_set :: v'
            end
        in
        Map.update_hlt ~k:core ~v:v' remergeable_map, remergeable_set
      end
  in
  let min_index =
    Ordset.min_elm ~cmp:StateNub.cmp remergeable_set
    |> Option.value_hlt
    |> StateNub.index in
  let index_map =
    index_map
    |> Ordmap.remove min_index
    |> fun index_map -> Ordset.fold ~init:index_map ~f:(fun index_map statenub ->
      let statenub_index = StateNub.index statenub in
      match StateNub.Index.(statenub_index = min_index) with
      | true -> index_map
      | false -> Ordmap.upsert ~k:statenub_index ~v:min_index index_map
    ) remergeable_set
  in
  {remergeable_map; index_map}

let index_map {index_map; _} =
  index_map
