open Basis
open! Basis.Rudiments

type rel =
  | Unknown
  | Distinct
  | Mergeable

type core_rels = {
  (* Sets of `Mergeable` states, where each bitset index is an isocore set serial number (issn).
   * Mergeability is transitive, so each issn is a member of at most one set. *)
  mergeable_sets: Bitset.t list;
  (* Map from issn to the set of issns corresponding to mergeable states. A `Mergeable` relationship
   * exists between two issns if both map to the same set. Such a relationship can also be
   * definitively tested via a single map lookup and a single set membership test. This map is
   * entirely derived from `mergeable_sets`, and exists only to accelerate queries. *)
  issn2set: (uns, Bitset.t, Uns.cmper_witness) Map.t;
  (* Map of `Distinct` states, where each key is an issn, and each value (set) is the set of issns
   * corresponding to states which are known to be distinct from the key's state. Distinctness is
   * non-transitive, i.e. (distinct(a, b) and distinct(b, c)) does not imply distinct(a, c).
   * Consequently the values (sets) for two distinct states contain both states, but the sets may
   * not be equal. *)
  distinct: (uns, Bitset.t, Uns.cmper_witness) Map.t;
  (* Reverse map from issn to state nub, used by `mergeable_set` and `index_map`. *)
  issn2statenub: (uns, StateNub.t, Uns.cmper_witness) Map.t;
}

(* Map of cores to per core relationship data. *)
type cores_map = (Lr0Itemset.t, core_rels, Lr0Itemset.cmper_witness) Map.t

type t = {
  (* Core-keyed map of `Mergeable`/`Distinct` relationships, with pending relationships integrated
   * via `root`/`expand`. `distinct` rolls `cores_map` back to `snapshot`. *)
  cores_map: cores_map;
  (* Roots of subgraph remergeability exploration, set by `root` and cleared by
   * `distinct`/`mergeable`. *)
  roots: (StateNub.t * StateNub.t) option;
  (* Number of states in each of the two subgraphs currently being considered for remergeability. *)
  subgraph_size: uns;
  (* Snapshot of `cores_map`, updated by `mergeable`. *)
  snapshot: cores_map;
}

let empty =
  {
    cores_map=Map.empty (module Lr0Itemset);
    roots=None;
    subgraph_size=0L;
    snapshot=Map.empty (module Lr0Itemset);
  }

let rel (StateNub.{isocores_sn=isn0; isocore_set_sn=issn0; _} as statenub0)
  StateNub.{isocores_sn=isn1; isocore_set_sn=issn1; _} {cores_map; _} =
  let unknown_vs_distinct issn0 issn1 distinct = begin
    match Map.get issn0 distinct with
    | None -> Unknown
    | Some distinct_set -> begin
        match Bitset.mem issn1 distinct_set with
        | false -> Unknown
        | true -> Distinct
      end
  end in
  assert (isn0 = isn1);
  let core = Lr1Itemset.core StateNub.(statenub0.lr1itemsetclosure).kernel in
  match Map.get core cores_map with
  | None -> Unknown
  | Some {issn2set; distinct; _} -> begin
      match Map.get issn0 issn2set with
      | None -> unknown_vs_distinct issn0 issn1 distinct
      | Some mergeable_set -> begin
          match Bitset.mem issn1 mergeable_set with
          | false -> unknown_vs_distinct issn0 issn1 distinct
          | true -> Mergeable
        end
    end

let mergeable_set (StateNub.{isocore_set_sn=issn; _} as statenub) {cores_map; _} =
  let core = Lr1Itemset.core StateNub.(statenub.lr1itemsetclosure).kernel in
  match Map.get core cores_map with
  | None -> Ordset.empty (module StateNub)
  | Some {issn2set; issn2statenub; _} -> begin
      match Map.get issn issn2set with
      | None -> Ordset.empty (module StateNub)
      | Some mergeable_set -> begin
          Bitset.fold ~init:(Ordset.empty (module StateNub)) ~f:(fun mergeable_with issn ->
            let statenub = Map.get_hlt issn issn2statenub in
            Ordset.insert statenub mergeable_with
          ) mergeable_set
        end
    end

let subgraph_size {subgraph_size; _} =
  subgraph_size

let insert (StateNub.{isocores_sn=isn0; isocore_set_sn=issn0; _} as statenub0)
  (StateNub.{isocores_sn=isn1; isocore_set_sn=issn1; _} as statenub1)
  ({cores_map; subgraph_size; _} as t) =
  assert (isn0 = isn1);
  assert (issn0 <> issn1);
  let mergeable_pair = Bitset.of_array [|issn0; issn1|] in
  let core = Lr1Itemset.core StateNub.(statenub0.lr1itemsetclosure).kernel in
  let cores_map = match Map.get core cores_map with
    | None -> begin
        let core_rels = {
          mergeable_sets=[mergeable_pair];
          issn2set=Map.of_alist (module Uns) [
            (issn0, mergeable_pair);
            (issn1, mergeable_pair);
          ];
          distinct=Map.empty (module Uns);
          issn2statenub=Map.of_alist (module Uns) [
            (issn0, statenub0);
            (issn1, statenub1);
          ];
        } in
        Map.insert_hlt ~k:core ~v:core_rels cores_map
      end
    | Some ({mergeable_sets; issn2statenub; _} as core_rels) -> begin
        (* Create the transitive mergeability closure of sets containing one or both of the
         * mergeable pair, preserving all unrelated sets. *)
        let mergeable_set, unrelated_sets = List.fold ~init:(mergeable_pair, [])
          ~f:(fun (mergeable_set, unrelated_sets) candidate_set ->
            match Bitset.(inter mergeable_pair candidate_set |> is_empty) with
            | true -> mergeable_set, candidate_set :: unrelated_sets
            | false -> Bitset.union candidate_set mergeable_set, unrelated_sets
          ) mergeable_sets in
        let mergeable_sets = mergeable_set :: unrelated_sets in
        (* (Re)build issn2set from scratch. *)
        let issn2set = List.fold ~init:(Map.empty (module Uns))
          ~f:(fun issn2set mergeable_set ->
            Bitset.fold ~init:issn2set ~f:(fun issn2set issn ->
              Map.insert_hlt ~k:issn ~v:mergeable_set issn2set
            ) mergeable_set
          ) mergeable_sets in
        let issn2statenub =
          issn2statenub
          |> Map.insert ~k:issn0 ~v:statenub0
          |> Map.insert ~k:issn1 ~v:statenub1
        in
        let core_rels = {core_rels with mergeable_sets; issn2set; issn2statenub} in
        Map.update_hlt ~k:core ~v:core_rels cores_map
      end
  in
  {t with cores_map; subgraph_size=succ subgraph_size}

let root (StateNub.{isocores_sn=isn0; _} as statenub0) (StateNub.{isocores_sn=isn1; _} as statenub1)
  ({roots; _} as t) =
  assert (isn0 = isn1);
  assert (Option.is_none roots);
  let t = insert statenub0 statenub1 t in
  {t with roots=Some (statenub0, statenub1)}

let expand = insert

let distinct ({roots; snapshot; _} as t) =
  assert (Option.is_some roots);
  let (StateNub.{isocore_set_sn=issn0; _} as statenub0), StateNub.{isocore_set_sn=issn1; _} =
    match roots with
    | None -> not_reached ()
    | Some roots -> roots
  in
  let distinct_pair = Bitset.of_array [|issn0; issn1|] in
  let core = Lr1Itemset.core StateNub.(statenub0.lr1itemsetclosure).kernel in
  let cores_map = match Map.get core snapshot with
    | None -> begin
        let core_rels = {
          mergeable_sets=[];
          issn2set=Map.empty (module Uns);
          distinct=Map.of_alist (module Uns) [
            (issn0, distinct_pair);
            (issn1, distinct_pair);
          ];
          issn2statenub=Map.empty (module Uns);
        } in
        Map.insert_hlt ~k:core ~v:core_rels snapshot
      end
    | Some ({distinct; _} as core_rels) -> begin
        let amend_distinct distinct_set_opt = begin
          match distinct_set_opt with
          | None -> Some distinct_pair
          | Some distinct_set -> Some (Bitset.union distinct_pair distinct_set)
        end in
        let distinct =
          distinct
          |> Map.amend issn0 ~f:amend_distinct
          |> Map.amend issn1 ~f:amend_distinct
        in
        let core_rels = {core_rels with distinct} in
        Map.update_hlt ~k:core ~v:core_rels snapshot
      end
  in
  {t with cores_map; roots=None; subgraph_size=0L}

let mergeable ({cores_map; _} as t) =
  {t with roots=None; subgraph_size=0L; snapshot=cores_map}

(* Return a map of state nub indexes, where keys are to be remerged into values. For each
 * remergeable set, mappings exist for all but the lowest-numbered state nub, and all the mappings
 * are to the lowest-numbered state nub.
 *
 * Example: Given remergeable set {1, 2, 3}, the map contains [(2, 1); [3, 1)]. *)
let index_map {cores_map; _} =
  Map.fold ~init:(Ordmap.empty (module StateNub.Index))
    ~f:(fun index_map (_core, {mergeable_sets; issn2statenub; _}) ->
      List.fold ~init:index_map ~f:(fun index_map mergeable_set ->
        let min_issn =
          Bitset.min_elm ~cmp:(fun issn0 issn1 ->
            let index0 = Map.get_hlt issn0 issn2statenub |> StateNub.index in
            let index1 = Map.get_hlt issn1 issn2statenub |> StateNub.index in
            StateNub.Index.cmp index0 index1
          ) mergeable_set
          |> Option.value_hlt in
        let min_index = Map.get_hlt min_issn issn2statenub |> StateNub.index in
        Bitset.fold ~init:index_map ~f:(fun index_map issn ->
          let index = Map.get_hlt issn issn2statenub |> StateNub.index in
          match StateNub.Index.(index > min_index) with
          | false -> index_map
          | true -> Ordmap.insert_hlt ~k:index ~v:min_index index_map
        ) mergeable_set
      ) mergeable_sets
    ) cores_map
