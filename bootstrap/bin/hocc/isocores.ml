open Basis
open! Basis.Rudiments

type v = {
  (* Isocore set, stored as a map for efficient lookup of serial numbers. States have identical
   * cores, but distinct kernels. *)
  isocore_set: (StateNub.Index.t, StateNub.Index.cmper_witness) Ordset.t;
  (* Isocore set sequence number. *)
  isocores_sn: uns;
}

type t = {
  compat: GotoNub.t -> StateNub.t -> bool;
  isocores: (Lr0Itemset.t, v, Lr0Itemset.cmper_witness) Map.t;
  statenubs_map: (StateNub.Index.t, StateNub.t, StateNub.Index.cmper_witness) Ordmap.t;
}

let init ~compat =
  {
    compat;
    isocores=Map.empty (module Lr0Itemset);
    statenubs_map=Ordmap.empty (module StateNub.Index);
  }

let mem core {isocores; _} =
  match Map.get core isocores with
  | None -> false
  | Some _ -> true

let indexes_of_isocore_set isocore_set =
  Ordset.fold ~init:(Ordset.empty (module StateNub.Index))
    ~f:(fun indexes statenub_index ->
      Ordset.insert statenub_index indexes
    ) isocore_set

let mems core {isocores; _} =
  match Map.get core isocores with
  | None -> Ordset.empty (module StateNub.Index)
  | Some {isocore_set; _} -> indexes_of_isocore_set isocore_set

let get gotonub {compat; isocores; statenubs_map} =
  let core = GotoNub.core gotonub in
  match Map.get core isocores with
  | None -> None
  | Some {isocore_set; _} -> begin
      Ordset.fold_until ~init:None ~f:(fun _ statenub_index ->
        let statenub = Ordmap.get_hlt statenub_index statenubs_map in
        match compat gotonub statenub with
        | false -> None, false
        | true -> Some statenub_index, true
      ) isocore_set
    end

let get_hlt gotonub t =
  Option.value_hlt (get gotonub t)

let get_core_hlt core {isocores; _} =
  let {isocore_set; _} = Map.get_hlt core isocores in
  let indexes = indexes_of_isocore_set isocore_set in
  assert (Uns.(=) (Ordset.length indexes) 1L);
  Ordset.choose_hlt indexes

let insert symbols (GotoNub.{isocores_sn_opt; _} as gotonub) ({isocores; statenubs_map; _} as t) =
  assert (Option.is_none (get gotonub t));
  let core = GotoNub.core gotonub in
  let statenub_index = Ordmap.length statenubs_map in
  match Map.get core isocores with
  | None -> begin
      (* Create a new state nub; unique core. *)
      let isocore_set_sn = 0L in
      let isocores_sn = match isocores_sn_opt with
        | None -> Map.length isocores
        | Some isocores_sn -> isocores_sn
      in
      let statenub =
        StateNub.init symbols ~index:statenub_index ~isocores_sn ~isocore_set_sn gotonub in
      let statenubs_map' = Ordmap.insert_hlt ~k:statenub_index ~v:statenub statenubs_map in
      let v = { isocore_set=Ordset.singleton (module StateNub.Index) statenub_index; isocores_sn} in
      let isocores' = Map.insert_hlt ~k:core ~v isocores in
(*
      File.Fmt.stderr |> Fmt.fmt "XXX Isocores.insert " |> Uns.pp index |> Fmt.fmt " (unique)\n" |> ignore;
*)
      statenub_index, {t with isocores=isocores'; statenubs_map=statenubs_map'}
    end
  | Some ({isocore_set; isocores_sn=isocores_sn_existing} as v) -> begin
      (* Create a new LR(1) item set closure; non-unique core. *)
      let isocores_sn = match isocores_sn_opt with
        | None -> isocores_sn_existing
        | Some isocores_sn -> begin
            assert Uns.(isocores_sn = isocores_sn_existing);
            isocores_sn
          end
      in
      let isocore_set_sn = Ordset.length isocore_set in
      let statenub =
        StateNub.init symbols ~index:statenub_index ~isocores_sn ~isocore_set_sn gotonub in
      let statenubs_map' = Ordmap.insert_hlt ~k:statenub_index ~v:statenub statenubs_map in
      let v' = {v with isocore_set=Ordset.insert statenub_index isocore_set} in
      let isocores' = Map.update_hlt ~k:core ~v:v' isocores in
(*
      File.Fmt.stderr |> Fmt.fmt "XXX Isocores.insert " |> Uns.pp index |> Fmt.fmt " (non-unique wrt " |> Ordset.pp (indexes_of_vs vs) |> Fmt.fmt ")\n" |> ignore;
*)
      statenub_index, {t with isocores=isocores'; statenubs_map=statenubs_map'}
    end

let merge symbols gotonub merge_index ({statenubs_map; _} as t) =
  (* Merge into existing LR(1) item set closure. *)
  let merge_statenub = Ordmap.get_hlt merge_index statenubs_map in
  let merged, merge_statenub' = StateNub.merge symbols gotonub merge_statenub in
  match merged with
  | false -> begin
(*
      File.Fmt.stderr |> Fmt.fmt "XXX Isocores.merge " |> Uns.pp (StateNub.index merge_statenub) |> Fmt.fmt " (non-modifying)\n" |> ignore;
*)
      false, t
    end
  | true -> begin
      let statenubs_map' = Ordmap.update_hlt ~k:merge_index ~v:merge_statenub' statenubs_map in
(*
      File.Fmt.stderr |> Fmt.fmt "XXX Isocores.merge " |> Uns.pp (StateNub.index merge_statenub) |> Fmt.fmt " (modifying)\n" |> ignore;
*)
      true, {t with statenubs_map=statenubs_map'}
    end

let isocores_length {isocores; _} =
  Map.length isocores

let length {statenubs_map; _} =
  Ordmap.length statenubs_map

let statenub index {statenubs_map; _} =
  Ordmap.get_hlt index statenubs_map

let fold ~init ~f {statenubs_map; _} =
  Ordmap.fold ~init ~f:(fun accum (_, statenub) ->
    f accum statenub
  ) statenubs_map
