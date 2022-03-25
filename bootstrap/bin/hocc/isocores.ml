open Basis
open! Basis.Rudiments

(* Isocore set. States have identical cores, but distinct kernels. *)
type v = (StateNub.Index.t, StateNub.Index.cmper_witness) Ordset.t

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

let indexes_of_vs vs =
  Ordset.fold ~init:(Ordset.empty (module StateNub.Index)) ~f:(fun indexes index ->
    Ordset.insert index indexes
  ) vs

let mems core {isocores; _} =
  match Map.get core isocores with
  | None -> Ordset.empty (module StateNub.Index)
  | Some vs -> indexes_of_vs vs

let get gotonub {compat; isocores; statenubs_map} =
  let core = GotoNub.core gotonub in
  match Map.get core isocores with
  | None -> None
  | Some vs -> begin
      Ordset.fold_until ~init:None ~f:(fun _ state_index ->
        let statenub = Ordmap.get_hlt state_index statenubs_map in
        match compat gotonub statenub with
        | false -> None, false
        | true -> Some state_index, true
      ) vs
    end

let get_hlt gotonub t =
  Option.value_hlt (get gotonub t)

let get_core_hlt core {isocores; _} =
  let indexes = indexes_of_vs (Map.get_hlt core isocores) in
  assert (Uns.(=) (Ordset.length indexes) 1L);
  Ordset.choose_hlt indexes

let insert symbols gotonub ({isocores; statenubs_map; _} as t) =
  assert (Option.is_none (get gotonub t));
  let core = GotoNub.core gotonub in
  let index = Ordmap.length statenubs_map in
  let statenub = StateNub.init symbols ~index gotonub in
  match Map.get core isocores with
  | None -> begin
      (* Create a new state nub; unique core. *)
      let statenubs_map' = Ordmap.insert_hlt ~k:index ~v:statenub statenubs_map in
      let vs = Ordset.singleton (module StateNub.Index) index in
      let isocores' = Map.insert_hlt ~k:core ~v:vs isocores in
(*
      File.Fmt.stderr |> Fmt.fmt "XXX Isocores.insert " |> Uns.pp index |> Fmt.fmt " (unique)\n" |> ignore;
*)
      index, {t with isocores=isocores'; statenubs_map=statenubs_map'}
    end
  | Some vs -> begin
      (* Create a new LR(1) item set closure; non-unique core. *)
      let statenubs_map' = Ordmap.insert_hlt ~k:index ~v:statenub statenubs_map in
      let vs' = Ordset.insert index vs in
      let isocores' = Map.update_hlt ~k:core ~v:vs' isocores in
(*
      File.Fmt.stderr |> Fmt.fmt "XXX Isocores.insert " |> Uns.pp index |> Fmt.fmt " (non-unique wrt " |> Ordset.pp (indexes_of_vs vs) |> Fmt.fmt ")\n" |> ignore;
*)
      index, {t with isocores=isocores'; statenubs_map=statenubs_map'}
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

let length {statenubs_map; _} =
  Ordmap.length statenubs_map

let statenub index {statenubs_map; _} =
  Ordmap.get_hlt index statenubs_map

let fold ~init ~f {statenubs_map; _} =
  Ordmap.fold ~init ~f:(fun accum (_, statenub) ->
    f accum statenub
  ) statenubs_map
