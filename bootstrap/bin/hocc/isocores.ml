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

let get_isocore_set_hlt core {isocores; _} =
  let {isocore_set; _} = Map.get_hlt core isocores in
  indexes_of_isocore_set isocore_set

let get_core_hlt core t =
  let indexes = get_isocore_set_hlt core t in
  assert (Uns.(=) (Ordset.length indexes) 1L);
  Ordset.choose_hlt indexes

let insert symbols _XXX_prods (GotoNub.{isocores_sn_opt; _} as gotonub) ({isocores; statenubs_map; _} as t) =
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
      File.Fmt.stderr |> Fmt.fmt "XXX Isocores.insert " |> Uns.pp isocores_sn |> Fmt.fmt "." |> Uns.pp isocore_set_sn |> Fmt.fmt "\n" |> ignore;
*)
(*
      let () = match isocores_sn with
        | 234L -> begin
            File.Fmt.stderr |> Fmt.fmt "\nXXX Isocores.insert " |> Uns.pp isocores_sn |> Fmt.fmt "." |> Uns.pp isocore_set_sn |> Fmt.fmt "\n" |> ignore;
            File.Fmt.stderr |> Fmt.fmt "XXX 234 insert\n" |> ignore;
            let gotonub_kernel = gotonub.goto in
            File.Fmt.stderr
            |> Fmt.fmt "gotonub kernel=" |> Lr1Itemset.fmt_hr symbols ~alt:true gotonub_kernel
            |> Fmt.fmt "\n" |> ignore;
          end
        | _ -> ()
      in
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
      File.Fmt.stderr |> Fmt.fmt "XXX Isocores.insert " |> Uns.pp isocores_sn |> Fmt.fmt "." |> Uns.pp isocore_set_sn |> Fmt.fmt "\n" |> ignore;
*)
(*
      let () = match isocores_sn with
        | 234L -> begin
            File.Fmt.stderr |> Fmt.fmt "\nXXX Isocores.insert " |> Uns.pp isocores_sn |> Fmt.fmt "." |> Uns.pp isocore_set_sn |> Fmt.fmt "\n" |> ignore;
            File.Fmt.stderr |> Fmt.fmt "XXX 234 insert\n" |> ignore;
            let gotonub_kernel = gotonub.goto in
            File.Fmt.stderr
            |> Fmt.fmt "gotonub kernel=" |> Lr1Itemset.fmt_hr symbols ~alt:true gotonub_kernel
            |> Fmt.fmt "\n" |> ignore;
            Ordset.fold ~init:() ~f:(fun _ incompatible_statenub_index ->
              let incompatible_statenub = Ordmap.get_hlt incompatible_statenub_index statenubs_map in
              File.Fmt.stderr
              |> Fmt.fmt "incompatible_statenub_index=" |> StateIndex.pp incompatible_statenub_index
              |> Fmt.fmt "\n" |> ignore;
              StateNub.explain_ielr1 ~resolve:true(*XXX*) symbols prods gotonub incompatible_statenub
            ) isocore_set
          end
        | _ -> ()
      in
*)
      statenub_index, {t with isocores=isocores'; statenubs_map=statenubs_map'}
    end

let merge symbols _XXX_prods gotonub merge_index ({statenubs_map; _} as t) =
  (* Merge into existing LR(1) item set closure. *)
  let merge_statenub = Ordmap.get_hlt merge_index statenubs_map in
  let merged, merge_statenub' = StateNub.merge symbols gotonub merge_statenub in
  match merged with
  | false -> begin
(*
      let StateNub.{isocores_sn; isocore_set_sn; _} = merge_statenub' in
      File.Fmt.stderr |> Fmt.fmt "XXX Isocores.merge " |> Uns.pp isocores_sn |> Fmt.fmt "." |> Uns.pp isocore_set_sn |> Fmt.fmt " (non-modifying)\n" |> ignore;
*)
      false, t
    end
  | true -> begin
(*
      let StateNub.{isocores_sn; isocore_set_sn; _} = merge_statenub' in
*)
(*
      File.Fmt.stderr |> Fmt.fmt "XXX Isocores.merge " |> Uns.pp isocores_sn |> Fmt.fmt "." |> Uns.pp isocore_set_sn |> Fmt.fmt " (modifying)\n" |> ignore;
*)
(*
      let () = match isocores_sn with
        | 234L -> begin
            File.Fmt.stderr |> Fmt.fmt "\nXXX Isocores.merge " |> Uns.pp isocores_sn |> Fmt.fmt "." |> Uns.pp isocore_set_sn |> Fmt.fmt " (modifying)\n" |> ignore;
            File.Fmt.stderr |> Fmt.fmt "XXX 234 merge\n" |> ignore;
            let statenub_kernel = merge_statenub.lr1itemsetclosure.kernel in
            File.Fmt.stderr
            |> Fmt.fmt "unmerged kernel=" |> Lr1Itemset.fmt_hr symbols ~alt:true statenub_kernel
            |> Fmt.fmt "\n" |> ignore;

            let gotonub_kernel = gotonub.goto in
            File.Fmt.stderr
            |> Fmt.fmt "gotonub kernel=" |> Lr1Itemset.fmt_hr symbols ~alt:true gotonub_kernel
            |> Fmt.fmt "\n" |> ignore;
            let () = StateNub.explain_ielr1 ~resolve:true(*XXX*) symbols prods gotonub merge_statenub in

            let merged_kernel = merge_statenub'.lr1itemsetclosure.kernel in
            File.Fmt.stderr |> Fmt.fmt "merged kernel=" |> Lr1Itemset.fmt_hr symbols ~alt:true merged_kernel
            |> Fmt.fmt "\n" |> ignore;
          end
        | _ -> ()
      in
*)
      let statenubs_map' = Ordmap.update_hlt ~k:merge_index ~v:merge_statenub' statenubs_map in
      true, {t with statenubs_map=statenubs_map'}
    end

let reindex index_map ({statenubs_map; _} as t) =
  let isocores', statenubs_map' =
    Map.fold ~init:(Map.empty (module Lr0Itemset), Ordmap.empty (module StateNub.Index))
      ~f:(fun (isocores', statenubs_map') (index, index') ->
        let statenub = Ordmap.get_hlt index statenubs_map in
        let statenub' = StateNub.reindex index_map statenub in
        let core = Lr1Itemset.core StateNub.(statenub'.lr1itemsetclosure).kernel in
        let isocores' = Map.amend core ~f:(fun v_opt ->
          match v_opt with
          | None -> Some {
              isocore_set=Ordset.singleton (module StateNub.Index) index';
              isocores_sn=statenub'.isocores_sn
            }
          | Some ({isocore_set; _} as v) -> Some {v with
              isocore_set=Ordset.insert index' isocore_set
            }
          ) isocores' in
        let statenubs_map' = Ordmap.insert_hlt ~k:index' ~v:statenub' statenubs_map' in
        isocores', statenubs_map'
      ) index_map
  in
  {t with isocores=isocores'; statenubs_map=statenubs_map'}

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

let fold_isocore_sets ~init ~f {isocores; _} =
  Map.fold ~init ~f:(fun accum (_k, {isocore_set; _}) ->
    f accum isocore_set
  ) isocores
