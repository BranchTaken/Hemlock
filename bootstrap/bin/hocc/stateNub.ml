open Basis
open! Basis.Rudiments

module T = struct
  module Index = Lr1ItemsetClosure.Index
  type t = {
    lr1itemsetclosure: Lr1ItemsetClosure.t;
    isocores_sn: uns;
    isocore_set_sn: uns;
    kernel_attribs: KernelAttribs.t;
    attribs: Attribs.t;
  }

  let hash_fold {lr1itemsetclosure; _} state =
    state |> Lr1ItemsetClosure.hash_fold lr1itemsetclosure

  let cmp {lr1itemsetclosure=c0; _} {lr1itemsetclosure=c1; _} =
    Lr1ItemsetClosure.cmp c0 c1

  let pp {lr1itemsetclosure; isocores_sn; isocore_set_sn; kernel_attribs; attribs} formatter =
    formatter
    |> Fmt.fmt "{lr1itemsetclosure=" |> Lr1ItemsetClosure.pp  lr1itemsetclosure
    |> Fmt.fmt "; isocores_sn=" |> Uns.pp isocores_sn
    |> Fmt.fmt "; isocore_set_sn=" |> Uns.pp isocore_set_sn
    |> Fmt.fmt "; kernel_attribs=" |> KernelAttribs.pp kernel_attribs
    |> Fmt.fmt "; attribs=" |> Attribs.pp attribs
    |> Fmt.fmt "}"
end
include T
include Identifiable.Make(T)

let init symbols ~index ~isocores_sn ~isocore_set_sn GotoNub.{goto; kernel_attribs; attribs; _} =
  let lr1itemsetclosure = Lr1ItemsetClosure.init symbols ~index goto in
  {lr1itemsetclosure; isocores_sn; isocore_set_sn; kernel_attribs; attribs}

let remerge symbols remergeable_index_map
    {lr1itemsetclosure=c0; isocores_sn=is0; isocore_set_sn=iss0; kernel_attribs=ka0; attribs=a0}
    {lr1itemsetclosure=c1; isocores_sn=is1; isocore_set_sn=iss1; kernel_attribs=ka1; attribs=a1} =
  assert Uns.(is0 = is1);
  {
    lr1itemsetclosure=Lr1ItemsetClosure.remerge symbols remergeable_index_map c0 c1;
    isocores_sn=is0;
    isocore_set_sn=Uns.min iss0 iss1;
    kernel_attribs=KernelAttribs.(union (remerge1 remergeable_index_map ka0)
      (remerge1 remergeable_index_map ka1));
    attribs=Attribs.remerge remergeable_index_map a0 a1
  }

let reindex state_index_map
    ({lr1itemsetclosure={index; _} as lr1itemsetclosure; isocores_sn; isocore_set_sn=_;
        kernel_attribs=_; attribs=_} as t) =
  let lr1itemsetclosure = Lr1ItemsetClosure.reindex state_index_map lr1itemsetclosure in
  let isocore_set_sn = StateIndexMap.reindexed_isocore_set_sn index state_index_map in
  {t with lr1itemsetclosure; isocores_sn; isocore_set_sn}

let index {lr1itemsetclosure; _} =
  lr1itemsetclosure.index

let isocores_sn {isocores_sn; _} =
  isocores_sn

let isocore_set_sn {isocore_set_sn; _} =
  isocore_set_sn

let merge symbols GotoNub.{goto; kernel_attribs=gotonub_ka; _}
  {lr1itemsetclosure; isocores_sn; isocore_set_sn; kernel_attribs=statenub_ka; attribs} =
  let merged, (Lr1ItemsetClosure.{kernel=lr1itemset; _} as lr1itemsetclosure) =
    Lr1ItemsetClosure.merge symbols goto lr1itemsetclosure in
  let kernel_attribs = KernelAttribs.union gotonub_ka statenub_ka in
  let attribs = match merged with
    | false -> attribs (* No-op merge means no change in attribs. *)
    | true -> Attribs.union (KernelAttribs.attribs lr1itemset kernel_attribs) attribs
  in
  merged, {lr1itemsetclosure; isocores_sn; isocore_set_sn; kernel_attribs; attribs}

let filtered_kernel_attribs {lr1itemsetclosure=Lr1ItemsetClosure.{kernel; _}; kernel_attribs; _} =
  KernelAttribs.fold ~init:KernelAttribs.empty
    ~f:(fun kernel_attribs (_src_lr1item, src_lr1item_attribs) ->
      Attribs.fold ~init:kernel_attribs
        ~f:(fun kernel_attribs (Attrib.{symbol_index; isucc_lr1itemset; _} as attrib) ->
          Lr1Itemset.fold ~init:kernel_attribs ~f:(fun kernel_attribs isucc_lr1item ->
            match Lr1Itemset.get isucc_lr1item kernel with
            | None -> kernel_attribs
            | Some {follow; _} -> begin
                match Bitset.mem symbol_index follow with
                | false -> kernel_attribs
                | true ->
                  KernelAttribs.insert isucc_lr1item (Attribs.singleton attrib) kernel_attribs
              end
          ) isucc_lr1itemset
        ) src_lr1item_attribs
    ) kernel_attribs

let compat_ielr ~resolve symbols prods GotoNub.{attribs=o_attribs; _} {attribs=t_attribs; _} =
  Attribs.fold2_until ~init:true
    ~f:(fun _compat attrib_opt0 attrib_opt1 ->
      let o_attrib, t_attrib = match attrib_opt0, attrib_opt1 with
        | Some o_attrib, Some t_attrib -> o_attrib, t_attrib
        | Some (Attrib.{conflict_state_index; symbol_index; conflict; _} as o_attrib), None ->
          o_attrib, Attrib.empty ~conflict_state_index ~symbol_index ~conflict
        | None, Some (Attrib.{conflict_state_index; symbol_index; conflict; _} as t_attrib) ->
          Attrib.empty ~conflict_state_index ~symbol_index ~conflict, t_attrib
        | None, None -> not_reached ()
      in
      let compat = Attrib.compat_ielr ~resolve symbols prods o_attrib t_attrib in
      compat, not compat
    ) o_attribs t_attribs

let compat_lr GotoNub.{goto; _} {lr1itemsetclosure={kernel; _}; _} =
  Lr1Itemset.compat_lr goto kernel

let compat_pgm GotoNub.{goto; _} {lr1itemsetclosure={kernel; _}; _} =
  Lr1Itemset.compat_pgm goto kernel

let compat_lalr GotoNub.{goto; _} {lr1itemsetclosure={kernel; _}; _} =
  Lr1Itemset.compat_lalr goto kernel
