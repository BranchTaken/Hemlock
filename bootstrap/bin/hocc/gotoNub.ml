open Basis
open! Basis.Rudiments

module T = struct
  type t = {
    goto: Lr1Itemset.t;
    isocores_sn_opt: uns option;
    transit_attribs: TransitAttribs.t;
    attribs: Attribs.t;
  }

  let hash_fold {goto; _} state =
    state |> Lr1Itemset.hash_fold goto

  let cmp {goto=g0; _} {goto=g1; _} =
    Lr1Itemset.cmp g0 g1

  let pp {goto; isocores_sn_opt; transit_attribs; attribs} formatter =
    formatter
    |> Fmt.fmt "{goto=" |> Lr1Itemset.pp goto
    |> Fmt.fmt "; isocores_sn_opt=" |> Option.pp Uns.pp isocores_sn_opt
    |> Fmt.fmt "; transit_attribs=" |> TransitAttribs.pp transit_attribs
    |> Fmt.fmt "; attribs=" |> Attribs.pp attribs
    |> Fmt.fmt "}"
end
include T
include Identifiable.Make(T)

let init ~isocores_sn_opt ~goto ~transit_attribs =
  {goto; isocores_sn_opt; transit_attribs; attribs=TransitAttribs.attribs goto transit_attribs}

let core {goto; _} =
  Lr1Itemset.core goto
