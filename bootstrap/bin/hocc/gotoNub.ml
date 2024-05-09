open Basis
open! Basis.Rudiments

module T = struct
  type t = {
    goto: Lr1Itemset.t;
    transit_contribs: TransitContribs.t;
    attribs: Attribs.t;
  }

  let hash_fold {goto; _} state =
    state |> Lr1Itemset.hash_fold goto

  let cmp {goto=g0; _} {goto=g1; _} =
    Lr1Itemset.cmp g0 g1

  let pp {goto; transit_contribs; attribs} formatter =
    formatter
    |> Fmt.fmt "{goto=" |> Lr1Itemset.pp goto
    |> Fmt.fmt "; transit_contribs=" |> TransitContribs.pp transit_contribs
    |> Fmt.fmt "; attribs=" |> Attribs.pp attribs
    |> Fmt.fmt "}"
end
include T
include Identifiable.Make(T)

let init ~goto ~transit_contribs =
  {goto; transit_contribs; attribs=TransitContribs.attribs goto transit_contribs}

let core {goto; _} =
  Lr1Itemset.core goto
