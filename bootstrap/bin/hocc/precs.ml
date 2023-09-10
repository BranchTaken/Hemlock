open Basis
open! Basis.Rudiments

type t = {
  names: (string, Prec.Index.t, String.cmper_witness) Map.t;
  precs: (Prec.Index.t, Prec.t, Prec.Index.cmper_witness) Ordmap.t;
}

let empty = {
  names=Map.empty (module String);
  precs=Ordmap.empty (module Prec.Index);
}

let length {precs; _} =
  Ordmap.length precs

let insert ~name ~assoc ~doms ~stmt ({names; precs} as t) =
  let index = length t in
  let prec = Prec.init ~index ~name ~assoc ~doms ~stmt in
  let names' = Map.insert_hlt ~k:name ~v:index names in
  let precs' = Ordmap.insert_hlt ~k:index ~v:prec precs in
  {names=names'; precs=precs'}

let prec_index_of_name name {names; _} =
  Map.get name names

let prec_of_name name ({precs; _} as t) =
  match prec_index_of_name name t with
  | None -> None
  | Some prec_index -> Ordmap.get prec_index precs

let prec_of_prec_index prec_index {precs; _} =
  Ordmap.get_hlt prec_index precs

let fold ~init ~f {precs; _} =
  Ordmap.fold ~init ~f:(fun accum (_, prec) -> f accum prec) precs
