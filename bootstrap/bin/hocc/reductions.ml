open Basis
open! Basis.Rudiments

type t = (Reduction.Index.t, Reduction.t, Reduction.Index.cmper_witness) Ordmap.t

let empty = Ordmap.empty (module Reduction.Index)

let length = Ordmap.length

let insert ~lhs ~rhs ~code t =
  let index = length t in
  let reduction = Reduction.init ~index ~lhs ~rhs ~code in
  reduction, Ordmap.insert_hlt ~k:index ~v:reduction t

let fold ~init ~f t =
  Ordmap.fold ~init ~f:(fun accum (_, reduction) -> f accum reduction) t
