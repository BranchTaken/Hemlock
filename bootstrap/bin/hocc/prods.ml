open! Basis
open! Basis.Rudiments

type t = (Prod.Index.t, Prod.t, Prod.Index.cmper_witness) Ordmap.t

let empty = Ordmap.empty (module Prod.Index)

let length = Ordmap.length

let insert ~lhs_index ~rhs_indexes ~prec ~stmt ~callback t =
  let index = length t in
  let prod = Prod.init ~index ~lhs_index ~rhs_indexes ~prec ~stmt ~callback in
  prod, Ordmap.insert_hlt ~k:index ~v:prod t

let prod_of_prod_index = Ordmap.get_hlt

let fold ~init ~f t =
  Ordmap.fold ~init ~f:(fun accum (_, prod) -> f accum prod) t
