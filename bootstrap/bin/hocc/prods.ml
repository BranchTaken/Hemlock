open! Basis
open! Basis.Rudiments

type t = Prod.t array

module Builder = struct
  type outer = t
  type t = (Prod.Index.t, Prod.t, Prod.Index.cmper_witness) Ordmap.t

  let empty = Ordmap.empty (module Prod.Index)

  let insert ~lhs_index ~rhs_indexes ~prec ~stmt ~callback t =
    let index = Ordmap.length t in
    let prod = Prod.init ~index ~lhs_index ~rhs_indexes ~prec ~stmt ~callback in
    prod, Ordmap.insert_hlt ~k:index ~v:prod t

  let build t =
    Array.init (0L =:< Ordmap.length t) ~f:(fun index -> Ordmap.get_hlt index t)
end

let length = Array.length

let prod_of_prod_index = Array.get

let fold = Array.fold
