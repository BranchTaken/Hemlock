open Basis
open! Basis.Rudiments

type t = (Callback.Index.t, Callback.t, Callback.Index.cmper_witness) Ordmap.t

let empty = Ordmap.empty (module Callback.Index)

let length = Ordmap.length

let insert ~lhs:Symbols.{name; stype; _} ~rhs ~code t =
  let index = length t in
  let callback = Callback.init ~index ~lhs_name:name ~lhs_stype:stype ~rhs ~code in
  callback, Ordmap.insert_hlt ~k:index ~v:callback t

let fold ~init ~f t =
  Ordmap.fold ~init ~f:(fun accum (_, callback) -> f accum callback) t
