open Basis
open! Basis.Rudiments

module T = struct
  type t = (Lr0Item.t, Lr0Item.cmper_witness) Ordset.t

  let hash_fold t =
    Ordset.hash_fold t

  let cmp t0 t1 =
    Ordset.cmp t0 t1

  let pp t formatter =
    formatter
    |> Ordset.pp t
end
include T
include Identifiable.Make(T)

let equal = Ordset.equal

let empty = Ordset.empty (module Lr0Item)

let singleton lr0item =
  Ordset.singleton (module Lr0Item) lr0item

let mem = Ordset.mem

let insert = Ordset.insert

let remove = Ordset.remove

let union = Ordset.union
