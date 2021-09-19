open! Basis.Rudiments
open! Basis
open Ordmap

let of_klist ks =
  List.fold ks ~init:(empty (module Uns)) ~f:(fun ordmap k ->
    insert_hlt ~k ~v:(k * 100) ordmap
  )

let of_karray ks =
  Array.fold ks ~init:(empty (module Uns)) ~f:(fun ordmap k ->
    insert_hlt ~k ~v:(k * 100) ordmap
  )

let veq v0 v1 =
  Cmp.is_eq (Uns.cmp v0 v1)

let merge k v0 v1 =
  assert Uns.(k * 100 = v0);
  assert (veq v0 v1);
  v0
