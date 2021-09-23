open! Basis.Rudiments
open! Basis
open Ordmap

let iter_oc base past f =
  let rec fn i past f = begin
    match Uns.(i < past) with
    | false -> ()
    | true -> begin
        f i;
        fn (Uns.succ i) past f
      end
  end in
  fn base past f

let of_klist ks =
  List.fold ks ~init:(empty (module Uns)) ~f:(fun ordmap k ->
    insert_hlt ~k ~v:(k * 100L) ordmap
  )

let of_karray ks =
  Array.fold ks ~init:(empty (module Uns)) ~f:(fun ordmap k ->
    insert_hlt ~k ~v:(k * 100L) ordmap
  )

let veq v0 v1 =
  Cmp.is_eq (Uns.cmp v0 v1)

let merge k v0 v1 =
  assert Uns.(k * 100L = v0);
  assert (veq v0 v1);
  v0
