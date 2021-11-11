open! Basis.Rudiments
open! Basis
open Ordmap

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

let pp_kv_pair pp_v (k, v) formatter =
  formatter
  |> Fmt.fmt "("
  |> Uns.pp k
  |> Fmt.fmt ", "
  |> pp_v v
  |> Fmt.fmt ")"

let pp_kv_opt_pair (kv0_opt, kv1_opt) formatter =
  formatter
  |> Fmt.fmt "("
  |> Option.fmt (pp_kv_pair Uns.pp) kv0_opt
  |> Fmt.fmt ", "
  |> Option.fmt (pp_kv_pair Uns.pp) kv1_opt
  |> Fmt.fmt ")"
