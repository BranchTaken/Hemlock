open! Basis.Rudiments
open! Basis
open Ordmap

let of_klist ks =
  List.fold ks ~init:(empty (module Uns)) ~f:(fun ordmap k ->
    amend k ~f:(fun bitset_opt ->
      match bitset_opt with
      | None -> Some (Bitset.singleton k)
      | Some bitset -> Some (Bitset.insert k bitset)
    ) ordmap
  )

let of_klist_v ks v =
  List.fold ks ~init:(empty (module Uns)) ~f:(fun ordmap k ->
    amend k ~f:(fun bitset_opt ->
      match bitset_opt with
      | None -> Some (Bitset.singleton v)
      | Some bitset -> Some (Bitset.insert v bitset)
    ) ordmap
  )

let of_karray ks =
  Array.fold ks ~init:(empty (module Uns)) ~f:(fun ordmap k ->
    amend k ~f:(fun v_opt ->
      match v_opt with
      | None -> Some (Bitset.singleton k)
      | Some v_existing -> Some (Bitset.insert k v_existing)
    ) ordmap
  )

let vsum v =
  Bitset.fold ~init:0L ~f:(fun sum elm ->
    sum + elm
  ) v

let vequal _k v0 v1 =
  Bitset.equal v0 v1

let vsubset _k v0 v1 =
  Bitset.subset v0 v1

let vdisjoint _k v0 v1 =
  Bitset.disjoint v0 v1

let vunion _k v0 v1 =
  Bitset.union v0 v1

let vinter _k v0 v1 =
  let v = Bitset.inter v0 v1 in
  match Bitset.is_empty v with
  | true -> None
  | false -> Some v

let vdiff _k v0 v1 =
  let v = Bitset.diff v0 v1 in
  match Bitset.is_empty v with
  | true -> None
  | false -> Some v

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
  |> Option.fmt (pp_kv_pair Bitset.pp) kv0_opt
  |> Fmt.fmt ", "
  |> Option.fmt (pp_kv_pair Bitset.pp) kv1_opt
  |> Fmt.fmt ")"
