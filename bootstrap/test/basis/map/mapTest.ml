open! Basis.Rudiments
open! Basis
open Map

(* Test comparator module for uns that uses unseeded hashing, with several specially handled values
 * for the purpose of collision testing. This allows deterministic hashing across test runs. *)
module UnsTestCmper = struct
  type t = uns
  module T = struct
    type nonrec t = t
    let hash_fold a _state =
      match a with
      | 42L | 420L | 4200L -> Hash.State.of_u128 U128.zero
      | 421L ->
        (* Set the least significant consumed hash bit. *)
        Hash.State.of_u128 (U128.bit_sl ~shift:(bits_per_hash % bits_per_level) U128.one)
      | _ -> Uns.hash_fold a Hash.State.empty
    let cmp = Uns.cmp
    let pp = Uns.pp
  end
  include Cmper.MakeMono(T)
end

let of_klist ks =
  List.fold ks ~init:(empty (module UnsTestCmper)) ~f:(fun ordmap k ->
    amend k ~f:(fun bitset_opt ->
      match bitset_opt with
      | None -> Some (Bitset.singleton k)
      | Some bitset -> Some (Bitset.insert k bitset)
    ) ordmap
  )

let of_klist_v ks v =
  List.fold ks ~init:(empty (module UnsTestCmper)) ~f:(fun ordmap k ->
    amend k ~f:(fun bitset_opt ->
      match bitset_opt with
      | None -> Some (Bitset.singleton v)
      | Some bitset -> Some (Bitset.insert v bitset)
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

let pp_kv pp_v (k, v) formatter =
  formatter
  |> Fmt.fmt "("
  |> Uns.pp k
  |> Fmt.fmt ", "
  |> pp_v v
  |> Fmt.fmt ")"

let pp_kv_opt_pair (kv0_opt, kv1_opt) formatter =
  formatter
  |> Fmt.fmt "("
  |> Option.fmt (pp_kv Bitset.pp) kv0_opt
  |> Fmt.fmt ", "
  |> Option.fmt (pp_kv Bitset.pp) kv1_opt
  |> Fmt.fmt ")"
