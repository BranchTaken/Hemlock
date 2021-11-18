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
  List.fold ks ~init:(empty (module UnsTestCmper)) ~f:(fun map k ->
    insert_hlt ~k ~v:(k * 100L) map
  )

let veq v0 v1 =
  Cmp.is_eq (Uns.cmp v0 v1)

let merge k v0 v1 =
  assert Uns.(k * 100L = v0);
  assert (veq v0 v1);
  v0

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
  |> Option.fmt (pp_kv Uns.pp) kv0_opt
  |> Fmt.fmt ", "
  |> Option.fmt (pp_kv Uns.pp) kv1_opt
  |> Fmt.fmt ")"
