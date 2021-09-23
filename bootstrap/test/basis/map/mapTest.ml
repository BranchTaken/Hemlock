open! Basis.Rudiments
open! Basis
open Map

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
