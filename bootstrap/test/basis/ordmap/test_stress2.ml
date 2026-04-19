open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap

let test () =
  (* test is n^2 time complexity, so keep n small. *)
  let vequal_u128 _k v0 v1 = Cmp.is_eq (U128.cmp v0 v1) in
  let vunion_u128 k v0 v1 = begin
    assert U128.(k = (bit_not v0));
    assert (vequal_u128 k v0 v1);
    v0
  end in
  let vinter_u128 k v0 v1 = begin
    assert U128.(k = (bit_not v0));
    let v = U128.bit_and v0 v1 in
    match U128.(v = zero) with
    | true -> None
    | false -> Some v
  end in
  let vdiff_u128 _k v0 v1 = begin
    let v = U128.(v0 - v1) in
    match U128.(v = zero) with
    | true -> None
    | false -> Some v
  end in
  let rec test n i e ordmap = begin
    match i < n with
    | false -> ordmap
    | true -> begin
        (* Hash i in order to test semi-random insertion order. *)
        let h = Hash.(t_of_state (Uns.hash_fold i State.empty)) in
        let ordmap' = remove_hlt h
            (test n (succ i) e (insert_hlt ~k:h ~v:(U128.bit_not h) ordmap)) in
        validate ordmap';
        assert (equal ~vequal:vequal_u128 ordmap ordmap');
        assert (equal ~vequal:vequal_u128 ordmap (union ~vunion:vunion_u128 ordmap ordmap'));
        assert (equal ~vequal:vequal_u128 ordmap (inter ~vinter:vinter_u128 ordmap ordmap'));
        assert (equal ~vequal:vequal_u128 e (diff ~vdiff:vdiff_u128 ordmap ordmap'));
        ordmap'
      end
  end in
  let e = empty (module U128) in
  let _ = test 100L 0L e e in
  ()

let _ = test ()
