open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap
open Format

let test () =
  printf "@[";
  (* test is n^2 time complexity, so keep n small. *)
  let veq_u128 v0 v1 = Cmp.is_eq (U128.cmp v0 v1) in
  let merge_u128 k v0 v1 = begin
    assert U128.(k = (bit_not v0));
    assert (veq_u128 v0 v1);
    v0
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
        assert (equal veq_u128 ordmap ordmap');
        assert (equal veq_u128 ordmap (union ~f:merge_u128 ordmap ordmap'));
        assert (equal veq_u128 ordmap (inter ~f:merge_u128 ordmap ordmap'));
        assert (equal veq_u128 e (diff ordmap ordmap'));
        ordmap'
      end
  end in
  let e = empty (module U128) in
  let _ = test 100L 0L e e in
  printf "@]"

let _ = test ()
