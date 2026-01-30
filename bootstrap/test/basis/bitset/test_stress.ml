open! Basis.Rudiments
open! Basis
open Bitset

let test () =
  (* test is n^2 time complexity, so keep n small. *)
  let rec test n i e bitset = begin
    match i < n with
    | false -> bitset
    | true -> begin
        let bitset' = remove i (test n (succ i) e (insert i bitset)) in
        assert (equal bitset bitset');
        assert (equal bitset (union bitset bitset'));
        assert (equal bitset (inter bitset bitset'));
        assert (equal e (diff bitset bitset'));
        bitset'
      end
  end in
  let e = empty in
  let _ = test 100L 0L e e in
  ()

let _ = test ()
