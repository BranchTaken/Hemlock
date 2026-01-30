open! Basis.Rudiments
open! Basis
open Bitset

let test () =
  (* test is n^2 time complexity, so keep n small. *)
  let rec test n i bitset = begin
    match i < n with
    | false -> bitset
    | true -> begin
        let bitset' = test n (succ i) (insert i bitset) in
        let m = choose_hlt bitset' in
        let bitset'' = remove m bitset' in
        assert ((length bitset') = (length bitset'') + 1L);
        bitset''
      end
  end in
  let e = empty in
  let _ = test 100L 0L e in
  ()

let _ = test ()
