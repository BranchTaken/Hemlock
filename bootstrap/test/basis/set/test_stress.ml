open! Basis.Rudiments
open! Basis
open Set

let test () =
  (* test is n^2 time complexity, so keep n small. *)
  let rec test n i e set = begin
    match i < n with
    | false -> set
    | true -> begin
        let set' = remove i (test n (succ i) e (insert i set)) in
        assert (equal set set');
        assert (equal set (union set set'));
        assert (equal set (inter set set'));
        assert (equal e (diff set set'));
        set'
      end
  end in
  let e = empty (module Uns) in
  let _ = test 100L 0L e e in
  ()

let _ = test ()
