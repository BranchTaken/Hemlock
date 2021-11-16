open! Basis.Rudiments
open! Basis
open! SetTest
open Set

let test () =
  (* test is n^2 time complexity, so keep n small. *)
  let rec test n i set = begin
    match i < n with
    | false -> set
    | true -> begin
        let set' = test n (succ i) (insert i set) in
        let m = choose_hlt set' in
        let set'' = remove m set' in
        assert ((length set') = (length set'') + 1L);
        set''
      end
  end in
  let e = empty (module Uns) in
  let _ = test 100L 0L e in
  ()

let _ = test ()
