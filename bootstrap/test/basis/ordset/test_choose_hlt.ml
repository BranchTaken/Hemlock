open! Basis.Rudiments
open! Basis
open Ordset

let test () =
  (* test is n^2 time complexity, so keep n small. *)
  let rec test n i ordset = begin
    match i < n with
    | false -> ordset
    | true -> begin
        let ordset' = test n (succ i) (insert i ordset) in
        let m = choose_hlt ordset' in
        let ordset'' = remove m ordset' in
        assert ((length ordset') = (length ordset'') + 1L);
        ordset''
      end
  end in
  let e = empty (module Uns) in
  let _ = test 100L 0L e in
  ()

let _ = test ()
