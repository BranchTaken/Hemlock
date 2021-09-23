open! Basis.Rudiments
open! Basis
open! OrdsetTest
open Ordset
open Format

let test () =
  printf "@[";
  (* test is n^2 time complexity, so keep n small. *)
  let rec test n i e ordset = begin
    match i < n with
    | false -> ordset
    | true -> begin
        let ordset' = remove i (test n (succ i) e (insert i ordset)) in
        assert (equal ordset ordset');
        assert (equal ordset (union ordset ordset'));
        assert (equal ordset (inter ordset ordset'));
        assert (equal e (diff ordset ordset'));
        ordset'
      end
  end in
  let e = empty (module Uns) in
  let _ = test 100L 0L e e in
  printf "@]"

let _ = test ()
