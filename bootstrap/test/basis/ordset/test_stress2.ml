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
        (* Hash i in order to test semi-random insertion order. *)
        let h = Hash.(t_of_state (Uns.hash_fold i State.empty)) in
        let ordset' = remove h (test n (succ i) e (insert h ordset)) in
        assert (equal ordset ordset');
        assert (equal ordset (union ordset ordset'));
        assert (equal ordset (inter ordset ordset'));
        assert (equal e (diff ordset ordset'));
        ordset'
      end
  end in
  let e = empty (module U128) in
  let _ = test 100L 0L e e in
  printf "@]"

let _ = test ()
