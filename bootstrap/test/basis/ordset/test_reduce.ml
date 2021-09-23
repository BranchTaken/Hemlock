open! Basis.Rudiments
open! Basis
open! OrdsetTest
open Ordset
open Format

let test () =
  printf "@[<h>";
  let test ms = begin
    let ordset = of_list (module Uns) ms in
    let sum = reduce ~f:( + ) ordset in
    printf "reduce ~f:( + ) %a -> %a\n"
      (List.pp Uns.pp) ms
      (Option.pp Uns.pp) sum
  end in
  let test_lists = [
    [];
    [0L];
    [0L; 1L];
    [0L; 1L; 2L];
    [0L; 1L; 66L];
    [0L; 1L; 66L; 91L];
  ] in
  List.iter test_lists ~f:(fun ms ->
    test ms
  );
  printf "@]"

let _ = test ()
