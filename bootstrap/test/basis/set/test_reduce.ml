open! Basis.Rudiments
open! Basis
open! SetTest
open Set
open Format

let test () =
  printf "@[<h>";
  let test ms = begin
    let set = of_list (module Uns) ms in
    let sum = reduce ~f:( + ) set in
    printf "reduce ~f:( + ) %a -> %a\n"
      (List.xpp Uns.xpp) ms
      (Option.xpp Uns.xpp) sum
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
