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
      (List.pp Uns.pp) ms
      (Option.pp Uns.pp) sum
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
  ] in
  List.iter test_lists ~f:(fun ms ->
    test ms
  );
  printf "@]"

let _ = test ()
