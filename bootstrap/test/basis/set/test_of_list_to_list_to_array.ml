open! Basis.Rudiments
open! Basis
open! SetTest
open Set
open Format

let test () =
  printf "@[<h>";
  let test ms = begin
    let set = of_list (module Uns) ms in
    let list_sorted = List.sort ~cmp:Uns.cmp (to_list set) in
    let array_sorted = Array.sort ~cmp:Uns.cmp (to_array set) in
    printf "of_list %a; to_list -> %a; to_array -> %a\n"
      (List.pp Uns.pp) ms
      (List.pp Uns.pp) list_sorted
      (Array.pp Uns.pp) array_sorted
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
