open! Basis.Rudiments
open! Basis
open! OrdsetTest
open Ordset
open Format

let test () =
  printf "@[<h>";
  let test ms = begin
    let ordset = of_list (module Uns) ms in
    printf "of_list %a; to_list -> %a; to_array -> %a\n"
      (List.xpp Uns.xpp) ms
      (List.xpp Uns.xpp) (to_list ordset)
      (Array.xpp Uns.xpp) (to_array ordset)
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
