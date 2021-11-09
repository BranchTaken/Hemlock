open! Basis.Rudiments
open! Basis
open! OrdsetTest
open Ordset
open Format

let test () =
  printf "@[";
  let xpp_pair xppf (a0_opt, a1_opt) = begin
    fprintf xppf "(%a, %a)"
      (Option.xpp Uns.xpp) a0_opt
      (Option.xpp Uns.xpp) a1_opt
  end in
  let test ms0 ms1 = begin
    let ordset0 = of_list (module Uns) ms0 in
    let ordset1 = of_list (module Uns) ms1 in
    let pairs = fold2 ~init:[] ~f:(fun accum a0_opt a1_opt ->
      (a0_opt, a1_opt) :: accum
    ) ordset0 ordset1 in
    printf "fold2 %a %a -> %a@\n"
      (List.xpp Uns.xpp) ms0
      (List.xpp Uns.xpp) ms1
      (List.xpp xpp_pair) pairs
  end in
  let test_lists = [
    [];
    [0L];
    [0L; 1L];
    [0L; 1L; 2L];
    [0L; 1L; 66L];
    [0L; 1L; 66L; 91L];
  ] in
  List.iteri test_lists ~f:(fun i ms0 ->
    List.iteri test_lists ~f:(fun j ms1 ->
      if i <= j then test ms0 ms1
    )
  );
  printf "@]"

let _ = test ()
