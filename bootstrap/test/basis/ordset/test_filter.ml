open! Basis.Rudiments
open! Basis
open! OrdsetTest
open Ordset
open Format

let test () =
  printf "@[<h>";
  let test arr = begin
    let ordset = of_array (module Uns) arr in
    let ordset' = filter ordset ~f:(fun mem -> mem % 2L = 0L) in
    let arr' = to_array ordset' in
    printf "%a -> %a@\n"
      (Array.xpp Uns.xpp) arr
      (Array.xpp Uns.xpp) arr'
  end in
  Range.iter (0L =:< 7L) ~f:(fun n ->
    let arr = Array.init (0L =:< n) ~f:(fun i -> i) in
    test arr
  );
  printf "@]"

let _ = test ()
