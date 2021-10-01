open! Basis.Rudiments
open! Basis
open! OrdsetTest
open Ordset
open Format

let test () =
  printf "@[<h>";
  let test arr = begin
    let ordset = of_array (module Uns) arr in
    let ordset' = filteri ordset ~f:(fun i _mem -> i % 2L = 0L) in
    let arr' = to_array ordset' in
    printf "%a -> %a@\n"
      (Array.pp Uns.pp) arr
      (Array.pp Uns.pp) arr'
  end in
  Range.iter (0L =:< 7L) ~f:(fun n ->
    let arr = Array.init (0L =:< n) ~f:(fun i -> i * 10L) in
    test arr
  );
  printf "@]"

let _ = test ()
