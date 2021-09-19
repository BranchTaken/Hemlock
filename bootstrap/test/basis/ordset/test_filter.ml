open! Basis.Rudiments
open! Basis
open! OrdsetTest
open Ordset
open Format

let test () =
  printf "@[<h>";
  let test arr = begin
    let ordset = of_array (module Uns) arr in
    let ordset' = filter ordset ~f:(fun mem -> mem % 2 = 0) in
    let arr' = to_array ordset' in
    printf "%a -> %a@\n"
      (Array.pp Uns.pp) arr
      (Array.pp Uns.pp) arr'
  end in
  for n = 0 to 6 do
    let arr = Array.init n ~f:(fun i -> i) in
    test arr
  done;
  printf "@]"

let _ = test ()
