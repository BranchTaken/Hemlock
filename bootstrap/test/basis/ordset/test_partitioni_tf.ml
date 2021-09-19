open! Basis.Rudiments
open! Basis
open! OrdsetTest
open Ordset
open Format

let test () =
  printf "@[<h>";
  let test arr = begin
    let ordset = of_array (module Uns) arr in
    let t_ordset, f_ordset = partitioni_tf ordset ~f:(fun i _mem -> i % 2 = 0) in
    let t_arr = to_array t_ordset in
    let f_arr = to_array f_ordset in
    printf "%a -> %a / %a@\n"
      (Array.pp Uns.pp) arr
      (Array.pp Uns.pp) t_arr
      (Array.pp Uns.pp) f_arr
  end in
  for n = 0 to 6 do
    let arr = Array.init n ~f:(fun i -> i * 10) in
    test arr
  done;
  printf "@]"

let _ = test ()
