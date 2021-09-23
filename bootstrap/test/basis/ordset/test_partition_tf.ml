open! Basis.Rudiments
open! Basis
open! OrdsetTest
open Ordset
open Format

let test () =
  printf "@[<h>";
  let test arr = begin
    let ordset = of_array (module Uns) arr in
    let t_ordset, f_ordset = partition_tf ordset ~f:(fun mem -> mem % 2L = 0L) in
    let t_arr = to_array t_ordset in
    let f_arr = to_array f_ordset in
    printf "%a -> %a / %a@\n"
      (Array.pp Uns.pp) arr
      (Array.pp Uns.pp) t_arr
      (Array.pp Uns.pp) f_arr
  end in
  iter_oc 0L 7L (fun n ->
    let arr = Array.init n ~f:(fun i -> i) in
    test arr
  );
  printf "@]"

let _ = test ()
