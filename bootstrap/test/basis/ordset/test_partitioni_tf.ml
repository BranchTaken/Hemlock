open! Basis.Rudiments
open! Basis
open! OrdsetTest
open Ordset
open Format

let test () =
  printf "@[<h>";
  let test arr = begin
    let ordset = of_array (module Uns) arr in
    let t_ordset, f_ordset = partitioni_tf ordset ~f:(fun i _mem -> i % 2L = 0L) in
    let t_arr = to_array t_ordset in
    let f_arr = to_array f_ordset in
    printf "%a -> %a / %a@\n"
      (Array.xpp Uns.xpp) arr
      (Array.xpp Uns.xpp) t_arr
      (Array.xpp Uns.xpp) f_arr
  end in
  Range.iter (0L =:< 7L) ~f:(fun n ->
    let arr = Array.init (0L =:< n) ~f:(fun i -> i * 10L) in
    test arr
  );
  printf "@]"

let _ = test ()
