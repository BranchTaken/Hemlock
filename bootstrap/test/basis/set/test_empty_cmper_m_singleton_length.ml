open! Basis.Rudiments
open! Basis
open SetTest
open Set
open Format

let test () =
  printf "@[";
  let e = empty (module Uns) in
  assert (length e = 0);
  printf "%a@\n" pp e;

  let s = singleton (cmper_m e) 0 in
  assert (length s = 1);
  printf "%a@\n" pp s;
  printf "@]"

let _ = test ()
