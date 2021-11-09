open! Basis.Rudiments
open! Basis
open SetTest
open Set
open Format

let test () =
  printf "@[";
  let e = empty (module Uns) in
  assert (length e = 0L);
  printf "%a@\n" xpp e;

  let s = singleton (cmper_m e) 0L in
  assert (length s = 1L);
  printf "%a@\n" xpp s;
  printf "@]"

let _ = test ()
