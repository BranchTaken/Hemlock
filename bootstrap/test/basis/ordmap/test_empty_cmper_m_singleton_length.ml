open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap
open Format

let test () =
  printf "@[";
  let e = empty (module Uns) in
  validate e;
  assert (length e = 0);
  printf "%a@\n" (pp Unit.pp) e;

  let s = singleton (cmper_m e) ~k:0 ~v:"zero" in
  validate s;
  assert (length s = 1);
  printf "%a@\n" (pp String.pp) s;
  printf "@]"

let _ = test ()
