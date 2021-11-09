open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap
open Format

let test () =
  printf "@[";
  let e = empty (module Uns) in
  validate e;
  assert (length e = 0L);
  printf "%a@\n" (xpp Unit.xpp) e;

  let s = singleton (cmper_m e) ~k:0L ~v:"zero" in
  validate s;
  assert (length s = 1L);
  printf "%a@\n" (xpp String.xpp) s;
  printf "@]"

let _ = test ()
