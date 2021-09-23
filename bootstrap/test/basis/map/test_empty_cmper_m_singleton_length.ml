open! Basis.Rudiments
open! Basis
open MapTest
open Map
open Format

let test () =
  printf "@[";
  let e = empty (module UnsTestCmper) in
  validate e;
  assert (length e = 0L);
  printf "%a@\n" (pp Unit.pp) e;

  let s = singleton (cmper_m e) ~k:0L ~v:"0" in
  validate s;
  assert (length s = 1L);
  printf "%a@\n" (pp String.pp) s;
  printf "@]"

let _ = test ()
