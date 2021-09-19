open! Basis.Rudiments
open! Basis
open Unit
open Format

let test () =
  printf "@[<h>";
  printf "pp %s -> %a\n" (to_string ()) pp ();
  printf "@]"

let _ = test ()
