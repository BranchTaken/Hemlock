open! Basis.Rudiments
open! Basis
open Unit
open Format

let test () =
  printf "@[<h>";
  printf "pp %s -> %a\n" (to_string ()) xpp ();
  printf "@]"

let _ = test ()
