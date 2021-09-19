open! Basis.Rudiments
open! Basis
open Real
open Format

let test () =
  printf "@[<h>";
  printf "min_value: %h\n" min_value;
  printf "max_value: %h\n" max_value;
  printf "@]"

let _ = test ()
