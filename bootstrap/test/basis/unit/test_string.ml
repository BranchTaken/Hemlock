open! Basis.Rudiments
open! Basis
open Unit
open Format

let test () =
  printf "to_string () -> %s\n" (to_string ());
  printf "of_string unit -> %s\n" (to_string (of_string "unit"));
  printf "of_string () -> %s\n" (to_string (of_string "()"))

let _ = test ()
