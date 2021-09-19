open! Basis.Rudiments
open! Basis
open N
open Format

let test () =
  printf "zero=%a\n" pp_x zero;
  printf "one=%a\n" pp_x one

let _ = test ()
