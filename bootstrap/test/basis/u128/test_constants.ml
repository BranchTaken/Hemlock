open! Basis.Rudiments
open! Basis
open U128
open Format

let test () =
  printf "zero=%a\n" pp_x zero;
  printf "one=%a\n" pp_x one;
  printf "min_value=%a\n" pp_x min_value;
  printf "max_value=%a\n" pp_x max_value

let _ = test ()
