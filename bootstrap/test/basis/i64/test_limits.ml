open! Basis.Rudiments
open! Basis
open I64
open Format

let test () =
  printf "min_value=%a\n" pp_x min_value;
  printf "max_value=%a\n" pp_x max_value

let _ = test ()
