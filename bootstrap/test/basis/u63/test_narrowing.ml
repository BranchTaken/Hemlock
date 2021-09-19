open! Basis.Rudiments
open! Basis
open U63
open Format

let test () =
  printf "max_value + 1 -> %a\n" pp_x (max_value + 1);
  printf "min_value - 1 -> %a\n" pp_x (min_value - 1);
  printf "max_value * 15 -> %a\n" pp_x (max_value * 15)

let _ = test ()
