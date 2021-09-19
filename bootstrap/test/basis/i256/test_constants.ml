open! Basis.Rudiments
open! Basis
open I256
open Format

let test () =
  printf "zero=%a %a\n" pp_x zero pp zero;
  printf "one=%a %a\n" pp_x one pp one;
  printf "min_value=%a %a\n" pp_x min_value pp min_value;
  printf "max_value=%a %a\n" pp_x max_value pp max_value

let _ = test ()
