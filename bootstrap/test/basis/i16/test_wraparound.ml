open! Basis.Rudiments
open! Basis
open I16
open Format

let test () =
  printf "max_value + %a -> %a\n" pp one pp_x (max_value + one);
  printf "min_value - %a -> %a\n" pp one pp_x (min_value - one)

let _ = test ()
