open! Basis.Rudiments
open! Basis
open I32
open Format

let test () =
  printf "max_value + %a -> %a\n" xpp one xpp_x (max_value + one);
  printf "min_value - %a -> %a\n" xpp one xpp_x (min_value - one)

let _ = test ()
