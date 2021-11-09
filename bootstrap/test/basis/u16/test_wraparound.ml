open! Basis.Rudiments
open! Basis
open U16
open Format

let test () =
  let fifteen = (kv 15L) in
  printf "max_value + %a -> %a\n" xpp one xpp_x (max_value + one);
  printf "min_value - %a -> %a\n" xpp one xpp_x (min_value - one);
  printf "max_value * %a -> %a\n" xpp fifteen xpp_x (max_value * fifteen)

let _ = test ()
