open! Basis.Rudiments
open! Basis
open U64
open Format

let test () =
  printf "min_value=%a\n" xpp_x min_value;
  printf "max_value=%a\n" xpp_x max_value

let _ = test ()
