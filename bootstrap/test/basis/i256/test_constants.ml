open! Basis.Rudiments
open! Basis
open I256
open Format

let test () =
  printf "zero=%a %a\n" xpp_x zero xpp zero;
  printf "one=%a %a\n" xpp_x one xpp one;
  printf "min_value=%a %a\n" xpp_x min_value xpp min_value;
  printf "max_value=%a %a\n" xpp_x max_value xpp max_value

let _ = test ()
