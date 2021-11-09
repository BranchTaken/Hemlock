open! Basis.Rudiments
open! Basis
open Z
open Format

let test () =
  printf "zero=%a\n" xpp_x zero;
  printf "one=%a\n" xpp_x one;
  printf "neg_one=%a\n" xpp_x neg_one

let _ = test ()
