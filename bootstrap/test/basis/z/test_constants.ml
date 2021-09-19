open! Basis.Rudiments
open! Basis
open Z
open Format

let test () =
  printf "zero=%a\n" pp_x zero;
  printf "one=%a\n" pp_x one;
  printf "neg_one=%a\n" pp_x neg_one

let _ = test ()
