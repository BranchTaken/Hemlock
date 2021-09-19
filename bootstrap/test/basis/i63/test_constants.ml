open! Basis.Rudiments
open! Basis
open I63
open Format

let test () =
  printf "zero=%a\n" pp zero;
  printf "one=%a\n" pp one;
  printf "neg_one=%a\n" pp neg_one

let _ = test ()
