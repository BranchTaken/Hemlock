open! Basis.Rudiments
open! Basis
open Real
open Format

let test () =
  printf "@[<h>";
  printf "one: %h\n" one;
  printf "neg_one: %h\n" neg_one;
  printf "nan: %h\n" nan;
  printf "inf: %h\n" inf;
  printf "neg_inf: %h\n" neg_inf;
  printf "pi: %h\n" pi;
  printf "@]"

let _ = test ()
