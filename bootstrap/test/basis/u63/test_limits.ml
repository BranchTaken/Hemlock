open! Basis.Rudiments
open! Basis
open U63
open Format

let test () =
  printf "bit_length=%a\n" pp (bit_pop (bit_not zero));
  printf "min_value=%a\n" pp_x min_value;
  printf "max_value=%a\n" pp_x max_value

let _ = test ()
