open! Basis.Rudiments
open! Basis
open I32
open Format

let test () =
  printf "bit_length=%a\n" Uns.pp (bit_pop (bit_not zero));
  printf "min_value=%a %a\n" pp min_value pp_x min_value;
  printf "max_value=%a %a\n" pp max_value pp_x max_value

let _ = test ()
