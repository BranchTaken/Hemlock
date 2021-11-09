open! Basis.Rudiments
open! Basis
open U16
open Format

let test () =
  printf "bit_length=%a\n" Uns.xpp (bit_pop (bit_not zero));
  printf "min_value=%a\n" xpp_x min_value;
  printf "max_value=%a\n" xpp_x max_value

let _ = test ()
