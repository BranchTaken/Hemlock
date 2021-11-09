open! Basis.Rudiments
open! Basis
open I256
open Format

let test () =
  let fifteen = of_string "15" in
  printf "neg_one + one -> %a\n" xpp_x (neg_one + one);
  printf "zero - one -> %a\n" xpp_x (zero - one);
  printf "neg_one * %a -> %a\n" xpp fifteen xpp_x (neg_one * fifteen)

let _ = test ()
