open! Basis.Rudiments
open! Basis
open I256
open Format

let test () =
  let fifteen = of_string "15" in
  printf "neg_one + one -> %a\n" pp_x (neg_one + one);
  printf "zero - one -> %a\n" pp_x (zero - one);
  printf "neg_one * %a -> %a\n" pp fifteen pp_x (neg_one * fifteen)

let _ = test ()
