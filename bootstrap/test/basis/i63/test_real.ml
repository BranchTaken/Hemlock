open! Basis.Rudiments
open! Basis
open I63
open Format

let test () =
  let x = of_real 0. in
  printf "%a\n" pp x;
  let r = to_real (kv 1) in
  printf "%.1f\n" r

let _ = test ()
