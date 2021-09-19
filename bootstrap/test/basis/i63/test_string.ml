open! Basis.Rudiments
open! Basis
open I63
open Format

let test () =
  let x = of_string "0" in
  printf "%a\n" pp x;
  let s = to_string (kv 1) in
  printf "%s\n" s

let _ = test ()
