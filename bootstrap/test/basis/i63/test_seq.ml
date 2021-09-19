open! Basis.Rudiments
open! Basis
open I63
open Format

let test () =
  let x = kv 1 in
  printf "succ: %a -> %a\n" pp x pp (succ x);
  printf "pred: %a -> %a\n" pp x pp (pred x)

let _ = test ()
