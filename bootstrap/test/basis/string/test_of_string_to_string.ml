open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let s = "hello" in
  let s2 = of_string s in
  let s3 = to_string s2 in

  printf "s=%a, s2=%a, s3=%a\n" pp s pp s2 pp s3

let _ = test ()
