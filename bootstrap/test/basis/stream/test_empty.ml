open! Basis.Rudiments
open! Basis
open Stream
open Format

let test () =
  let xppt = (xpp Uns.xpp) in
  printf "@[<h>";
  let t = empty in
  printf "empty = %a\n" xppt t;
  printf "@]"

let _ = test ()
