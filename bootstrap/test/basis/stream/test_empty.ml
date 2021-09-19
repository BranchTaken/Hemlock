open! Basis.Rudiments
open! Basis
open Stream
open Format

let test () =
  let ppt = (pp Uns.pp) in
  printf "@[<h>";
  let t = empty in
  printf "empty = %a\n" ppt t;
  printf "@]"

let _ = test ()
