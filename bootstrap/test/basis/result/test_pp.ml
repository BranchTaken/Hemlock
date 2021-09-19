open! Basis.Rudiments
open! Basis
open Result
open Format

let test () =
  printf "@[<h>";
  printf "Ok 42 -> %a\n" (pp Uns.pp String.pp) (Ok 42);
  printf "Error \"bang\" -> %a\n" (pp Uns.pp String.pp) (Error "bang");
  printf "@]"

let _ = test ()
