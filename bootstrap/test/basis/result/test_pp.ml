open! Basis.Rudiments
open! Basis
open Result
open Format

let test () =
  printf "@[<h>";
  printf "Ok 42 -> %a\n" (xpp Uns.xpp String.xpp) (Ok 42L);
  printf "Error \"bang\" -> %a\n" (xpp Uns.xpp String.xpp) (Error "bang");
  printf "@]"

let _ = test ()
