open! Basis.Rudiments
open! Basis
open Option
open Format

let test () =
  printf "@[<h>";
  printf "Some 42 -> %a\n" (xpp Uns.xpp) (Some 42L);
  printf "None -> %a\n" (xpp Uns.xpp) None;
  printf "@]"

let _ = test ()
