open! Basis.Rudiments
open! Basis
open Option
open Format

let test () =
  printf "@[<h>";
  printf "Some 42 -> %a\n" (pp Uns.pp) (Some 42L);
  printf "None -> %a\n" (pp Uns.pp) None;
  printf "@]"

let _ = test ()
