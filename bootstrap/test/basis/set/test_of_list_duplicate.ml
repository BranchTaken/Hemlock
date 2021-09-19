open! Basis.Rudiments
open! Basis
open SetTest
open Set
open Format

let test () =
  printf "@[";
  printf "%a@\n" pp (of_list (module Uns) [0; 0]);
  printf "@]"

let _ = test ()
