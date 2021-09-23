open! Basis.Rudiments
open! Basis
open SetTest
open Set
open Format

let test () =
  printf "@[";
  printf "%a@\n" pp (of_list (module Uns) [0L; 0L]);
  printf "@]"

let _ = test ()
