open! Basis.Rudiments
open! Basis
open OrdsetTest
open Ordset
open Format

let test () =
  printf "@[";
  printf "%a@\n" pp (of_list (module Uns) [0; 0]);
  printf "@]"

let _ = test ()
