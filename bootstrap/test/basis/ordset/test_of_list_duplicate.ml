open! Basis.Rudiments
open! Basis
open OrdsetTest
open Ordset
open Format

let test () =
  printf "@[";
  printf "%a@\n" pp (of_list (module Uns) [0L; 0L]);
  printf "@]"

let _ = test ()
