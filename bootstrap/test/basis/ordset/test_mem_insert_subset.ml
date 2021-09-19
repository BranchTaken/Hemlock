open! Basis.Rudiments
open! Basis
open OrdsetTest
open Ordset
open Format

let test () =
  printf "@[";
  let rec test ms ordset = begin
    match ms with
    | [] -> printf "%a@\n" pp ordset
    | m :: ms' -> begin
        assert (not (mem m ordset));
        let ordset' = insert m ordset in
        assert (mem m ordset');
        assert (subset ordset' ordset);
        assert (not (subset ordset ordset'));
        test ms' ordset'
      end
  end in
  let ms = [1; 3; 2; 44; 45; 56; 60; 66; 75; 81; 91] in
  test ms (empty (module Uns));
  printf "@]"

let _ = test ()
