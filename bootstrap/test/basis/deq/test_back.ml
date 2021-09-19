open! Basis.Rudiments
open! Basis
open Deq
open Format

let test () =
  let ppt = (pp Uns.pp) in
  printf "@[<h>";
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t' = push i t in
        let elm = back t' in
        printf "back %a = %a\n" ppt t' Uns.pp elm;
        fn (succ i) n t'
      end
  end in
  fn 0 4 empty;
  printf "@]"

let _ = test ()
