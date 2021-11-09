open! Basis.Rudiments
open! Basis
open Deq
open Format

let test () =
  let xppt = (xpp Uns.xpp) in
  printf "@[<h>";
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t' = push i t in
        let elm = back t' in
        printf "back %a = %a\n" xppt t' Uns.xpp elm;
        fn (succ i) n t'
      end
  end in
  fn 0L 4L empty;
  printf "@]"

let _ = test ()
