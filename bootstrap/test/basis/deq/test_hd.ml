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
        let t' = push_back i t in
        let elm = hd t' in
        printf "hd %a = %a\n" ppt t' Uns.pp elm;
        fn (succ i) n t'
      end
  end in
  fn 0 4 empty;
  printf "@]"

let _ = test ()
