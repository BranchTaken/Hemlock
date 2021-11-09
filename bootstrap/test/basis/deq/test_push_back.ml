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
        let t' = push_back i t in
        printf "push_back %a %a = %a\n" Uns.xpp i xppt t xppt t';
        fn (succ i) n t'
      end
  end in
  fn 0L 4L empty;
  printf "@]"

let _ = test ()
