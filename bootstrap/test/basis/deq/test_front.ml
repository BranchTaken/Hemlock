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
        let t'' = front t' in
        printf "front %a = %a\n" ppt t' ppt t'';
        fn (succ i) n t'
      end
  end in
  fn 0 4 empty;
  printf "@]"

let _ = test ()
