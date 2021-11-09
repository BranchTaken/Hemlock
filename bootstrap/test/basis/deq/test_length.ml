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
        let l = length t in
        printf "length %a = %a\n" xppt t Uns.xpp l;
        fn (succ i) n (push i t)
      end
  end in
  fn 0L 3L empty

let _ = test ()
