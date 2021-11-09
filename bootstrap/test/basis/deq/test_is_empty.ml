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
        let e = is_empty t in
        printf "is_empty %a = %b\n" xppt t e;
        fn (succ i) n (push i t);
        fn (succ i) n (push_back i t)
      end
  end in
  fn 0L 1L empty

let _ = test ()
