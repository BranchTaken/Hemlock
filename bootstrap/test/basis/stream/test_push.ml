open! Basis.Rudiments
open! Basis
open Stream
open Format

let test () =
  let xppt = (xpp Uns.xpp) in
  printf "@[<h>";
  let rec test_push_up_to t i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t' = push i t in
        printf "push %a %a = %a\n" Uns.xpp i xppt t xppt t';
        test_push_up_to t' (succ i) n
      end
  end in
  test_push_up_to empty 0L 3L;
  printf "@]"

let _ = test ()
