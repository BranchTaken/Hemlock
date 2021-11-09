open! Basis.Rudiments
open! Basis
open Stream
open Format

let test () =
  let xppt = (xpp Uns.xpp) in
  printf "@[<h>";
  let rec test_rev_up_to i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t = init (0L =:< i) ~f:(fun i -> i) in
        let t' = rev t in
        printf "rev %a = %a\n" xppt t xppt t';
        test_rev_up_to (succ i) n
      end
  end in
  test_rev_up_to 0L 3L;
  printf "@]"

let _ = test ()
