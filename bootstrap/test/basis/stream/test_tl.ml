open! Basis.Rudiments
open! Basis
open Stream
open Format

let test () =
  let xppt = (xpp Uns.xpp) in
  printf "@[<h>";
  let rec test_tl_up_to i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t = init (0L =:< i) ~f:(fun i -> i) in
        let t' = tl t in
        printf "tl %a = %a\n" xppt t xppt t';
        test_tl_up_to (succ i) n
      end
  end in
  (* halts if we start at 0 *)
  test_tl_up_to 1L 4L;
  printf "@]"

let _ = test ()
