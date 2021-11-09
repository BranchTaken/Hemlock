open! Basis.Rudiments
open! Basis
open Stream
open Format

let test () =
  let xppt = (xpp Uns.xpp) in
  printf "@[<h>";
  let rec test_rev_split_up_to i l n =
    match i <= l, l <= n with
    | _, false -> ()
    | false, _ -> test_rev_split_up_to 0L (succ l) n
    | true, true -> begin
        let t = init (0L =:< l) ~f:(fun i -> i) in
        let t0, t1 = rev_split i t in
        printf "rev_split %a %a = %a %a\n" Uns.xpp i xppt t xppt t0 xppt t1;
        test_rev_split_up_to (succ i) l n
      end in
  test_rev_split_up_to 0L 0L 3L

let _ = test ()
