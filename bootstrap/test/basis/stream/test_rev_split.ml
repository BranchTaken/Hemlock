open! Basis.Rudiments
open! Basis
open Stream
open Format

let test () =
  let ppt = (pp Uns.pp) in
  printf "@[<h>";
  let rec test_rev_split_up_to i l n =
    match i <= l, l <= n with
    | _, false -> ()
    | false, _ -> test_rev_split_up_to 0 (succ l) n
    | true, true -> begin
        let t = init l ~f:(fun i -> i) in
        let t0, t1 = rev_split i t in
        printf "rev_split %a %a = %a %a\n" Uns.pp i ppt t ppt t0 ppt t1;
        test_rev_split_up_to (succ i) l n
      end in
  test_rev_split_up_to 0 0 3

let _ = test ()
