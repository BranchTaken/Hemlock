open! Basis.Rudiments
open! Basis
open Stream
open Format

let test () =
  let ppt = (pp Uns.pp) in
  printf "@[<h>";
  let rec test_split_up_to i l n =
    match i <= l, l <= n with
    | _, false -> ()
    | false, _ -> test_split_up_to 0L (succ l) n
    | true, true -> begin
        let t = init l ~f:(fun i -> i) in
        let t0, t1 = split i t in
        printf "split %a %a = %a %a\n" Uns.pp i ppt t ppt t0 ppt t1;
        test_split_up_to (succ i) l n
      end in
  test_split_up_to 0L 0L 3L;
  printf "@]"

let _ = test ()
