open! Basis.Rudiments
open! Basis
open Stream
open Format

let test () =
  let ppt = (pp Uns.pp) in
  printf "@[<h>";
  let rec test_is_empty_up_to i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t = init i ~f:(fun i -> i) in
        let e = is_empty t in
        printf "is_empty %a = %b\n" ppt t e;
        test_is_empty_up_to (succ i) n
      end
  end in
  test_is_empty_up_to 0 3

let _ = test ()
