open! Basis.Rudiments
open! Basis
open Stream
open Format

let test () =
  let ppt = (pp Uns.pp) in
  printf "@[<h>";
  let rec test_init_up_to i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t = init i ~f:(fun i -> i) in
        printf "init %a ~f:(fun i -> i) = %a\n" Uns.pp i ppt t;
        test_init_up_to (succ i) n
      end
  end in
  test_init_up_to 0 3;
  printf "@]"

let _ = test ()
