open! Basis.Rudiments
open! Basis
open Stream
open Format

let test () =
  let ppt = (pp Uns.pp) in
  printf "@[<h>";
  let rec test_length_up_to i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t = init i ~f:(fun i -> i) in
        let l = length t in
        printf "length %a = %a\n" ppt t Uns.pp l;
        test_length_up_to (succ i) n
      end
  end in
  test_length_up_to 0 3

let _ = test ()
