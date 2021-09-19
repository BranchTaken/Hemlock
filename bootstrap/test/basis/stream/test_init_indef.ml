open! Basis.Rudiments
open! Basis
open Stream
open Format

let test () =
  let ppt = (pp Uns.pp) in
  printf "@[<h>";
  let rec test_init_indef_up_to i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let f state = begin
          match state < i with
          | false -> None
          | true -> Some(state, succ state)
        end in
        let t = init_indef ~f 0 in
        printf "init_indef until %a = %a\n" Uns.pp i ppt t;
        test_init_indef_up_to (succ i) n
      end
  end in
  test_init_indef_up_to 0 3;
  printf "@]"

let _ = test ()
