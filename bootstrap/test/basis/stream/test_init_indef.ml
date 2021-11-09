open! Basis.Rudiments
open! Basis
open Stream
open Format

let test () =
  let xppt = (xpp Uns.xpp) in
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
        let t = init_indef ~f 0L in
        printf "init_indef until %a = %a\n" Uns.xpp i xppt t;
        test_init_indef_up_to (succ i) n
      end
  end in
  test_init_indef_up_to 0L 3L;
  printf "@]"

let _ = test ()
