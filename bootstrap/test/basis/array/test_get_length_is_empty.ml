open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test_length arr = begin
    printf "%a: length=%a, is_empty=%B\n"
      (pp Uns.pp) arr
      Uns.pp (length arr)
      (is_empty arr)
  end in
  printf "@[<h>";
  test_length [||];
  test_length [|0|];
  test_length [|0; 1|];
  test_length [|0; 1; 2|];
  printf "@]"

let _ = test ()
