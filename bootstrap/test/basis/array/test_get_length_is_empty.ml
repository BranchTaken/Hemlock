open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test_length arr = begin
    printf "%a: length=%a, is_empty=%B\n"
      (xpp Uns.xpp) arr
      Uns.xpp (length arr)
      (is_empty arr)
  end in
  printf "@[<h>";
  test_length [||];
  test_length [|0L|];
  test_length [|0L; 1L|];
  test_length [|0L; 1L; 2L|];
  printf "@]"

let _ = test ()
