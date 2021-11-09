open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test_is_sorted arr = begin
    printf "is_sorted %a: not strict -> %B, strict -> %B\n"
      (xpp Uns.xpp) arr
      (is_sorted arr ~cmp:Uns.cmp)
      (is_sorted ~strict:true arr ~cmp:Uns.cmp)
  end in
  printf "@[<h>";
  test_is_sorted [||];
  test_is_sorted [|0L|];
  test_is_sorted [|0L; 0L|];
  test_is_sorted [|0L; 1L|];
  test_is_sorted [|1L; 0L|];
  test_is_sorted [|0L; 1L; 1L|];
  test_is_sorted [|0L; 1L; 2L|];
  test_is_sorted [|0L; 2L; 1L|];
  printf "@]"

let _ = test ()
