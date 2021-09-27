open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test_is_sorted arr = begin
    printf "is_sorted %a: not strict -> %B, strict -> %B\n"
      (pp Uns.pp) arr
      (is_sorted arr ~cmp:Uns.cmp)
      (is_sorted ~strict:true arr ~cmp:Uns.cmp)
  end in
  printf "@[<h>";
  test_is_sorted [||];
  test_is_sorted [|0|];
  test_is_sorted [|0; 0|];
  test_is_sorted [|0; 1|];
  test_is_sorted [|1; 0|];
  test_is_sorted [|0; 1; 1|];
  test_is_sorted [|0; 1; 2|];
  test_is_sorted [|0; 2; 1|];
  printf "@]"

let _ = test ()