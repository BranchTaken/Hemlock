open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test_rev arr = begin
    printf "rev %a -> %a -> rev_inplace %a -> "
      (xpp Uns.xpp) arr
      (xpp Uns.xpp) (rev arr)
      (xpp Uns.xpp) arr
    ;
    rev_inplace arr;
    printf "%a\n" (xpp Uns.xpp) arr
  end in
  printf "@[<h>";
  test_rev [|0L|];
  test_rev [|0L; 1L|];
  test_rev [|0L; 1L; 2L|];
  test_rev [|0L; 1L; 2L; 3L|];
  printf "@]"

let _ = test ()
