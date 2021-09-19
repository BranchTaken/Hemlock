open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test_blit len i0 arr0 i1 arr1 = begin
    printf "blit %a %a %a %a %a -> "
      Uns.pp len
      Uns.pp i0
      (pp Uns.pp) arr0
      Uns.pp i1
      (pp Uns.pp) arr1
    ;
    blit len i0 arr0 i1 arr1;
    printf "%a\n" (pp Uns.pp) arr1
  end in
  printf "@[<h>";
  test_blit 0 0 [||] 0 [||];
  test_blit 1 0 [|0|] 0 [|1|];
  test_blit 1 1 [|0; 1|] 0 [|2|];
  test_blit 1 0 [|0|] 1 [|1; 2|];
  test_blit 2 0 [|0; 1|] 0 [|2; 3|];
  test_blit 2 1 [|0; 1; 2|] 0 [|3; 4; 5|];
  test_blit 3 0 [|0; 1; 2|] 0 [|3; 4; 5|];
  printf "@]"

let _ = test ()
