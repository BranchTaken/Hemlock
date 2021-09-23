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
  test_blit 0L 0L [||] 0L [||];
  test_blit 1L 0L [|0L|] 0L [|1L|];
  test_blit 1L 1L [|0L; 1L|] 0L [|2L|];
  test_blit 1L 0L [|0L|] 1L [|1L; 2L|];
  test_blit 2L 0L [|0L; 1L|] 0L [|2L; 3L|];
  test_blit 2L 1L [|0L; 1L; 2L|] 0L [|3L; 4L; 5L|];
  test_blit 3L 0L [|0L; 1L; 2L|] 0L [|3L; 4L; 5L|];
  printf "@]"

let _ = test ()
