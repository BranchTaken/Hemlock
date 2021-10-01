open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let range_pp ppf r = begin
    Format.fprintf ppf "(%Lu .. %Lu)" (Range.base r) (Range.past r)
  end in
  let test_blit r0 arr0 r1 arr1 = begin
    printf "blit %a %a %a %a -> "
      range_pp r0
      (pp Uns.pp) arr0
      range_pp r1
      (pp Uns.pp) arr1
    ;
    blit r0 arr0 r1 arr1;
    printf "%a\n" (pp Uns.pp) arr1
  end in
  printf "@[<h>";
  test_blit (0L =:< 0L) [||] (0L =:< 0L) [||];
  test_blit (0L =:< 1L) [|0L|] (0L =:< 1L) [|1L|];
  test_blit (1L =:< 2L) [|0L; 1L|] (0L =:< 1L) [|2L|];
  test_blit (0L =:< 1L) [|0L|] (1L =:< 2L) [|1L; 2L|];
  test_blit (0L =:< 2L) [|0L; 1L|] (0L =:< 2L) [|2L; 3L|];
  test_blit (1L =:< 3L) [|0L; 1L; 2L|] (0L =:< 2L) [|3L; 4L; 5L|];
  test_blit (0L =:< 3L) [|0L; 1L; 2L|] (0L =:< 3L) [|3L; 4L; 5L|];
  printf "@]"

let _ = test ()
