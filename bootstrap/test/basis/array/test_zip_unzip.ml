open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test_zip arr0 arr1 = begin
    let arr0', arr1' = unzip (zip arr0 arr1) in
    printf "%a %a\n"
      (xpp Uns.xpp) arr0'
      (xpp Uns.xpp) arr1'
  end in
  printf "@[<h>";
  test_zip [||] [||];
  test_zip [|0L|] [|1L|];
  test_zip [|0L; 1L|] [|2L; 3L|];
  test_zip [|0L; 1L; 2L|] [|3L; 4L; 5L|];
  printf "@]"

let _ = test ()
