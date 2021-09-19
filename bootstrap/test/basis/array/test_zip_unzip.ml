open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test_zip arr0 arr1 = begin
    let arr0', arr1' = unzip (zip arr0 arr1) in
    printf "%a %a\n"
      (pp Uns.pp) arr0'
      (pp Uns.pp) arr1'
  end in
  printf "@[<h>";
  test_zip [||] [||];
  test_zip [|0|] [|1|];
  test_zip [|0; 1|] [|2; 3|];
  test_zip [|0; 1; 2|] [|3; 4; 5|];
  printf "@]"

let _ = test ()
