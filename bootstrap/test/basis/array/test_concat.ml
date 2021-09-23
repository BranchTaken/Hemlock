open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test arr0 arr1 = begin
    printf "concat %a %a -> %a\n"
      (pp Uns.pp) arr0
      (pp Uns.pp) arr1
      (pp Uns.pp) (concat arr0 arr1)
    ;
  end in
  printf "@[<h>";
  test [||] [||];
  test [|0L|] [||];
  test [||] [|0L|];
  test [|0L|] [|1L|];
  test [|0L; 1L|] [|2L|];
  test [|0L|] [|1L; 2L|];
  test [|0L; 1L|] [|2L; 3L|];
  printf "@]"

let _ = test ()
