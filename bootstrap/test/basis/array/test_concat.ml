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
  test [|0|] [||];
  test [||] [|0|];
  test [|0|] [|1|];
  test [|0; 1|] [|2|];
  test [|0|] [|1; 2|];
  test [|0; 1|] [|2; 3|];
  printf "@]"

let _ = test ()
