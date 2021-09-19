open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test arr x = begin
    let arr_x = append x arr in
    let x_arr = prepend x arr in
    printf "%a %a: append -> %a, prepend -> %a\n"
      (pp Uns.pp) arr
      Uns.pp x
      (pp Uns.pp) arr_x
      (pp Uns.pp) x_arr
  end in
  printf "@[<h>";
  test [||] 0;
  test [|0|] 1;
  test [|0; 1|] 2;
  test [|0; 1; 2|] 3;
  printf "@]"

let _ = test ()
