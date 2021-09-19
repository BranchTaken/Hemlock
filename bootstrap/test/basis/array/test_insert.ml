open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test arr x = begin
    printf "insert %a %a ->"
      (pp Uns.pp) arr
      Uns.pp x
    ;
    for i = 0 to length arr do
      let arr' = insert i x arr in
      printf " %a" (pp Uns.pp) arr'
    done;
    printf "\n"
  end in
  printf "@[<h>";
  test [||] 0;
  test [|0|] 1;
  test [|0; 1|] 2;
  test [|0; 1; 2|] 3;
  printf "@]"

let _ = test ()
