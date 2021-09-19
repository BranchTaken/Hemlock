open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test arr = begin
    printf "remove %a ->" (pp Uns.pp) arr;
    for i = 0 to pred (length arr) do
      let arr' = remove i arr in
      printf " %a" (pp Uns.pp) arr';
    done;
    printf "\n"
  end in
  printf "@[<h>";
  test [|0|];
  test [|0; 1|];
  test [|0; 1; 2|];
  printf "@]"

let _ = test ()
