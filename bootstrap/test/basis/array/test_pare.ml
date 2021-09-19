open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test arr = begin
    printf "pare %a ->" (pp Uns.pp) arr;
    for i = 0 to length arr do
      for j = i to length arr do
        let arr' = pare arr ~base:i ~past:j in
        printf " [%a,%a)=%a"
          Uns.pp i
          Uns.pp j
          (pp Uns.pp) arr'
      done
    done;
    printf "\n"
  end in
  printf "@[<h>";
  test [||];
  test [|0|];
  test [|0; 1|];
  test [|0; 1; 2|];
  printf "@]"

let _ = test ()
