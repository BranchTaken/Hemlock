open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let list_pairs = [
    ([], []);
    ([0], [1]);
    ([0; 1], [2; 3]);
    ([0; 1; 2], [3; 4; 5]);
  ] in
  printf "@[<h>";
  iter list_pairs ~f:(fun (a, b) ->
    printf "iter2 %a %a ->"
      (pp Uns.pp) a
      (pp Uns.pp) b
    ;
    let f i a b = begin
      printf " (i=%a, a=%a, b=%a)" Uns.pp i Uns.pp a Uns.pp b
    end in
    iteri2 a b ~f;
    printf "\n"
  );
  printf "@]"

let _ = test ()
