open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 2; 3];
    [0; 1; 2; 3; 4]
  ] in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    printf "reduce %a" (pp Uns.pp) l;
    match (reduce l ~f:(fun a b -> a + b)) with
    | None -> printf "-> None\n"
    | Some result -> printf " -> %a\n" Uns.pp result
  );
  printf "@]"

let _ = test ()
