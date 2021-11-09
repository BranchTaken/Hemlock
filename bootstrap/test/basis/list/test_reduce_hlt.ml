open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let lists = [
    [];
    [0L];
    [0L; 1L];
    [0L; 1L; 2L];
    [0L; 1L; 2L; 3L];
    [0L; 1L; 2L; 3L; 4L]
  ] in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    printf "reduce %a" (xpp Uns.xpp) l;
    match (reduce l ~f:(fun a b -> a + b)) with
    | None -> printf "-> None\n"
    | Some result -> printf " -> %a\n" Uns.xpp result
  );
  printf "@]"

let _ = test ()
