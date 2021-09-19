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
  ] in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    printf "rev %a -> %a\n"
      (pp Uns.pp) l
      (pp Uns.pp) (rev l)
  );
  printf "@]"

let _ = test ()
