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
  ] in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    printf "rev %a -> %a\n"
      (pp Uns.pp) l
      (pp Uns.pp) (rev l)
  );
  printf "@]"

let _ = test ()
