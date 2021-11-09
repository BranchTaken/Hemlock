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
      (xpp Uns.xpp) l
      (xpp Uns.xpp) (rev l)
  );
  printf "@]"

let _ = test ()
