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
  ] in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    let f i elm = elm + i * 10L in
    printf "[rev_]mapi %a -> %a / %a\n"
      (pp Uns.pp) l
      (pp Uns.pp) (mapi l ~f)
      (pp Uns.pp) (rev_mapi l ~f)
  );
  printf "@]"

let _ = test ()
