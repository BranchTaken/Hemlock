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
  ] in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    let f i elm = elm + i * 10 in
    printf "[rev_]mapi %a -> %a / %a\n"
      (pp Uns.pp) l
      (pp Uns.pp) (mapi l ~f)
      (pp Uns.pp) (rev_mapi l ~f)
  );
  printf "@]"

let _ = test ()
