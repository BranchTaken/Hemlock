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
    [0; 1; 2; 3; 4];
  ] in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    let f i _ = (i % 2 = 0) in
    printf "[rev_]filteri %a -> %a / %a\n"
      (pp Uns.pp) l
      (pp Uns.pp) (filteri l ~f)
      (pp Uns.pp) (rev_filteri l ~f)
  );
  printf "@]"

let _ = test ()
