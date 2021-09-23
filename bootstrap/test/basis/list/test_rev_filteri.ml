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
    [0L; 1L; 2L; 3L; 4L];
  ] in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    let f i _ = (i % 2L = 0L) in
    printf "[rev_]filteri %a -> %a / %a\n"
      (pp Uns.pp) l
      (pp Uns.pp) (filteri l ~f)
      (pp Uns.pp) (rev_filteri l ~f)
  );
  printf "@]"

let _ = test ()
