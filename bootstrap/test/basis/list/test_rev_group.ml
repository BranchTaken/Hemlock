open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let lists = [
    [];
    [0L];
    [0L; 1L];

    [0L; 0L];

    [0L; 0L; 0L];

    [0L; 0L; 1L; 1L];
    [0L; 1L; 1L; 2L; 2L; 3L];

    [0L; 0L; 0L; 0L];
  ] in
  let eq x0 x1 = (x0 = x1) in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    printf "[rev_]group %a ~break:eq -> %a / %a\n"
      (pp Uns.pp) l
      (pp (pp Uns.pp)) (group l ~break:eq)
      (pp (pp Uns.pp)) (rev_group l ~break:eq)
  );
  printf "@]"

let _ = test ()
