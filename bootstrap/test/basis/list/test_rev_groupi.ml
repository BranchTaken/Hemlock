open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let lists = [
    [];
    [9L];
    [9L; 9L];

    [0L; 1L];
    [9L; 1L; 2L; 9L];

    [0L; 1L; 2L];
    [9L; 1L; 2L; 9L; 4L; 5L; 9L];
  ] in
  let inds i x0 x1 = (i = x1) && (i = succ x0) in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    printf "[rev_]groupi %a ~break:inds -> %a / %a\n"
      (pp Uns.pp) l
      (pp (pp Uns.pp)) (groupi l ~break:inds)
      (pp (pp Uns.pp)) (rev_groupi l ~break:inds)
  );
  printf "@]"

let _ = test ()
