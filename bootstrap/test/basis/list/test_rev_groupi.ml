open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let lists = [
    [];
    [9];
    [9; 9];

    [0; 1];
    [9; 1; 2; 9];

    [0; 1; 2];
    [9; 1; 2; 9; 4; 5; 9];
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
