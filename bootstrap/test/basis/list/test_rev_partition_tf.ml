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
    let even x = (x % 2 = 0) in
    let l_true, l_false = partition_tf l ~f:even in
    let rl_true, rl_false = rev_partition_tf l ~f:even in
    printf "[rev_]partition_tf %a ~f:even -> %a %a / %a %a\n"
      (pp Uns.pp) l
      (pp Uns.pp) l_true
      (pp Uns.pp) l_false
      (pp Uns.pp) rl_true
      (pp Uns.pp) rl_false
  );
  printf "@]"

let _ = test ()
