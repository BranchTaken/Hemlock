open! Basis.Rudiments
open! Basis
open Unit
open Format

let test () =
  let t0 = () in
  let t1 = () in
  printf "@[<h>";
  printf "cmp %s %s -> %a\n" (to_string t0) (to_string t1) Cmp.xpp (cmp t0 t1);
  printf "%s = %s -> %B\n" (to_string t0) (to_string t1) (t0 = t1);
  printf "%s <> %s -> %B\n" (to_string t0) (to_string t1) (t0 <> t1);
  printf "@]"

let _ = test ()
