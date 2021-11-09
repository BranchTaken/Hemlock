open! Basis.Rudiments
open! Basis
open Bool
open Format

let test () =
  let fn t0 t1 = begin
    printf "cmp %b %b -> %a\n" t0 t1 Cmp.xpp (cmp t0 t1);
    printf "%b = %b -> %b\n" t0 t1 (t0 = t1);
    printf "%b <> %b -> %b\n" t0 t1 (t0 <> t1);
  end in
  printf "@[<h>";
  fn false false;
  printf "\n";
  fn false true;
  printf "\n";
  fn true false;
  printf "\n";
  fn true true;
  printf "@]"

let _ = test ()
