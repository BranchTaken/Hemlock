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

    [0; 0];
    [0; 1; 1];
    [0; 1; 2; 2];

    [1; 0];
    [0; 2; 1]
  ] in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    printf "is_sorted               %a -> %b\n"
      (pp Uns.pp) l
      (is_sorted l ~cmp:Uns.cmp)
    ;
    printf "is_sorted ~strict:false %a -> %b\n"
      (pp Uns.pp) l
      (is_sorted ~strict:false l ~cmp:Uns.cmp)
    ;
    printf "is_sorted ~strict:true  %a -> %b\n"
      (pp Uns.pp) l
      (is_sorted ~strict:true l ~cmp:Uns.cmp)
  );
  printf "@]"

let _ = test ()
