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

    [0L; 0L];
    [0L; 1L; 1L];
    [0L; 1L; 2L; 2L];

    [1L; 0L];
    [0L; 2L; 1L]
  ] in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    printf "is_sorted               %a -> %b\n"
      (xpp Uns.xpp) l
      (is_sorted l ~cmp:Uns.cmp)
    ;
    printf "is_sorted ~strict:false %a -> %b\n"
      (xpp Uns.xpp) l
      (is_sorted ~strict:false l ~cmp:Uns.cmp)
    ;
    printf "is_sorted ~strict:true  %a -> %b\n"
      (xpp Uns.xpp) l
      (is_sorted ~strict:true l ~cmp:Uns.cmp)
  );
  printf "@]"

let _ = test ()
