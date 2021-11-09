open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  printf "@[<h>";
  let test list = begin
    printf "of_list[_rev] %a -> %a / %a\n"
      (List.xpp Uns.xpp) list
      (xpp Uns.xpp) (of_list list)
      (xpp Uns.xpp) (of_list_rev list);
  end in
  let lists = [
    [];
    [0L];
    [0L; 1L];
    [0L; 1L; 2L];
    [0L; 1L; 2L; 3L];
  ] in
  List.iter lists ~f:test;
  printf "@]"

let _ = test ()
