open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  printf "@[<h>";
  let test list = begin
    printf "of_list[_rev] %a -> %a / %a\n"
      (List.pp Uns.pp) list
      (pp Uns.pp) (of_list list)
      (pp Uns.pp) (of_list_rev list);
  end in
  let lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 2; 3];
  ] in
  List.iter lists ~f:test;
  printf "@]"

let _ = test ()
