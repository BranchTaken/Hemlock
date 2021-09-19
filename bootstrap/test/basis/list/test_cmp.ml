open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let test_cmp lst0 lst1 = begin
    printf " -> %s\n" (match cmp Uns.cmp lst0 lst1 with
      | Cmp.Lt -> "Lt"
      | Cmp.Eq -> "Eq"
      | Cmp.Gt -> "Gt"
    )
  end in
  let rec test_with_lists lists lists0 lists1 = begin
    match lists0, lists1 with
    | [], _ -> ()
    | _ :: lists0', [] -> test_with_lists lists lists0' lists
    | list0 :: _, list1 :: lists1' -> begin
        printf "cmp %a %a -> " (pp Uns.pp) list0 (pp Uns.pp) list1;
        test_cmp list0 list1;
        test_with_lists lists lists0 lists1'
      end
  end in
  let lists = [
    [];
    [0];
    [1];
    [0; 1];
    [0; 2];
    [1; 2]
  ] in
  printf "@[<h>";
  test_with_lists lists lists lists;
  printf "@]"

let _ = test ()
