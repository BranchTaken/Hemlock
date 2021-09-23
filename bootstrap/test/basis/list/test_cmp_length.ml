open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let test_cmp_length lst0 lst1 = begin
    printf " -> %s\n" (match cmp_length lst0 lst1 with
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
        printf "cmp_length %a %a" (pp Uns.pp) list0 (pp Uns.pp) list1;
        test_cmp_length list0 list1;
        test_with_lists lists lists0 lists1'
      end
  end in
  let lists = [
    [];
    [0L];
    [0L; 1L];
    [0L; 1L; 2L]
  ] in
  printf "@[<h>";
  test_with_lists lists lists lists;
  printf "@]"

let _ = test ()
