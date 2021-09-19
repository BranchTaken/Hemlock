open! Basis.Rudiments
open! Basis
open OrdsetTest
open Ordset
open Format

let test () =
  let test_search ordset key_max = begin
    printf "%a@\n" pp ordset;
    for probe = 0 to key_max do
      printf "  %a -> %s, %s, %s@\n" Uns.pp probe
        (match psearch probe ordset with
          | None -> "<"
          | Some (Cmp.Lt, i) -> asprintf "<[%a]=%a"
              Uns.pp i Uns.pp (nth i ordset)
          | Some (Cmp.Eq, i) -> asprintf "=[%a]=%a"
              Uns.pp i Uns.pp (nth i ordset)
          | Some (Cmp.Gt, i) -> asprintf ">[%a]=%a"
              Uns.pp i Uns.pp (nth i ordset)
        )
        (match search probe ordset with
          | None -> "<>"
          | Some i -> asprintf "=%a" Uns.pp (nth i ordset)
        )
        (match nsearch probe ordset with
          | Some (Cmp.Lt, i) -> asprintf "<[%a]=%a"
              Uns.pp i Uns.pp (nth i ordset)
          | Some (Cmp.Eq, i) -> asprintf "=[%a]=%a"
              Uns.pp i Uns.pp (nth i ordset)
          | Some (Cmp.Gt, i) -> asprintf ">[%a]=%a"
              Uns.pp i Uns.pp (nth i ordset)
          | None -> ">"
        );
    done
  end in
  printf "@[";
  for len = 0 to 3 do
    let ordset = of_array (module Uns)
      (Array.init len ~f:(fun i -> i * 2 + 1)) in
    let key_max = len * 2 in
    test_search ordset key_max
  done;
  printf "@]"

let _ = test ()
