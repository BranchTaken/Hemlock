open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap
open Format

let test () =
  let test_search ordmap key_max = begin
    printf "%a@\n" (pp Uns.pp) ordmap;
    for probe = 0 to key_max do
      printf "  %a -> %s, %s, %s@\n" Uns.pp probe
        (match psearch probe ordmap with
          | None -> "<"
          | Some (Cmp.Lt, i) -> asprintf "<[%a]=%a"
              Uns.pp i (pp_kv Uns.pp) (nth i ordmap)
          | Some (Cmp.Eq, i) -> asprintf "=[%a]=%a"
              Uns.pp i (pp_kv Uns.pp) (nth i ordmap)
          | Some (Cmp.Gt, i) -> asprintf ">[%a]=%a"
              Uns.pp i (pp_kv Uns.pp) (nth i ordmap)
        )
        (match search probe ordmap with
          | None -> "<>"
          | Some i -> asprintf "=%a" (pp_kv Uns.pp) (nth i ordmap)
        )
        (match nsearch probe ordmap with
          | Some (Cmp.Lt, i) -> asprintf "<[%a]=%a"
              Uns.pp i (pp_kv Uns.pp) (nth i ordmap)
          | Some (Cmp.Eq, i) -> asprintf "=[%a]=%a"
              Uns.pp i (pp_kv Uns.pp) (nth i ordmap)
          | Some (Cmp.Gt, i) -> asprintf ">[%a]=%a"
              Uns.pp i (pp_kv Uns.pp) (nth i ordmap)
          | None -> ">"
        );
    done
  end in
  printf "@[";
  for len = 0 to 3 do
    let ordmap = of_array (module Uns)
      (Array.init len ~f:(fun i -> let k = (i * 2 + 1) in k, k * 10)) in
    let key_max = len * 2 in
    test_search ordmap key_max
  done;
  printf "@]"

let _ = test ()
