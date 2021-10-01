open! Basis.Rudiments
open! Basis
open OrdsetTest
open Ordset
open Format

let test () =
  let test_search ordset key_max = begin
    printf "%a@\n" pp ordset;
    RangeF.Uns.(iter (0L =:= key_max)) ~f:(fun probe ->
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
    )
  end in
  printf "@[";
  Range.iter (0L =:< 4L) ~f:(fun len ->
    let ordset = of_array (module Uns)
      (Array.init (0L =:< len) ~f:(fun i -> i * 2L + 1L)) in
    let key_max = len * 2L in
    test_search ordset key_max
  );
  printf "@]"

let _ = test ()
