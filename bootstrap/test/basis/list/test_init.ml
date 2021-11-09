open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  Range.iter (0L =:< 4L) ~f:(fun i ->
    Range.iter (0L =:< i) ~f:(fun j ->
      printf "@[<h>(%Lu .. %Lu) -> %a@\n@]" j i (xpp Uns.xpp) (init (j =:< i) ~f:(fun k -> k));
    )
  )

let _ = test ()
