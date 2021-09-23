open! Basis.Rudiments
open! Basis
open! ListTest
open List
open Format

let test () =
  iter_oc 0L 4L (fun i ->
    printf "%a\n" (pp Uns.pp) (init i ~f:(fun j -> j));
  )

let _ = test ()
