open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  for i = 0 to 3 do
    printf "%a\n" (pp Uns.pp) (init i ~f:(fun j -> j));
  done

let _ = test ()
