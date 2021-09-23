open! Basis.Rudiments
open! Basis
open! ListTest
open List
open Format

let test () =
  let l = [0L; 1L] in
  iter_oc 0L 3L (fun i ->
    match nth_opt i l with
    | None -> printf "%a -> None\n" Uns.pp i
    | Some x -> printf "%a -> Some %a\n" Uns.pp i Uns.pp x
  )

let _ = test ()
