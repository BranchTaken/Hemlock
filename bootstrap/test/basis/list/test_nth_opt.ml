open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let l = [0L; 1L] in
  Range.iter (0L =:< 3L) ~f:(fun i ->
    match nth_opt i l with
    | None -> printf "%a -> None\n" Uns.xpp i
    | Some x -> printf "%a -> Some %a\n" Uns.xpp i Uns.xpp x
  )

let _ = test ()
