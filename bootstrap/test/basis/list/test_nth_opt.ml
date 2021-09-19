open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let l = [0; 1] in
  for i = 0 to 2 do
    match nth_opt i l with
    | None -> printf "%a -> None\n" Uns.pp i
    | Some x -> printf "%a -> Some %a\n" Uns.pp i Uns.pp x
  done

let _ = test ()
