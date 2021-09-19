open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap
open Format

let test () =
  printf "@[<h>";
  let test arr = begin
    let ordmap = of_karray arr in
    let ordmap' = filter ordmap ~f:(fun (k, _) -> k % 2 = 0) in
    let arr' = to_array ordmap' in
    printf "%a -> %a@\n"
      (Array.pp Uns.pp) arr
      (Array.pp (pp_kv Uns.pp)) arr'
  end in
  for n = 0 to 6 do
    let arr = Array.init n ~f:(fun i -> i) in
    test arr
  done;
  printf "@]"

let _ = test ()
