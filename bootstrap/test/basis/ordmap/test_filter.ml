open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap
open Format

let test () =
  printf "@[<h>";
  let test arr = begin
    let ordmap = of_karray arr in
    let ordmap' = filter ordmap ~f:(fun (k, _) -> k % 2L = 0L) in
    let arr' = to_array ordmap' in
    printf "%a -> %a@\n"
      (Array.pp Uns.pp) arr
      (Array.pp (pp_kv Uns.pp)) arr'
  end in
  iter_oc 0L 7L (fun n ->
    let arr = Array.init n ~f:(fun i -> i) in
    test arr
  );
  printf "@]"

let _ = test ()
