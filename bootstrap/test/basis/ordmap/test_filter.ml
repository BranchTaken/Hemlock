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
      (Array.xpp Uns.xpp) arr
      (Array.xpp (xpp_kv Uns.xpp)) arr'
  end in
  Range.iter (0L =:< 7L) ~f:(fun n ->
    let arr = Array.init (0L =:< n) ~f:(fun i -> i) in
    test arr
  );
  printf "@]"

let _ = test ()
