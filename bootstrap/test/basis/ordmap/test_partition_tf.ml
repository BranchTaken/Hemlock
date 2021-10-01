open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap
open Format

let test () =
  printf "@[<h>";
  let test arr = begin
    let ordmap = of_karray arr in
    let t_ordmap, f_ordmap = partition_tf ordmap ~f:(fun (k, _) -> k % 2L = 0L) in
    let t_arr = to_array t_ordmap in
    let f_arr = to_array f_ordmap in
    printf "%a -> %a / %a@\n"
      (Array.pp Uns.pp) arr
      (Array.pp (pp_kv Uns.pp)) t_arr
      (Array.pp (pp_kv Uns.pp)) f_arr
  end in
  Range.iter (0L =:< 7L) ~f:(fun n ->
    let arr = Array.init (0L =:< n) ~f:(fun i -> i) in
    test arr
  );
  printf "@]"

let _ = test ()
