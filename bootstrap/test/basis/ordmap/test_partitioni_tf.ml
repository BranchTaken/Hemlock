open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap
open Format

let test () =
  printf "@[<h>";
  let test arr = begin
    let ordmap = of_karray arr in
    let t_ordmap, f_ordmap = partitioni_tf ordmap ~f:(fun i _kv -> i % 2 = 0) in
    let t_arr = to_array t_ordmap in
    let f_arr = to_array f_ordmap in
    printf "%a -> %a / %a@\n"
      (Array.pp Uns.pp) arr
      (Array.pp (pp_kv Uns.pp)) t_arr
      (Array.pp (pp_kv Uns.pp)) f_arr
  end in
  for n = 0 to 6 do
    let arr = Array.init n ~f:(fun i -> i * 10) in
    test arr
  done;
  printf "@]"

let _ = test ()
