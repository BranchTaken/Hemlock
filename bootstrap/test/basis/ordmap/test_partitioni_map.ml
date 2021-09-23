open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap
open Format

let test () =
  printf "@[<h>";
  let test arr = begin
    let ordmap = of_karray arr in
    let a_ordmap, b_ordmap = partitioni_map ordmap ~f:(fun i (_, v) ->
      match i % 2L = 0L with
      | true -> First (Uns.to_string v)
      | false -> Second (Uns.to_sint v)
    ) in
    let a_arr = to_array a_ordmap in
    let b_arr = to_array b_ordmap in
    printf "%a -> %a / %a@\n"
      (Array.pp Uns.pp) arr
      (Array.pp (pp_kv String.pp)) a_arr
      (Array.pp (pp_kv Sint.pp)) b_arr
  end in
  iter_oc 0L 7L (fun n ->
    let arr = Array.init n ~f:(fun i -> i * 10L) in
    test arr
  );
  printf "@]"

let _ = test ()
