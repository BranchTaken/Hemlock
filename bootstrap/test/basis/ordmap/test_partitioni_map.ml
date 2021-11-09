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
      | false -> Second (Uns.bits_to_sint v)
    ) in
    let a_arr = to_array a_ordmap in
    let b_arr = to_array b_ordmap in
    printf "%a -> %a / %a@\n"
      (Array.xpp Uns.xpp) arr
      (Array.xpp (xpp_kv String.xpp)) a_arr
      (Array.xpp (xpp_kv Sint.xpp)) b_arr
  end in
  Range.iter (0L =:< 7L) ~f:(fun n ->
    let arr = Array.init (0L =:< n) ~f:(fun i -> i * 10L) in
    test arr
  );
  printf "@]"

let _ = test ()
