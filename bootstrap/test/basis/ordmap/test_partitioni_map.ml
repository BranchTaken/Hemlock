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
      match i % 2 = 0 with
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
  for n = 0 to 6 do
    let arr = Array.init n ~f:(fun i -> i * 10) in
    test arr
  done;
  printf "@]"

let _ = test ()
