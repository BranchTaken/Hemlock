open! Basis.Rudiments
open! Basis
open Ordset

let test () =
  let test arr = begin
    let ordset = of_array (module Uns) arr in
    let t_ordset, f_ordset = partitioni_tf ordset ~f:(fun i _mem -> i % 2L = 0L) in
    let t_arr = to_array t_ordset in
    let f_arr = to_array f_ordset in
    File.Fmt.stdout
    |> (Array.pp Uns.pp) arr
    |> Fmt.fmt " -> "
    |> (Array.pp Uns.pp) t_arr
    |> Fmt.fmt " / "
    |> (Array.pp Uns.pp) f_arr
    |> Fmt.fmt "\n"
    |> ignore
  end in
  Range.Uns.iter (0L =:< 7L) ~f:(fun n ->
    let arr = Array.init (0L =:< n) ~f:(fun i -> i * 10L) in
    test arr
  )

let _ = test ()
