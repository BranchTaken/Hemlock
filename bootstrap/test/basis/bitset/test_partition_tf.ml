open! Basis.Rudiments
open! Basis
open Bitset

let test () =
  let test arr = begin
    let bitset = of_array arr in
    let t_bitset, f_bitset = partition_tf bitset ~f:(fun mem -> mem % 2L = 0L) in
    let t_arr = to_array t_bitset in
    let f_arr = to_array f_bitset in
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
    let arr = Array.init (0L =:< n) ~f:(fun i -> i) in
    test arr
  )

let _ = test ()
