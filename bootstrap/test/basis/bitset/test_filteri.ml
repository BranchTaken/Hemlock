open! Basis.Rudiments
open! Basis
open Bitset

let test () =
  let test arr = begin
    let bitset = of_array arr in
    let bitset' = filteri bitset ~f:(fun i _mem -> i % 2L = 0L) in
    File.Fmt.stdout
    |> fmt bitset
    |> Fmt.fmt " -> "
    |> fmt bitset'
    |> Fmt.fmt "\n"
    |> ignore
  end in
  Range.Uns.iter (0L =:< 7L) ~f:(fun n ->
    let arr = Array.init (0L =:< n) ~f:(fun i -> i * 10L) in
    test arr
  )

let _ = test ()
