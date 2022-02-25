open! Basis.Rudiments
open! Basis
open! OrdsetTest
open Ordset

let test () =
  let test arr = begin
    let ordset = of_array (module Uns) arr in
    let ordset' = filter ordset ~f:(fun mem -> mem % 2L = 0L) in
    File.Fmt.stdout
    |> fmt ordset
    |> Fmt.fmt " -> "
    |> fmt ordset'
    |> Fmt.fmt "\n"
    |> ignore
  end in
  Range.Uns.iter (0L =:< 7L) ~f:(fun n ->
    let arr = Array.init (0L =:< n) ~f:(fun i -> i) in
    test arr
  )

let _ = test ()
