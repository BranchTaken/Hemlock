open! Basis.Rudiments
open! Basis
open Ordset

let test () =
  let test ms = begin
    let ordset = of_array (module Uns) ms in
    File.Fmt.stdout
    |> Fmt.fmt "of_array "
    |> (Array.pp Uns.pp) ms
    |> Fmt.fmt " -> "
    |> fmt ordset
    |> Fmt.fmt "\n"
    |> ignore
  end in
  let test_arrays = [
    [||];
    [|0L; 1L; 4L; 5L; 3L; 2L|];
  ] in
  List.iter test_arrays ~f:(fun ms ->
    test ms
  )

let _ = test ()
