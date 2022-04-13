open! Basis.Rudiments
open! Basis
open Ordset

let test () =
  let test ms = begin
    let ordset = of_list (module Uns) ms in
    let sum = reduce ~f:( + ) ordset in
    File.Fmt.stdout
    |> Fmt.fmt "reduce ~f:( + ) "
    |> (List.pp Uns.pp) ms
    |> Fmt.fmt " -> "
    |> (Option.fmt Uns.pp) sum
    |> Fmt.fmt "\n"
    |> ignore
  end in
  let test_lists = [
    [];
    [0L];
    [0L; 1L];
    [0L; 1L; 2L];
    [0L; 1L; 66L];
    [0L; 1L; 66L; 91L];
  ] in
  List.iter test_lists ~f:(fun ms ->
    test ms
  )

let _ = test ()
