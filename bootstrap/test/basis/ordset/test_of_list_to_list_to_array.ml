open! Basis.Rudiments
open! Basis
open! OrdsetTest
open Ordset

let test () =
  let test ms = begin
    let ordset = of_list (module Uns) ms in
    File.Fmt.stdout
    |> Fmt.fmt "of_list "
    |> (List.pp Uns.pp) ms
    |> Fmt.fmt "; to_list -> "
    |> (List.pp Uns.pp) (to_list ordset)
    |> Fmt.fmt "; to_array -> "
    |> (Array.pp Uns.pp) (to_array ordset)
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
