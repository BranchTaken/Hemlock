open! Basis.Rudiments
open! Basis
open Ordset

let test () =
  let test m ordset descr = begin
    let ordset' = remove m ordset in
    File.Fmt.stdout
    |> Fmt.fmt "--- "
    |> Fmt.fmt descr
    |> Fmt.fmt " ---\nremove "
    |> Uns.pp m
    |> Fmt.fmt "\n"
    |> fmt ordset
    |> Fmt.fmt " -> "
    |> fmt ordset'
    |> Fmt.fmt "\n"
    |> ignore
  end in
  let test_tuples = [
    ([0L; 1L], 2L,             "Not member.");
    ([0L], 0L,                 "Member, length 1 -> 0.");
    ([0L; 1L], 1L,             "Member, length 2 -> 1.");
    ([0L; 1L; 2L], 2L,         "Member, length 3 -> 2.");
  ] in
  List.iter test_tuples ~f:(fun (ms, m, descr) ->
    let ordset = of_list (module Uns) ms in
    test m ordset descr
  )

let _ = test ()
