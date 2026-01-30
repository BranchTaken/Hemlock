open! Basis.Rudiments
open! Basis
open Bitset

let test () =
  let test m bitset descr = begin
    let bitset' = remove m bitset in
    File.Fmt.stdout
    |> Fmt.fmt "--- "
    |> Fmt.fmt descr
    |> Fmt.fmt " ---\nremove "
    |> Uns.pp m
    |> Fmt.fmt "\n"
    |> fmt bitset
    |> Fmt.fmt " -> "
    |> fmt bitset'
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
    let bitset = of_list ms in
    test m bitset descr
  )

let _ = test ()
