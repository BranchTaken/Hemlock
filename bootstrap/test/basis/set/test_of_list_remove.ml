open! Basis.Rudiments
open! Basis
open SetTest
open Set

let test () =
  let test m set descr = begin
    File.Fmt.stdout
    |> Fmt.fmt "--- "
    |> Fmt.fmt descr
    |> Fmt.fmt " ---\n"
    |> Fmt.fmt "remove "
    |> Uns.pp m
    |> Fmt.fmt "\n"
    |> fmt set
    |> Fmt.fmt " -> "
    |> fmt (remove m set)
    |> Fmt.fmt "\n"
    |> ignore
  end in
  let test_tuples = [
    ([0L; 1L], 2L,            "Not member, elm empty.");
    ([1L], 91L,               "Not member, elm of different value.");
    ([0L], 0L,                "Member, length 1 -> 0.");
    ([0L; 1L], 1L,            "Member, length 2 -> 1.");
    ([0L; 1L; 2L], 2L,        "Member, length 3 -> 2.");
    ([0L; 1L; 66L], 66L,      "Member, subnode elms 2 -> 1.");
    ([0L; 1L; 66L; 91L], 91L, "Member, subnode elms 3 -> 2.");
  ] in
  List.iter test_tuples ~f:(fun (ms, m, descr) ->
    let set = of_list (module Uns) ms in
    test m set descr
  )

let _ = test ()
