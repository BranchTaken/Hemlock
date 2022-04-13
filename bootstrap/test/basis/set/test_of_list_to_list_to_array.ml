open! Basis.Rudiments
open! Basis
open Set

let test () =
  let test ms = begin
    let set = of_list (module Uns) ms in
    let list_sorted = List.sort ~cmp:Uns.cmp (to_list set) in
    let array_sorted = Array.sort ~cmp:Uns.cmp (to_array set) in
    File.Fmt.stdout
    |> Fmt.fmt "of_list "
    |> (List.pp Uns.pp) ms
    |> Fmt.fmt "; to_list -> "
    |> (List.pp Uns.pp) list_sorted
    |> Fmt.fmt "; to_array -> "
    |> (Array.pp Uns.pp) array_sorted
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
