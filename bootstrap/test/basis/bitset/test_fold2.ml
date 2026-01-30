open! Basis.Rudiments
open! Basis
open Bitset

let test () =
  let pp_pair (a0_opt, a1_opt) formatter = begin
    formatter
    |> Fmt.fmt "("
    |> (Option.fmt Uns.pp) a0_opt
    |> Fmt.fmt ", "
    |> (Option.fmt Uns.pp) a1_opt
    |> Fmt.fmt ")"
  end in
  let test ms0 ms1 = begin
    let bitset0 = of_list ms0 in
    let bitset1 = of_list ms1 in
    let pairs = fold2 ~init:[] ~f:(fun accum a0_opt a1_opt ->
      (a0_opt, a1_opt) :: accum
    ) bitset0 bitset1 in
    File.Fmt.stdout
    |> Fmt.fmt "fold2 "
    |> (List.pp Uns.pp) ms0
    |> Fmt.fmt " "
    |> (List.pp Uns.pp) ms1
    |> Fmt.fmt " -> "
    |> (List.pp pp_pair) pairs
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
  List.iteri test_lists ~f:(fun i ms0 ->
    List.iteri test_lists ~f:(fun j ms1 ->
      if i <= j then test ms0 ms1
    )
  )

let _ = test ()
