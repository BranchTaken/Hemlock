open! Basis.Rudiments
open! Basis
open Set

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
    let set0 = of_list (module Uns) ms0 in
    let set1 = of_list (module Uns) ms1 in
    let pairs = fold2 ~init:[] ~f:(fun accum a0_opt a1_opt ->
      (a0_opt, a1_opt) :: accum
    ) set0 set1 in
    let pairs_sorted = List.sort ~cmp:(fun pair0 pair1 ->
      let a_of_pair = function
        | Some a, _
        | _, Some a -> a
        | None, None -> not_reached ()
      in
      let a0 = a_of_pair pair0 in
      let a1 = a_of_pair pair1 in
      Uns.cmp a0 a1
    ) pairs in
    File.Fmt.stdout
    |> Fmt.fmt "fold2 "
    |> (List.pp Uns.pp) ms0
    |> Fmt.fmt " "
    |> (List.pp Uns.pp) ms1
    |> Fmt.fmt " -> "
    |> (List.pp pp_pair) pairs_sorted
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
