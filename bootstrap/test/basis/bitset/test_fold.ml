open! Basis.Rudiments
open! Basis
open Bitset

let test () =
  let test ms = begin
    let bitset = of_list ms in
    File.Fmt.stdout
    |> Fmt.fmt "bitset=" |> Bitset.pp bitset
    |> Fmt.fmt "\n     ->" |> ignore;
    let () = fold ~init:() ~f:(fun _accum elm ->
      File.Fmt.stdout
      |> Fmt.fmt " " |> Uns.pp elm |> ignore
    ) bitset in
    File.Fmt.stdout
    |> Fmt.fmt "\n     ->" |> ignore;
    let () = fold_right ~init:() ~f:(fun _accum elm ->
      File.Fmt.stdout
      |> Fmt.fmt " " |> Uns.pp elm |> ignore
    ) bitset in
    File.Fmt.stdout |> Fmt.fmt "\n" |> ignore
  end in
  let test_lists = [
    [];
    [0L];
    [0L; 1L];
    [0L; 1L; 2L];
    [0L; 1L; 62L; 63L];
    [0L; 1L; 66L];
    [0L; 1L; 66L; 91L];
  ] in
  List.iter test_lists ~f:(fun ms ->
    test ms
  )

let _ = test ()
