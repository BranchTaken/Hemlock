open! Basis.Rudiments
open! Basis
open Array

let test () =
  let test_fold_map uarr = begin
    let accum, sarr = fold_map uarr ~init:0L ~f:(fun accum elm ->
      (accum + elm), (Uns.to_string elm)
    ) in
    let accum2, sarr2 = foldi_map uarr ~init:0L ~f:(fun i accum elm ->
      (accum + i + elm),
      (String.Fmt.empty |> Fmt.fmt "[" |> Uns.pp i |> Fmt.fmt "]=" |> Uns.pp elm |> Fmt.to_string)
    ) in
    File.Fmt.stdout
    |> (pp Uns.pp) uarr
    |> Fmt.fmt " -> fold_map "
    |> Uns.pp accum
    |> Fmt.fmt " "
    |> (pp String.pp) sarr
    |> Fmt.fmt " -> foldi_map "
    |> Uns.pp accum2
    |> Fmt.fmt " "
    |> (pp String.pp) sarr2
    |> Fmt.fmt "\n"
    |> ignore
  end in
  test_fold_map [||];
  test_fold_map [|0L|];
  test_fold_map [|1L; 0L|];
  test_fold_map [|2L; 1L; 0L|]

let _ = test ()
