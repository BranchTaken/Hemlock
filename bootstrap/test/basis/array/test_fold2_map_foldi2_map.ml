open! Basis.Rudiments
open! Basis
open Array

let test () =
  let test_fold2_map uarr0 uarr1 = begin
    let accum, sarr = fold2_map uarr0 uarr1 ~init:0L
        ~f:(fun accum elm0 elm1 ->
          (accum + elm0 + elm1),
          (String.Fmt.empty |> Fmt.fmt "(" |> Uns.pp elm0 |> Fmt.fmt "," |> Uns.pp elm1
           |> Fmt.fmt ")" |> Fmt.to_string)
        ) in
    let accum2, sarr2 = foldi2_map uarr0 uarr1 ~init:0L
        ~f:(fun i accum elm0 elm1 ->
          (accum + i + elm0 + elm1),
          (String.Fmt.empty |> Fmt.fmt "[" |> Uns.pp i |> Fmt.fmt "]=(" |> Uns.pp elm0
           |> Fmt.fmt "," |> Uns.pp elm1 |> Fmt.fmt ")" |> Fmt.to_string)
        ) in
    let _ =
      File.Fmt.stdout
      |> (pp Uns.pp) uarr0
      |> Fmt.fmt " "
      |> (pp Uns.pp) uarr1
      |> Fmt.fmt " -> fold2_map "
      |> Uns.pp accum
      |> Fmt.fmt " "
      |> (pp String.pp) sarr
      |> Fmt.fmt " -> foldi2_map "
      |> Uns.pp accum2
      |> Fmt.fmt " "
      |> (pp String.pp) sarr2
      |> Fmt.fmt "\n"
    in
    ()
  end in
  test_fold2_map [||] [||];
  test_fold2_map [|1L|] [|0L|];
  test_fold2_map [|3L; 2L|] [|1L; 0L|];
  test_fold2_map [|5L; 4L; 3L|] [|2L; 1L; 0L|]

let _ = test ()
