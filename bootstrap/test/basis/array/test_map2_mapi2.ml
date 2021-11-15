open! Basis.Rudiments
open! Basis
open Array

let test () =
  let test_map2 uarr0 uarr1 = begin
    let sarr = map2 uarr0 uarr1 ~f:(fun elm0 elm1 ->
      String.Fmt.empty
      |> Fmt.fmt "("
      |> Uns.pp elm0
      |> Fmt.fmt ","
      |> Uns.pp elm1
      |> Fmt.fmt ")"
      |> Fmt.to_string
    ) in
    let sarr2 = mapi2 uarr0 uarr1 ~f:(fun i elm0 elm1 ->
      String.Fmt.empty
      |> Fmt.fmt "["
      |> Uns.pp i
      |> Fmt.fmt "]=("
      |> Uns.pp elm0
      |> Fmt.fmt ","
      |> Uns.pp elm1
      |> Fmt.fmt ")"
      |> Fmt.to_string
    ) in
    File.Fmt.stdout
    |> (pp Uns.pp) uarr0
    |> Fmt.fmt " "
    |> (pp Uns.pp) uarr1
    |> Fmt.fmt " -> map2 "
    |> (pp String.pp) sarr
    |> Fmt.fmt " -> mapi2 "
    |> (pp String.pp) sarr2
    |> Fmt.fmt "\n"
    |> ignore
  end in
  test_map2 [||] [||];
  test_map2 [|1L|] [|0L|];
  test_map2 [|3L; 2L|] [|1L; 0L|];
  test_map2 [|5L; 4L; 3L|] [|2L; 1L; 0L|]

let _ = test ()
