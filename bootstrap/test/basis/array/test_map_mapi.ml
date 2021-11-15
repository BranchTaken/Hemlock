open! Basis.Rudiments
open! Basis
open Array

let test () =
  let test_map uarr = begin
    let sarr = map uarr ~f:(fun elm -> String.Fmt.empty |> Uns.pp elm |> Fmt.to_string) in
    let sarr2 = mapi uarr ~f:(fun i elm ->
      String.Fmt.empty
      |> Fmt.fmt "["
      |> Uns.pp i
      |> Fmt.fmt "]="
      |> Uns.pp elm
      |> Fmt.to_string
    ) in
    File.Fmt.stdout
    |> (pp Uns.pp) uarr
    |> Fmt.fmt " -> map "
    |> (pp String.pp) sarr
    |> Fmt.fmt " -> mapi "
    |> (pp String.pp) sarr2
    |> Fmt.fmt "\n"
    |> ignore
  end in
  test_map [||];
  test_map [|0L|];
  test_map [|1L; 0L|];
  test_map [|2L; 1L; 0L|]

let _ = test ()
