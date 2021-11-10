open! Basis.Rudiments
open! Basis
open Array

let test () =
  let test arr0 arr1 = begin
    File.Fmt.stdout
    |> Fmt.fmt "concat "
    |> (fmt Uns.fmt) arr0
    |> Fmt.fmt " "
    |> (fmt Uns.fmt) arr1
    |> Fmt.fmt " -> "
    |> (fmt Uns.fmt) (concat arr0 arr1)
    |> Fmt.fmt "\n"
    |> ignore
  end in
  test [||] [||];
  test [|0L|] [||];
  test [||] [|0L|];
  test [|0L|] [|1L|];
  test [|0L; 1L|] [|2L|];
  test [|0L|] [|1L; 2L|];
  test [|0L; 1L|] [|2L; 3L|]

let _ = test ()
