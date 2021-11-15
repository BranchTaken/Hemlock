open! Basis.Rudiments
open! Basis
open Array

let test () =
  let test_zip arr0 arr1 = begin
    let arr0', arr1' = unzip (zip arr0 arr1) in
    File.Fmt.stdout
    |> (pp Uns.pp) arr0'
    |> Fmt.fmt " "
    |> (pp Uns.pp) arr1'
    |> Fmt.fmt "\n"
    |> ignore
  end in
  test_zip [||] [||];
  test_zip [|0L|] [|1L|];
  test_zip [|0L; 1L|] [|2L; 3L|];
  test_zip [|0L; 1L; 2L|] [|3L; 4L; 5L|]

let _ = test ()
