open! Basis.Rudiments
open! Basis
open Array

let test () =
  let test_length arr = begin
    File.Fmt.stdout
    |> (pp Uns.pp) arr
    |> Fmt.fmt ": length="
    |> Uns.pp (length arr)
    |> Fmt.fmt ", is_empty="
    |> Bool.pp (is_empty arr)
    |> Fmt.fmt "\n"
    |> ignore
  end in
  test_length [||];
  test_length [|0L|];
  test_length [|0L; 1L|];
  test_length [|0L; 1L; 2L|]

let _ = test ()
