open! Basis.Rudiments
open! Basis
open Array

let test () =
  let test_is_sorted arr = begin
    File.Fmt.stdout
    |> Fmt.fmt "is_sorted "
    |> (pp Uns.pp) arr
    |> Fmt.fmt ": not strict -> "
    |> Bool.pp (is_sorted arr ~cmp:Uns.cmp)
    |> Fmt.fmt ", strict -> "
    |> Bool.pp (is_sorted ~strict:true arr ~cmp:Uns.cmp)
    |> Fmt.fmt "\n"
    |> ignore
  end in
  test_is_sorted [||];
  test_is_sorted [|0L|];
  test_is_sorted [|0L; 0L|];
  test_is_sorted [|0L; 1L|];
  test_is_sorted [|1L; 0L|];
  test_is_sorted [|0L; 1L; 1L|];
  test_is_sorted [|0L; 1L; 2L|];
  test_is_sorted [|0L; 2L; 1L|]

let _ = test ()
