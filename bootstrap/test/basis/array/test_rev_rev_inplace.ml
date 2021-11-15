open! Basis.Rudiments
open! Basis
open Array

let test () =
  let test_rev arr = begin
    File.Fmt.stdout
    |> Fmt.fmt "rev "
    |> (pp Uns.pp) arr
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) (rev arr)
    |> Fmt.fmt " -> rev_inplace "
    |> (pp Uns.pp) arr
    |> Fmt.fmt " -> "
    |> ignore;
    rev_inplace arr;
    File.Fmt.stdout
    |> (pp Uns.pp) arr
    |> Fmt.fmt "\n"
    |> ignore
  end in
  test_rev [|0L|];
  test_rev [|0L; 1L|];
  test_rev [|0L; 1L; 2L|];
  test_rev [|0L; 1L; 2L; 3L|]

let _ = test ()
