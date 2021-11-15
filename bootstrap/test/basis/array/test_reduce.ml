open! Basis.Rudiments
open! Basis
open Array

let test () =
  let test_reduce arr ~f = begin
    File.Fmt.stdout
    |> Fmt.fmt "reduce "
    |> (pp Uns.pp) arr
    |> (fun formatter ->
      match reduce arr ~f with
      | None -> formatter |> Fmt.fmt " -> None\n"
      | Some x -> formatter |> Fmt.fmt " -> " |> Uns.pp x |> Fmt.fmt "\n"
    )
    |> ignore
  end in
  let f a b = (a + b) in
  test_reduce [||] ~f;
  test_reduce [|0L; 1L; 2L; 3L; 4L|] ~f

let _ = test ()
