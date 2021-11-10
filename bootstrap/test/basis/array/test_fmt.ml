open! Basis.Rudiments
open! Basis
open Array

let test () =
  let a = [|
    [|0L; 1L|];
    [|2L; 3L|];
  |] in

  List.fold [false; true] ~init:File.Fmt.stdout ~f:(fun formatter alt ->
    formatter
    |> Fmt.fmt "a = "
    |> fmt ~alt (fmt ~alt ~width:4L Uns.fmt) a
    |> Fmt.fmt "\n"
  )

let _ = test ()
