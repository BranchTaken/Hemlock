open! Basis.Rudiments
open! Basis
open List

let test () =
  let l = [
    [0L; 1L];
    [2L; 3L];
  ] in

  List.fold [false; true] ~init:File.Fmt.stdout ~f:(fun formatter alt ->
    formatter
    |> Fmt.fmt "l = "
    |> fmt ~alt (fmt ~alt ~width:4L Uns.fmt) l
    |> Fmt.fmt "\n"
  )

let _ = test ()
