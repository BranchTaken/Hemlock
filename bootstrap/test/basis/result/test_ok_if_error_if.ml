open! Basis.Rudiments
open! Basis
open Result

let test () =
  List.iter [true; false] ~f:(fun b ->
    File.Fmt.stdout
    |> Fmt.fmt "ok_if "
    |> Bool.pp b
    |> Fmt.fmt " -> "
    |> (pp Unit.pp String.pp) (ok_if b ~error:"oops")
    |> Fmt.fmt "\nerror_if "
    |> Fmt.fmt " -> "
    |> (pp String.pp Unit.pp) (error_if b ~ok:"whew")
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
