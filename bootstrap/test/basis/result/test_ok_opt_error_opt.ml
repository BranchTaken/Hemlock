open! Basis.Rudiments
open! Basis
open Result

let test () =
  List.iter [Ok "ok"; Error "error"] ~f:(fun result ->
    File.Fmt.stdout
    |> Fmt.fmt "ok_opt "
    |> (pp String.pp String.pp) result
    |> Fmt.fmt " -> "
    |> (Option.fmt String.pp) (ok_opt result)
    |> Fmt.fmt "\nerror_opt "
    |> (pp String.pp String.pp) result
    |> Fmt.fmt " -> "
    |> (Option.fmt String.pp) (error_opt result)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
