open! Basis.Rudiments
open! Basis
open Result

let test () =
  List.iter [Ok "ok"; Error "error"] ~f:(fun result ->
    File.Fmt.stdout
    |> Fmt.fmt "is_ok "
    |> (pp String.pp String.pp) result
    |> Fmt.fmt " -> "
    |> Bool.pp (is_ok result)
    |> Fmt.fmt "\nis_error "
    |> (pp String.pp String.pp) result
    |> Fmt.fmt " -> "
    |> Bool.pp (is_error result)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
