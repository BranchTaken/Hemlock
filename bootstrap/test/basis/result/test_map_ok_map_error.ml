open! Basis.Rudiments
open! Basis
open Result

let test () =
  let f msg = String.Fmt.empty |> Fmt.fmt msg |> Fmt.fmt "'" |> Fmt.to_string in
  List.iter [Ok "ok"; Error "error"] ~f:(fun result ->
    File.Fmt.stdout
    |> Fmt.fmt "map_ok "
    |> (pp String.pp String.pp) result
    |> Fmt.fmt " -> "
    |> (pp String.pp String.pp) (map_ok result ~f)
    |> Fmt.fmt "\nmap_error "
    |> (pp String.pp String.pp) result
    |> Fmt.fmt " -> "
    |> (pp String.pp String.pp) (map_error result ~f)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
