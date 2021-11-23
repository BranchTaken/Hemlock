open! Basis.Rudiments
open! Basis
open Option

let test () =
  List.iter [Some 42L; None] ~f:(fun o ->
    File.Fmt.stdout
    |> Fmt.fmt "map ("
    |> (pp Uns.pp) o
    |> Fmt.fmt ") ~f:(fun u -> Uns.to_string u) -> "
    |> (pp String.pp) (map o ~f:(fun u -> Uns.to_string u))
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
