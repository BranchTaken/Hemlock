open! Basis.Rudiments
open! Basis
open Option

let test () =
  List.iter [Some 42L; None] ~f:(fun o ->
    File.Fmt.stdout
    |> Fmt.fmt "is_some "
    |> (pp Uns.pp) o
    |> Fmt.fmt " -> "
    |> Bool.pp (is_some o)
    |> Fmt.fmt "\nis_none "
    |> (pp Uns.pp) o
    |> Fmt.fmt " -> "
    |> Bool.pp (is_none o)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
