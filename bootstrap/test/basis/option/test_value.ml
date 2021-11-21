open! Basis.Rudiments
open! Basis
open Option

let test () =
  List.iter [Some 42L; None] ~f:(fun o ->
    File.Fmt.stdout
    |> Fmt.fmt "value "
    |> (pp Uns.pp) o
    |> Fmt.fmt " -> "
    |> Uns.pp (value ~default:13L o)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
