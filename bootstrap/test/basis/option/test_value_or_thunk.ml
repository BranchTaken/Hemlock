open! Basis.Rudiments
open! Basis
open Option

let test () =
  List.iter [Some 42L; None] ~f:(fun o ->
    File.Fmt.stdout
    |> Fmt.fmt "value_or_thunk ~f "
    |> pp Uns.pp o
    |> Fmt.fmt " -> "
    |> Uns.pp (value_or_thunk ~f:(fun () -> 43L) o)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
