open! Basis.Rudiments
open! Basis
open Option

let test () =
  List.iter [Some 42L; None] ~f:(fun o ->
    File.Fmt.stdout
    |> Fmt.fmt "some_or_thunk ~f "
    |> pp Uns.pp o
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) (some_or_thunk ~f:(fun () -> Some 43L) o)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
