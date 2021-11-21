open! Basis.Rudiments
open! Basis
open Option

let test () =
  File.Fmt.stdout
  |> Fmt.fmt "Some 42 -> "
  |> (fmt Uns.pp) (Some 42L)
  |> Fmt.fmt "\nNone -> "
  |> (fmt Uns.pp) None
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
