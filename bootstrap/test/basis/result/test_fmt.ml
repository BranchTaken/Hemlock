open! Basis.Rudiments
open! Basis
open Result

let test () =
  File.Fmt.stdout
  |> Fmt.fmt "Ok 42 -> "
  |> (fmt Uns.pp String.pp) (Ok 42L)
  |> Fmt.fmt "\nError \"bang\" -> "
  |> (fmt Uns.pp String.pp) (Error "bang")
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
