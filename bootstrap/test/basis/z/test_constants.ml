open! Basis.Rudiments
open! Basis
open Z

let test () =
  File.Fmt.stdout
  |> Fmt.fmt "zero="
  |> pp zero
  |> Fmt.fmt "\none="
  |> pp one
  |> Fmt.fmt "\nneg_one="
  |> pp neg_one
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
