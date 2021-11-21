open! Basis.Rudiments
open! Basis
open Real

let test () =
  File.Fmt.stdout
  |> Fmt.fmt "min_value: "
  |> pp min_value
  |> Fmt.fmt "\nmax_value: "
  |> pp max_value
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
