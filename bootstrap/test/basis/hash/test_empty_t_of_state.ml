open! Basis.Rudiments
open! Basis
open Hash

let test () =
  File.Fmt.stdout
  |> Fmt.fmt "hash=" |> pp (t_of_state State.empty) |> Fmt.fmt "\n"
  |> Fmt.fmt "state=" |> State.pp State.empty |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
