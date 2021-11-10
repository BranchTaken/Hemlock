open! Basis.Rudiments
open! Basis
open Unit

let test () =
  File.Fmt.stdout
  |> Fmt.fmt "to_string () -> " |> Fmt.fmt (to_string ()) |> Fmt.fmt "\n"
  |> Fmt.fmt "of_string unit -> " |> Fmt.fmt (to_string (of_string "unit")) |> Fmt.fmt "\n"
  |> Fmt.fmt "of_string () -> " |> Fmt.fmt (to_string (of_string "()")) |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
