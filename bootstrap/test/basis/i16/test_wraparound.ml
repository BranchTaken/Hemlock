open! Basis.Rudiments
open! Basis
open I16

let test () =
  File.Fmt.stdout
  |> Fmt.fmt "max_value + "
  |> pp one
  |> Fmt.fmt " -> "
  |> fmt ~alt:true ~zpad:true ~width:4L ~base:Fmt.Hex ~pretty:true (max_value + one)
  |> Fmt.fmt "\nmin_value - "
  |> pp one
  |> Fmt.fmt " -> "
  |> fmt ~alt:true ~zpad:true ~width:4L ~base:Fmt.Hex ~pretty:true (min_value - one)
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
