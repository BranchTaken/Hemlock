open! Basis.Rudiments
open! Basis
open I8

let test () =
  File.Fmt.stdout
  |> Fmt.fmt "max_value + "
  |> pp one
  |> Fmt.fmt " -> "
  |> fmt ~alt:true ~zpad:true ~width:2L ~radix:Radix.Hex ~pretty:true (max_value + one)
  |> Fmt.fmt "\nmin_value - "
  |> pp one
  |> Fmt.fmt " -> "
  |> fmt ~alt:true ~zpad:true ~width:2L ~radix:Radix.Hex ~pretty:true (min_value - one)
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
