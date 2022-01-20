open! Basis.Rudiments
open! Basis
open I64

let test () =
  File.Fmt.stdout
  |> Fmt.fmt "min_value="
  |> fmt ~alt:true ~zpad:true ~width:16L ~radix:Radix.Hex ~pretty:true min_value
  |> Fmt.fmt "\nmax_value="
  |> fmt ~alt:true ~zpad:true ~width:16L ~radix:Radix.Hex ~pretty:true max_value
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
