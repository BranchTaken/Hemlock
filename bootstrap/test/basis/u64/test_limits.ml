open! Basis.Rudiments
open! Basis
open U64

let test () =
  File.Fmt.stdout
  |> Fmt.fmt "min_value="
  |> fmt ~alt:true ~zpad:true ~width:16L ~radix:Radix.Hex min_value
  |> Fmt.fmt "\nmax_value="
  |> fmt ~alt:true ~zpad:true ~width:16L ~radix:Radix.Hex max_value
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
