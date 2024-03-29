open! Basis.Rudiments
open! Basis
open U8

let test () =
  let fifteen = (kv 15L) in
  File.Fmt.stdout
  |> Fmt.fmt "max_value + "
  |> pp one
  |> Fmt.fmt " -> "
  |> fmt ~alt:true ~zpad:true ~width:2L ~radix:Radix.Hex ~pretty:true (max_value + one)
  |> Fmt.fmt "\nmin_value - "
  |> pp one
  |> Fmt.fmt " -> "
  |> fmt ~alt:true ~zpad:true ~width:2L ~radix:Radix.Hex ~pretty:true (min_value - one)
  |> Fmt.fmt "\nmax_value * "
  |> pp fifteen
  |> Fmt.fmt " -> "
  |> fmt ~alt:true ~zpad:true ~width:2L ~radix:Radix.Hex ~pretty:true (max_value * fifteen)
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
