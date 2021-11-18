open! Basis.Rudiments
open! Basis
open U64

let test () =
  let fifteen = of_string "15" in
  File.Fmt.stdout
  |> Fmt.fmt "max_value + "
  |> pp one
  |> Fmt.fmt " -> "
  |> fmt ~alt:true ~zpad:true ~width:16L ~base:Fmt.Hex (max_value + one)
  |> Fmt.fmt "\nmin_value - "
  |> pp one
  |> Fmt.fmt " -> "
  |> fmt ~alt:true ~zpad:true ~width:16L ~base:Fmt.Hex (min_value - one)
  |> Fmt.fmt "\nmax_value * "
  |> pp fifteen
  |> Fmt.fmt " -> "
  |> fmt ~alt:true ~zpad:true ~width:16L ~base:Fmt.Hex (max_value * fifteen)
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
