open! Basis.Rudiments
open! Basis
open U128

let test () =
  File.Fmt.stdout
  |> Fmt.fmt "zero="
  |> fmt ~alt:true ~zpad:true ~width:32L ~base:Fmt.Hex ~pretty:true zero
  |> Fmt.fmt "\none="
  |> fmt ~alt:true ~zpad:true ~width:32L ~base:Fmt.Hex ~pretty:true one
  |> Fmt.fmt "\nmin_value="
  |> fmt ~alt:true ~zpad:true ~width:32L ~base:Fmt.Hex ~pretty:true min_value
  |> Fmt.fmt "\nmax_value="
  |> fmt ~alt:true ~zpad:true ~width:32L ~base:Fmt.Hex ~pretty:true max_value
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
