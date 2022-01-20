open! Basis.Rudiments
open! Basis
open I256

let test () =
  File.Fmt.stdout
  |> Fmt.fmt "zero="
  |> fmt ~alt:true ~zpad:true ~width:64L ~radix:Radix.Hex ~pretty:true zero
  |> Fmt.fmt " "
  |> pp zero
  |> Fmt.fmt "\none="
  |> fmt ~alt:true ~zpad:true ~width:64L ~radix:Radix.Hex ~pretty:true one
  |> Fmt.fmt " "
  |> pp one
  |> Fmt.fmt "\nmin_value="
  |> fmt ~alt:true ~zpad:true ~width:64L ~radix:Radix.Hex ~pretty:true min_value
  |> Fmt.fmt " "
  |> pp min_value
  |> Fmt.fmt "\nmax_value="
  |> fmt ~alt:true ~zpad:true ~width:64L ~radix:Radix.Hex ~pretty:true max_value
  |> Fmt.fmt " "
  |> pp max_value
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
