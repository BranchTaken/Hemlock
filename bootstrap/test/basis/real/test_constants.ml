open! Basis.Rudiments
open! Basis
open Real

let test () =
  File.Fmt.stdout
  |> Fmt.fmt "one: "
  |> fmt ~alt:true ~radix:Radix.Hex one
  |> Fmt.fmt "\nneg_one: "
  |> fmt ~alt:true ~radix:Radix.Hex neg_one
  |> Fmt.fmt "\nnan: "
  |> fmt ~alt:true ~radix:Radix.Hex nan
  |> Fmt.fmt "\ninf: "
  |> fmt ~alt:true ~radix:Radix.Hex inf
  |> Fmt.fmt "\nneg_inf: "
  |> fmt ~alt:true ~radix:Radix.Hex neg_inf
  |> Fmt.fmt "\npi: "
  |> fmt ~alt:true ~radix:Radix.Hex pi
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
