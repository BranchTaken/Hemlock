open! Basis.Rudiments
open! Basis
open Real

let test () =
  File.Fmt.stdout
  |> Fmt.fmt "one: "
  |> fmt ~alt:true ~base:Fmt.Hex one
  |> Fmt.fmt "\nneg_one: "
  |> fmt ~alt:true ~base:Fmt.Hex neg_one
  |> Fmt.fmt "\nnan: "
  |> fmt ~alt:true ~base:Fmt.Hex nan
  |> Fmt.fmt "\ninf: "
  |> fmt ~alt:true ~base:Fmt.Hex inf
  |> Fmt.fmt "\nneg_inf: "
  |> fmt ~alt:true ~base:Fmt.Hex neg_inf
  |> Fmt.fmt "\npi: "
  |> fmt ~alt:true ~base:Fmt.Hex pi
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
