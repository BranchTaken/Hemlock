open! Basis.Rudiments
open! Basis
open I256

let test () =
  let fifteen = of_string "15" in
  File.Fmt.stdout
  |> Fmt.fmt "neg_one + one -> "
  |> fmt ~alt:true ~zpad:true ~width:64L ~radix:Radix.Hex ~pretty:true (neg_one + one)
  |> Fmt.fmt "\nzero - one -> "
  |> fmt ~alt:true ~zpad:true ~width:64L ~radix:Radix.Hex ~pretty:true (zero - one)
  |> Fmt.fmt "\nneg_one * "
  |> pp fifteen
  |> Fmt.fmt " -> "
  |> fmt ~alt:true ~zpad:true ~width:64L ~radix:Radix.Hex ~pretty:true (neg_one * fifteen)
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
