open! Basis.Rudiments
open! Basis
open Zint

let test () =
  let u64_max = of_u64 U64.max_value in
  let fifteen = of_string "15" in
  File.Fmt.stdout
  |> Fmt.fmt "u64_max -> "
  |> fmt ~alt:true ~base:Fmt.Hex ~pretty:true u64_max
  |> Fmt.fmt "\n"
  |> ignore;

  let r = (u64_max + one) in
  File.Fmt.stdout
  |> Fmt.fmt "u64_max + "
  |> pp one
  |> Fmt.fmt " -> "
  |> fmt ~alt:true ~base:Fmt.Hex ~pretty:true r
  |> Fmt.fmt " "
  |> pp r
  |> Fmt.fmt "\n"
  |> ignore;

  let r = (zero - one) in
  File.Fmt.stdout
  |> pp zero
  |> Fmt.fmt " - "
  |> pp one
  |> Fmt.fmt " -> "
  |> fmt ~alt:true ~base:Fmt.Hex ~pretty:true r
  |> Fmt.fmt " "
  |> pp r
  |> Fmt.fmt "\n"
  |> ignore;

  let r = (u64_max * fifteen) in
  File.Fmt.stdout
  |> Fmt.fmt "u64_max * "
  |> pp fifteen
  |> Fmt.fmt " -> "
  |> fmt ~alt:true ~base:Fmt.Hex ~pretty:true r
  |> Fmt.fmt " "
  |> pp r
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
