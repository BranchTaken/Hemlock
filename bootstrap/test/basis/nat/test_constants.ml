open! Basis.Rudiments
open! Basis
open Nat

let test () =
  File.Fmt.stdout
  |> Fmt.fmt "zero="
  |> fmt ~alt:true ~radix:Radix.Hex zero
  |> Fmt.fmt "\none="
  |> fmt ~alt:true ~radix:Radix.Hex one
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
