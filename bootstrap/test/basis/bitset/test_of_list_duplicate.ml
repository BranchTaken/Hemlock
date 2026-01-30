open! Basis.Rudiments
open! Basis
open Bitset

let test () =
  File.Fmt.stdout
  |> fmt (of_list [0L; 0L])
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
