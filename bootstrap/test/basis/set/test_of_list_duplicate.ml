open! Basis.Rudiments
open! Basis
open Set

let test () =
  File.Fmt.stdout
  |> fmt (of_list (module Uns) [0L; 0L])
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
