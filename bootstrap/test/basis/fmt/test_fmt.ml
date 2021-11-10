open! Basis.Rudiments
open! Basis
open Fmt

let test () =
  File.Fmt.stdout
  |> fmt "hello\n"

  |> fmt "[" |> fmt ~just:Left "left" |> fmt "]\n"
  |> fmt "[" |> fmt ~just:Center "center" |> fmt "]\n"
  |> fmt "[" |> fmt ~just:Right "right" |> fmt "]\n"

  |> fmt "[" |> fmt ~just:Left ~width:15L "left" |> fmt "]\n"
  |> fmt "[" |> fmt ~just:Center ~width:15L "center" |> fmt "]\n"
  |> fmt "[" |> fmt ~just:Right ~width:15L "right" |> fmt "]\n"

  |> fmt "[" |> fmt ~pad:"_" ~just:Left ~width:15L "left" |> fmt "]\n"
  |> fmt "[" |> fmt ~pad:"_" ~just:Center ~width:15L "center" |> fmt "]\n"
  |> fmt "[" |> fmt ~pad:"_" ~just:Right ~width:15L "right" |> fmt "]\n"

let _ = test ()
