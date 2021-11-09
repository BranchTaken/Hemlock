open! Basis.Rudiments
open! Basis
open Fmt

let test () =
  File.Fmt.stdout
  |> fmt "hello\n"

  |> fmt "[" |> xfmt ~just:Left "left" |> fmt "]\n"
  |> fmt "[" |> xfmt ~just:Center "center" |> fmt "]\n"
  |> fmt "[" |> xfmt ~just:Right "right" |> fmt "]\n"

  |> fmt "[" |> xfmt ~just:Left ~width:15L "left" |> fmt "]\n"
  |> fmt "[" |> xfmt ~just:Center ~width:15L "center" |> fmt "]\n"
  |> fmt "[" |> xfmt ~just:Right ~width:15L "right" |> fmt "]\n"

  |> fmt "[" |> xfmt ~pad:"_" ~just:Left ~width:15L "left" |> fmt "]\n"
  |> fmt "[" |> xfmt ~pad:"_" ~just:Center ~width:15L "center" |> fmt "]\n"
  |> fmt "[" |> xfmt ~pad:"_" ~just:Right ~width:15L "right" |> fmt "]\n"

let _ = test ()
