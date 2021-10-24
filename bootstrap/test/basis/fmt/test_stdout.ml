open! Basis.Rudiments
open! Basis
open Fmt

let test () =
  let stdout_a = File.Fmt.stdout in
  let stdout_b = File.Fmt.stdout in

  let _ = stdout_a |> fmt "1\n" in
  let _ = stdout_b |> fmt "2\n" in
  let _ = stdout_a |> fmt "3\n" in
  let _ = stdout_b |> fmt "4\n" in

  let _ = stdout_a |> fmt "5\n" |> flush in
  stdout_b |> fmt "6\n" |> flush

let _ = test ()
