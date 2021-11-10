open! Basis.Rudiments
open! Basis
open Fmt

let test () =
  let stdout_a = File.Fmt.stdout in
  let stdout_b = File.Fmt.stdout in

  stdout_a |> fmt "1\n" |> ignore;
  stdout_b |> fmt "2\n" |> ignore;
  stdout_a |> fmt "3\n" |> ignore;
  stdout_b |> fmt "4\n" |> ignore;

  stdout_a |> fmt "5\n" |> flush |> ignore;
  stdout_b |> fmt "6\n" |> flush

let _ = test ()
