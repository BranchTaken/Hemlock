open Basis

let () =
  File.Fmt.sink |> Fmt.fmt "sink\n" |> Fmt.flush |> ignore;
  File.Fmt.stdout |> Fmt.fmt "stdout\n" |> Fmt.flush |> ignore
