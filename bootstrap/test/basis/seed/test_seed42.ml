open Basis

let () =
  File.Fmt.stdout
  |> Fmt.fmt "HEMLOCK_ENTROPY="
  |> String.pp (Sys.getenv "HEMLOCK_ENTROPY")
  |> Fmt.fmt " -> seed="
  |> Hash.State.pp Hash.State.seed
  |> Fmt.fmt "\n"
  |> ignore
