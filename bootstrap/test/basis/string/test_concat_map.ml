open! Basis.Rudiments
open! Basis
open String

let test () =
  let s = "abcde\n" in
  File.Fmt.stdout
  |> fmt s
  |> fmt (concat_map s ~f:(fun cp -> of_codepoint cp))
  |> fmt (concat_map s ~f:(fun cp ->
    match cp with
    | cp when Codepoint.(cp = (kv 0x61L)) -> "hello "
    | cp when Codepoint.(cp = (kv 0x64L)) -> " there "
    | _ -> of_codepoint cp
  ))
  |> ignore

let _ = test ()
