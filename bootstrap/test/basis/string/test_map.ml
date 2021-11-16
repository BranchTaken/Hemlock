open! Basis.Rudiments
open! Basis
open String

let test () =
  let s = "abcde" in
  let s2 = "a:b:cd:e" in
  File.Fmt.stdout
  |> Basis.Fmt.fmt "map: "
  |> pp s
  |> Basis.Fmt.fmt " -> "
  |> pp (map s ~f:(fun cp -> Codepoint.trunc_of_uns ((Codepoint.extend_to_uns cp) - 32L)))
  |> Basis.Fmt.fmt "\nmapi: "
  |> pp s
  |> Basis.Fmt.fmt " -> "
  |> pp (mapi s ~f:(fun i cp ->
    match (bit_and i 0x1L) with
    | 0L -> cp
    | 1L -> Codepoint.trunc_of_uns ((Codepoint.extend_to_uns cp) - 32L)
    | _ -> not_reached ()
  ))
  |> Basis.Fmt.fmt "\ntr: "
  |> pp s2
  |> Basis.Fmt.fmt " -> "
  |> pp (tr s2 ~target:Codepoint.(of_char ':')
    ~replacement:Codepoint.(of_char ' '))
  |> Basis.Fmt.fmt "\nfilter: "
  |> pp s2
  |> Basis.Fmt.fmt " -> "
  |> pp (filter s2 ~f:(fun codepoint ->
    Codepoint.(codepoint <> (of_char ':'))
  ))
  |> Basis.Fmt.fmt "\n"
  |> ignore

let _ = test ()
