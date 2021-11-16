open! Basis.Rudiments
open! Basis
open String

let test () =
  let test_strip ?drop s = begin
    File.Fmt.stdout
    |> Basis.Fmt.fmt "lstrip "
    |> pp s
    |> Basis.Fmt.fmt " -> "
    |> pp (lstrip ?drop s)
    |> Basis.Fmt.fmt "\nrstrip "
    |> pp s
    |> Basis.Fmt.fmt " -> "
    |> pp (rstrip ?drop s)
    |> Basis.Fmt.fmt "\nstrip "
    |> pp s
    |> Basis.Fmt.fmt " -> "
    |> pp (strip ?drop s)
    |> Basis.Fmt.fmt "\n"
    |> ignore
  end in
  test_strip "  a b c  ";
  test_strip ~drop:(fun codepoint ->
    Codepoint.(codepoint = (kv 0x5fL) (* '_' *))
  ) "_ a_b_c _"

let _ = test ()
