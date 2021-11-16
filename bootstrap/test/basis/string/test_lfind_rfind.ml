open! Basis.Rudiments
open! Basis
open String

let test () =
  let test_find s cp = begin
    File.Fmt.stdout
    |> Basis.Fmt.fmt "lfind "
    |> pp s
    |> Basis.Fmt.fmt " "
    |> Codepoint.pp cp
    |> Basis.Fmt.fmt " -> "
    |> Basis.Fmt.fmt (match lfind cp s with
      | None -> "<not found>"
      | Some cursor -> Uns.to_string (C.Cursor.bindex cursor)
    )
    |> Basis.Fmt.fmt "\ncontains "
    |> pp s
    |> Basis.Fmt.fmt " "
    |> Codepoint.pp cp
    |> Basis.Fmt.fmt " -> "
    |> Bool.pp (contains cp s)
    |> Basis.Fmt.fmt "\nrfind "
    |> pp s
    |> Basis.Fmt.fmt " "
    |> Codepoint.pp cp
    |> Basis.Fmt.fmt " -> "
    |> Basis.Fmt.fmt (match rfind cp s with
      | None -> "<not found>"
      | Some cursor -> Uns.to_string (C.Cursor.bindex cursor)
    )
    |> Basis.Fmt.fmt "\n"
    |> ignore
  end in
  test_find "" Codepoint.(of_char 'b');
  test_find "abcba" Codepoint.(of_char 'a');
  test_find "abcba" Codepoint.(of_char 'b');
  test_find "abcba" Codepoint.(of_char 'c');
  test_find "abcba" Codepoint.(of_char 'd')

let _ = test ()
