open! Basis.Rudiments
open! Basis
open String

let test () =
  let test_for s cp = begin
    let f codepoint = Codepoint.(codepoint = cp) in
    File.Fmt.stdout
    |> Basis.Fmt.fmt "for_any "
    |> pp s
    |> Basis.Fmt.fmt " "
    |> Codepoint.pp cp
    |> Basis.Fmt.fmt " -> "
    |> Bool.pp (for_any s ~f)
    |> Basis.Fmt.fmt "\nfor_all "
    |> pp s
    |> Basis.Fmt.fmt " "
    |> Codepoint.pp cp
    |> Basis.Fmt.fmt " -> "
    |> Bool.pp (for_all s ~f)
    |> Basis.Fmt.fmt "\nmem "
    |> pp s
    |> Basis.Fmt.fmt " "
    |> Codepoint.pp cp
    |> Basis.Fmt.fmt " -> "
    |> Bool.pp (mem cp s)
    |> Basis.Fmt.fmt "\n"
    |> ignore
  end in
  test_for "" Codepoint.(of_char 'a');
  test_for "abcde" Codepoint.(of_char 'a');
  test_for "abcde" Codepoint.(of_char 'b');
  test_for "abcde" Codepoint.(of_char 'f');
  test_for "fff" Codepoint.(of_char 'f')

let _ = test ()
