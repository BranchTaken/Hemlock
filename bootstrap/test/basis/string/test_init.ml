open! Basis.Rudiments
open! Basis
open String

let test () =
  let codepoints = Array.of_list [
    (Codepoint.of_char 'a');
    (Codepoint.of_char 'b');
    (Codepoint.of_char 'c');
    (Codepoint.of_char 'd');
    (Codepoint.of_char 'e');
  ] in
  File.Fmt.stdout
  |> Basis.Fmt.fmt "init -> "
  |> pp (init (0L =:< Array.length codepoints) ~f:(fun i ->
    Array.get i codepoints
  ))
  |> Basis.Fmt.fmt "\ninit (1 .. 3) -> "
  |> pp (init (1L =:< 3L) ~f:(fun i ->
    Array.get i codepoints
  ))
  |> Basis.Fmt.fmt "\nof_codepoint -> "
  |> pp (of_codepoint (Codepoint.of_char 'a'))
  |> Basis.Fmt.fmt "\n"
  |> ignore

let _ = test ()
