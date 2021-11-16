open! Basis.Rudiments
open! Basis
open String

let test () =
  let test_find s ~f = begin
    File.Fmt.stdout
    |> Basis.Fmt.fmt "find "
    |> pp s
    |> Basis.Fmt.fmt " -> "
    |> Basis.Fmt.fmt (match find s ~f with
      | None -> "None"
      | Some cp -> "'" ^ (of_codepoint cp) ^ "'"
    )
    |> Basis.Fmt.fmt "\n"
    |> ignore
  end in
  test_find "" ~f:(fun _ -> not_reached ());
  let f = function
    | cp when Codepoint.(cp = of_char 'c') -> true
    | _ -> false
  in
  test_find "abcde" ~f;
  test_find "ab de" ~f

let _ = test ()
