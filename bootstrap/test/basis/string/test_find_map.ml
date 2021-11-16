open! Basis.Rudiments
open! Basis
open String

let test () =
  let test_find_map s ~f = begin
    File.Fmt.stdout
    |> Basis.Fmt.fmt "find_map "
    |> pp s
    |> Basis.Fmt.fmt " -> "
    |> Basis.Fmt.fmt (match find_map s ~f with
      | None -> "None"
      | Some s -> s
    )
    |> Basis.Fmt.fmt "\n"
    |> ignore
  end in
  test_find_map "" ~f:(fun _ -> not_reached ());
  let f = function
    | cp when Codepoint.(cp = of_char 'c') -> Some "'c'"
    | _ -> None
  in
  test_find_map "abcde" ~f;
  test_find_map "ab de" ~f

let _ = test ()
