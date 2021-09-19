open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_find_map s ~f = begin
    printf "find_map %a -> %s\n" pp s (match find_map s ~f with
      | None -> "None"
      | Some s -> s
    );
  end in
  test_find_map "" ~f:(fun _ -> not_reached ());
  let f = function
    | cp when Codepoint.(cp = of_char 'c') -> Some "'c'"
    | _ -> None
  in
  test_find_map "abcde" ~f;
  test_find_map "ab de" ~f

let _ = test ()
