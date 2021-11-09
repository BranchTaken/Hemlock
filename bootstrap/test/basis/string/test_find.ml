open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_find s ~f = begin
    printf "find %a -> %s\n" xpp s (match find s ~f with
      | None -> "None"
      | Some cp -> "'" ^ (of_codepoint cp) ^ "'"
    );
  end in
  test_find "" ~f:(fun _ -> not_reached ());
  let f = function
    | cp when Codepoint.(cp = of_char 'c') -> true
    | _ -> false
  in
  test_find "abcde" ~f;
  test_find "ab de" ~f

let _ = test ()
