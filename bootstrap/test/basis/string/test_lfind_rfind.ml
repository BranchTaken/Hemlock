open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_find s cp = begin
    printf "lfind %a '%s' -> %s\n" pp s (of_codepoint cp)
      (match lfind cp s with
        | None -> "<not found>"
        | Some cursor -> asprintf "%a" Uns.pp (C.Cursor.bindex cursor)
      );
    printf "contains %a '%s' -> %B\n" pp s (of_codepoint cp) (contains cp s);
    printf "rfind %a '%s' -> %s\n" pp s (of_codepoint cp)
      (match rfind cp s with
        | None -> "<not found>"
        | Some cursor -> asprintf "%a" Uns.pp (C.Cursor.bindex cursor)
      )
  end in
  test_find "" Codepoint.(of_char 'b');
  test_find "abcba" Codepoint.(of_char 'a');
  test_find "abcba" Codepoint.(of_char 'b');
  test_find "abcba" Codepoint.(of_char 'c');
  test_find "abcba" Codepoint.(of_char 'd')

let _ = test ()
