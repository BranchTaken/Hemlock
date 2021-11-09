open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_for s cp = begin
    let f codepoint = Codepoint.(codepoint = cp) in
    printf "for_any %a '%s' -> %B\n" xpp s (of_codepoint cp) (for_any s ~f);
    printf "for_all %a '%s' -> %B\n" xpp s (of_codepoint cp) (for_all s ~f);
    printf "mem %a '%s' -> %B\n" xpp s (of_codepoint cp) (mem cp s);
  end in
  test_for "" Codepoint.(of_char 'a');
  test_for "abcde" Codepoint.(of_char 'a');
  test_for "abcde" Codepoint.(of_char 'b');
  test_for "abcde" Codepoint.(of_char 'f');
  test_for "fff" Codepoint.(of_char 'f')

let _ = test ()
