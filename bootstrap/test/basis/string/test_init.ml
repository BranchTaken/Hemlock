open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let codepoints = Array.of_list [
    (Codepoint.of_char 'a');
    (Codepoint.of_char 'b');
    (Codepoint.of_char 'c');
    (Codepoint.of_char 'd');
    (Codepoint.of_char 'e');
  ] in
  printf "init -> %a\n" pp (init (Array.length codepoints) ~f:(fun i ->
    Array.get i codepoints
  ));
  printf "of_codepoint -> %a\n" pp (of_codepoint (Codepoint.of_char 'a'))

let _ = test ()