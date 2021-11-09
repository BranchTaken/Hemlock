open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_array a = begin
    let s = of_array a in
    let a' = to_array s in
    let s' = of_array a' in
    printf "array: %a -> ... -> %a\n" xpp s xpp s';
  end in
  test_array [||];
  test_array (Array.of_list [
    (Codepoint.of_char 'a');
  ]);
  test_array (Array.of_list [
    (Codepoint.of_char 'a');
    (Codepoint.of_char 'b');
  ]);
  test_array (Array.of_list [
    (Codepoint.of_char 'a');
    (Codepoint.of_char 'b');
    (Codepoint.of_char 'c');
  ]);
  test_array (Array.of_list [
    (Codepoint.of_char 'a');
    (Codepoint.of_char 'b');
    (Codepoint.of_char 'c');
    (Codepoint.of_char 'd');
  ]);
  test_array (Array.of_list [
    (Codepoint.of_char 'a');
    (Codepoint.of_char 'b');
    (Codepoint.of_char 'c');
    (Codepoint.of_char 'd');
    (Codepoint.of_char 'e');
  ])

let _ = test ()
