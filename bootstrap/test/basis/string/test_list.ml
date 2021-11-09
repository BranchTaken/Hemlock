open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_list l = begin
    let s = of_list l in
    let l' = to_list s in
    let s' = of_list l' in
    printf "list: %a -> ... -> %a\n" xpp s xpp s';

    let s = of_list_rev l in
    let l' = to_list_rev s in
    let s' = of_list_rev l' in
    printf "list_rev: %a -> ... -> %a\n" xpp s xpp s';
  end in
  test_list [];
  test_list [
    (Codepoint.of_char 'a');
  ];
  test_list [
    (Codepoint.of_char 'a');
    (Codepoint.of_char 'b');
  ];
  test_list [
    (Codepoint.of_char 'a');
    (Codepoint.of_char 'b');
    (Codepoint.of_char 'c');
  ];
  test_list [
    (Codepoint.of_char 'a');
    (Codepoint.of_char 'b');
    (Codepoint.of_char 'c');
    (Codepoint.of_char 'd');
  ];
  test_list [
    (Codepoint.of_char 'a');
    (Codepoint.of_char 'b');
    (Codepoint.of_char 'c');
    (Codepoint.of_char 'd');
    (Codepoint.of_char 'e');
  ]

let _ = test ()
