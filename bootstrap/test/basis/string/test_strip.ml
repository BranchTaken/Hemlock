open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_strip ?drop s = begin
    printf "lstrip %a -> %a\n" xpp s xpp (lstrip ?drop s);
    printf "rstrip %a -> %a\n" xpp s xpp (rstrip ?drop s);
    printf "strip %a -> %a\n" xpp s xpp (strip ?drop s);
  end in
  test_strip "  a b c  ";
  test_strip ~drop:(fun codepoint ->
    Codepoint.(codepoint = (kv 0x5fL) (* '_' *))
  ) "_ a_b_c _"

let _ = test ()
