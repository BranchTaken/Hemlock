open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_rev s = begin
    printf "rev %a -> %a\n" xpp s xpp (rev s);
  end in
  test_rev "";
  test_rev "a";
  test_rev "ab";
  test_rev "abc";
  test_rev "abcd"

let _ = test ()
