open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_prefix s ~prefix = begin
    printf "is_prefix %a ~prefix:%a -> %B\n" pp s pp prefix
      (is_prefix s ~prefix);
    printf "chop_prefix %a ~prefix:%a -> %s\n" pp s pp prefix
      (match chop_prefix s ~prefix with
        | None -> "None"
        | Some s' -> "\"" ^ s' ^ "\""
      )
  end in

  test_prefix "abc" ~prefix:"";
  test_prefix "abc" ~prefix:"a";
  test_prefix "abc" ~prefix:"ab";
  test_prefix "abc" ~prefix:"abc";

  test_prefix "abc" ~prefix:"d";
  test_prefix "abc" ~prefix:"ad";
  test_prefix "abc" ~prefix:"abd";
  test_prefix "abc" ~prefix:"abcd"

let _ = test ()
