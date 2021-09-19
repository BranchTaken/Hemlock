open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let replacements = [
    (* pattern, with, in *)
    ("", "", "");
    ("", "", "abc");

    ("", "x", "abc");
    ("x", "y", "abc");

    ("", "x", "");
    ("a", "A", "abc");
    ("b", "B", "abc");
    ("c", "C", "abc");

    ("abc", "", "abc");
    ("abc", "A", "abc");
    ("abc", "AB", "abc");
    ("abc", "ABC", "abc");

    ("ab", "", "abc");
    ("ab", "A", "abc");
    ("ab", "AB", "abc");
    ("ab", "ABC", "abc");

    ("bc", "", "abc");
    ("bc", "A", "abc");
    ("bc", "AB", "abc");
    ("bc", "ABC", "abc");

    ("b", "B", "ababa");
    ("ab", "AB", "ababa");
    ("ba", "BA", "ababa");
  ] in
  List.iter replacements ~f:(fun (pattern, with_, in_) ->
    printf "s/%s/%s/ %a -> %a\n"
      pattern with_ pp in_ pp (substr_replace_first in_ ~pattern ~with_);
    printf "s/%s/%s/g %a -> %a\n"
      pattern with_ pp in_ pp (substr_replace_all in_ ~pattern ~with_);
    printf "\n"
  )

let _ = test ()
