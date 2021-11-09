open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_suffix s ~suffix = begin
    printf "is_suffix %a ~suffix:%a -> %B\n" xpp s xpp suffix
      (is_suffix s ~suffix);
    printf "chop_suffix %a ~suffix:%a -> %s\n" xpp s xpp suffix
      (match chop_suffix s ~suffix with
        | None -> "None"
        | Some s' -> "\"" ^ s' ^ "\""
      )
  end in

  test_suffix "abc" ~suffix:"";
  test_suffix "abc" ~suffix:"c";
  test_suffix "abc" ~suffix:"bc";
  test_suffix "abc" ~suffix:"abc";

  test_suffix "abc" ~suffix:"d";
  test_suffix "abc" ~suffix:"dc";
  test_suffix "abc" ~suffix:"dab";
  test_suffix "abc" ~suffix:"dabc"

let _ = test ()
