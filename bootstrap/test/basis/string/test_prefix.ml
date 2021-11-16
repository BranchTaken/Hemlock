open! Basis.Rudiments
open! Basis
open String

let test () =
  let test_prefix s ~prefix = begin
    File.Fmt.stdout
    |> Basis.Fmt.fmt "is_prefix "
    |> pp s
    |> Basis.Fmt.fmt " ~prefix:"
    |> pp prefix
    |> Basis.Fmt.fmt " -> "
    |> Bool.pp (is_prefix s ~prefix)
    |> Basis.Fmt.fmt "\nchop_prefix "
    |> pp s
    |> Basis.Fmt.fmt " ~prefix:"
    |> pp prefix
    |> Basis.Fmt.fmt " -> "
    |> Option.fmt String.pp (chop_prefix s ~prefix)
    |> Basis.Fmt.fmt "\n"
    |> ignore
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
