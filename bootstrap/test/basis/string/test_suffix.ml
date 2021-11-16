open! Basis.Rudiments
open! Basis
open String

let test () =
  let test_suffix s ~suffix = begin
    File.Fmt.stdout
    |> Basis.Fmt.fmt "is_suffix "
    |> pp s
    |> Basis.Fmt.fmt " ~suffix:"
    |> pp suffix
    |> Basis.Fmt.fmt " -> "
    |> Bool.pp (is_suffix s ~suffix)
    |> Basis.Fmt.fmt "\nchop_suffix "
    |> pp s
    |> Basis.Fmt.fmt " ~suffix:"
    |> pp suffix
    |> Basis.Fmt.fmt " -> "
    |> Option.fmt String.pp (chop_suffix s ~suffix)
    |> Basis.Fmt.fmt "\n"
    |> ignore
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
