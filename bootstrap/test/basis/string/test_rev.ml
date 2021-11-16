open! Basis.Rudiments
open! Basis
open String

let test () =
  let test_rev s = begin
    File.Fmt.stdout
    |> Basis.Fmt.fmt "rev "
    |> pp s
    |> Basis.Fmt.fmt " -> "
    |> pp (rev s)
    |> Basis.Fmt.fmt "\n"
    |> ignore
  end in
  test_rev "";
  test_rev "a";
  test_rev "ab";
  test_rev "abc";
  test_rev "abcd"

let _ = test ()
