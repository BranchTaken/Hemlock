open! Basis.Rudiments
open! Basis
open String

let test () =
  let s = "hello\n" in
  let s2 = of_string s in
  let s3 = to_string s2 in

  File.Fmt.stdout
  |> xfmt "s=" |> xfmt ~alt:true s |> xfmt ", s2=" |> xfmt ~alt:true s2 |> xfmt ", s3="
  |> xfmt ~pad:(Codepoint.of_char '_') ~just:Basis.Fmt.Center ~width:12L ~alt:true s3 |> xfmt "\n"

let _ = test ()
