open! Basis.Rudiments
open! Basis
open String

let test () =
  let s = "hello\n" in
  let s2 = of_string s in
  let s3 = to_string s2 in

  File.Fmt.stdout
  |> fmt "s=" |> pp s |> fmt ", s2=" |> pp s2 |> fmt ", s3="
  |> fmt ~pad:(Codepoint.of_char '_') ~just:Basis.Fmt.Center ~width:12L ~alt:true s3 |> fmt "\n"

let _ = test ()
