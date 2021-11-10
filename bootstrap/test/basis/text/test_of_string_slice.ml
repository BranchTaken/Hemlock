open! Basis.Rudiments
open! Basis
open Text

let test () =
  let fn s = begin
    let text = of_string_slice (String.C.Slice.of_string s) in
    let slice = Slice.init ~base:(Cursor.hd text) ~past:(Cursor.tl text) text in
    let s' = Slice.to_string slice in
    File.Fmt.stdout
    |> String.pp s
    |> Fmt.fmt " -> "
    |> String.pp s'
    |> Fmt.fmt "\n"
    |> ignore
  end in
  fn "";
  fn "Hello"

let _ = test ()
