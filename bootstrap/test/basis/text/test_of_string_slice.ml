open! Basis.Rudiments
open! Basis
open Text
open Format

let test () =
  printf "@[<h>";
  let fn s = begin
    let text = of_string_slice (String.C.Slice.of_string s) in
    let slice = Slice.init ~base:(Cursor.hd text) ~past:(Cursor.tl text) text in
    let s' = Slice.to_string slice in
    printf "%a -> %a\n" String.pp s String.pp s'
  end in
  fn "";
  fn "Hello";
  printf "@]"

let _ = test ()
