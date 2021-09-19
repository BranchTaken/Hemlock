open! Basis.Rudiments
open! Basis
open Text
open Format

let test () =
  printf "@[<h>";
  let text_path = path in
  let test_path ?path s = begin
    let text = of_string_slice ?path (String.Slice.of_string s) in
    printf "%a -> %a\n"
      (Option.pp String.pp) path
      (Option.pp String.pp) (text_path text)
  end in
  test_path "";
  test_path ~path:"/foo/bar" "";
  printf "@]"

let _ = test ()
