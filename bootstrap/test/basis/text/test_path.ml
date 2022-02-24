open! Basis.Rudiments
open! Basis
open Text

let test () =
  let text_path = path in
  let test_path ?path s = begin
    let text = of_string_slice ?path (String.C.Slice.of_string s) in
    File.Fmt.stdout
    |> (Option.fmt Path.pp) path
    |> Fmt.fmt " -> "
    |> (Option.fmt Path.pp) (text_path text)
    |> Fmt.fmt "\n"
    |> ignore
  end in
  test_path "";
  test_path ~path:(Path.of_string "/foo/bar") ""

let _ = test ()
