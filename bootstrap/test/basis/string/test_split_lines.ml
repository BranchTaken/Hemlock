open! Basis.Rudiments
open! Basis
open String

let test () =
  let test_split_lines s = begin
    File.Fmt.stdout
    |> Basis.Fmt.fmt "split_lines "
    |> pp s
    |> Basis.Fmt.fmt " -> "
    |> List.pp String.pp (split_lines s)
    |> Basis.Fmt.fmt "\nsplit_lines_rev "
    |> pp s
    |> Basis.Fmt.fmt " -> "
    |> List.pp String.pp (split_lines_rev s)
    |> Basis.Fmt.fmt "\n"
    |> ignore
  end in
  test_split_lines "ab";

  test_split_lines "\nab";
  test_split_lines "a\nb";
  test_split_lines "ab\n";
  test_split_lines "\na\nb\n";

  test_split_lines "\r\nab";
  test_split_lines "a\r\nb";
  test_split_lines "ab\r\n";
  test_split_lines "\r\na\r\nb\r\n";

  test_split_lines "a\r\r\nb";

  test_split_lines "a\n\nb";
  test_split_lines "a\r\n\r\nb";

  test_split_lines "a\n\r\nb";
  test_split_lines "a\r\n\nb";

  test_split_lines "a\n\n\nb"

let _ = test ()
