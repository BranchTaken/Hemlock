open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_split_lines s = begin
    printf "split_lines %S -> [" s;
    List.iteri (split_lines s)~f:(fun i substr ->
      if Uns.(i > 0L) then printf "; ";
      printf "%S" substr
    );
    printf "]\n";

    printf "split_lines_rev %S -> [" s;
    List.iteri (split_lines_rev s)~f:(fun i substr ->
      if Uns.(i > 0L) then printf "; ";
      printf "%S" substr
    );
    printf "]\n";
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
