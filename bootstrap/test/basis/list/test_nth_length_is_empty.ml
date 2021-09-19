open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let test_length lst = begin
    printf "[";
    for i = 0 to pred (length lst) do
      if i > 0 then printf "; ";
      printf "%a" Uns.pp (nth i lst);
    done;
    printf "]: length=%a, is_empty=%B\n"
      Uns.pp (length lst) (is_empty lst)
  end in
  test_length [];
  test_length [0];
  test_length [0; 1];
  test_length [0; 1; 2]

let _ = test ()
