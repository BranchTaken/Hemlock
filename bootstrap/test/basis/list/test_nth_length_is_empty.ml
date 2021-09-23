open! Basis.Rudiments
open! Basis
open! ListTest
open List
open Format

let test () =
  let test_length lst = begin
    printf "[";
    iter_oc 0L (length lst) (fun i ->
      if i > 0L then printf "; ";
      printf "%a" Uns.pp (nth i lst);
    );
    printf "]: length=%a, is_empty=%B\n"
      Uns.pp (length lst) (is_empty lst)
  end in
  test_length [];
  test_length [0L];
  test_length [0L; 1L];
  test_length [0L; 1L; 2L]

let _ = test ()
