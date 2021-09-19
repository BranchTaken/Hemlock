open! Basis.Rudiments
open! Basis
open Ordset

let test () =
  let test ms = begin
    let ordset = of_list (module Uns) ms in
    (* Compute the number of elements in the triangle defined by folding n times, each time
     * terminating upon encounter of a distinct set member. The size of the triangle is insensitive
     * to fold order. *)
    assert ((List.length ms) = (length ordset));
    let n = length ordset in
    let triangle_sum = List.fold ms ~init:0 ~f:(fun accum m ->
      accum + fold_right_until ordset ~init:0 ~f:(fun a accum ->
        (succ accum), (m = a)
      )
    ) in
    assert (triangle_sum = (n + 1) * n / 2);
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
  ] in
  List.iter test_lists ~f:(fun ms ->
    test ms
  )

let _ = test ()
