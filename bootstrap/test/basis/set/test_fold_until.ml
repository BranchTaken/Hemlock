open! Basis.Rudiments
open! Basis
open Set

let test () =
  let test ms = begin
    let set = of_list (module Uns) ms in
    (* Compute the number of elements in the triangle defined by folding n times, each time
     * terminating upon encounter of a distinct set member. The size of the triangle is insensitive
     * to fold order. *)
    assert ((List.length ms) = (length set));
    let n = length set in
    let triangle_sum = List.fold ms ~init:0L ~f:(fun accum m ->
      accum + fold_until set ~init:0L ~f:(fun accum a ->
        (succ accum), (m = a)
      )
    ) in
    assert (triangle_sum = (n + 1L) * n / 2L);
  end in
  let test_lists = [
    [];
    [0L];
    [0L; 1L];
    [0L; 1L; 2L];
    [0L; 1L; 66L];
    [0L; 1L; 66L; 91L];
  ] in
  List.iter test_lists ~f:(fun ms ->
    test ms
  )

let _ = test ()
