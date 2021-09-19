open! Basis.Rudiments
open! Basis
open Set

let test () =
  let test ms0 ms1 = begin
    let set0 = of_list (module Uns) ms0 in
    let set1 = of_list (module Uns) ms1 in
    let set = union set0 set1 in
    let ms = to_list set in
    (* Compute the number of elements in the triangle defined by folding n times, each time
     * terminating upon encounter of a distinct set member. The size of the triangle is insensitive
     * to fold order. *)
    assert ((List.length ms) = (length set));
    let n = length set in
    let triangle_sum = List.fold ms ~init:0 ~f:(fun accum m ->
      accum + fold2_until set0 set1 ~init:0 ~f:(fun accum a0_opt a1_opt ->
        match a0_opt, a1_opt with
        | Some a, Some _
        | Some a, None
        | None, Some a -> (succ accum), (m = a)
        | None, None -> not_reached ()
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
  List.iteri test_lists ~f:(fun i ms0 ->
    List.iteri test_lists ~f:(fun j ms1 ->
      if i <= j then test ms0 ms1
    )
  )

let _ = test ()
