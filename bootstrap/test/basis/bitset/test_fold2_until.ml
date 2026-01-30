open! Basis.Rudiments
open! Basis
open Bitset

let test () =
  let test ms0 ms1 = begin
    let bitset0 = of_list ms0 in
    let bitset1 = of_list ms1 in
    let bitset = union bitset0 bitset1 in
    let ms = to_list bitset in
    (* Compute the number of elements in the triangle defined by folding n times, each time
     * terminating upon encounter of a distinct set member. The size of the triangle is insensitive
     * to fold order. *)
    assert ((List.length ms) = (length bitset));
    let n = length bitset in
    let triangle_sum = List.fold ms ~init:0L ~f:(fun accum m ->
      accum + fold2_until bitset0 bitset1 ~init:0L ~f:(fun accum a0_opt a1_opt ->
        match a0_opt, a1_opt with
        | Some a, Some _
        | Some a, None
        | None, Some a -> (succ accum), (m = a)
        | None, None -> not_reached ()
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
  List.iteri test_lists ~f:(fun i ms0 ->
    List.iteri test_lists ~f:(fun j ms1 ->
      if i <= j then test ms0 ms1
    )
  )

let _ = test ()
