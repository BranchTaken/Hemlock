open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap

let test () =
  let test ks = begin
    let ordmap = of_klist ks in
    (* Compute the number of elements in the triangle defined by folding n times, each time
     * terminating upon encounter of a distinct key. The size of the triangle is insensitive to fold
     * order. *)
    assert ((List.length ks) = (length ordmap));
    let n = length ordmap in
    let triangle_sum = List.fold ks ~init:0 ~f:(fun accum k ->
      accum + fold_right_until ordmap ~init:0 ~f:(fun (k1, _) accum ->
        (succ accum), (k = k1)
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
  List.iter test_lists ~f:(fun ks ->
    test ks
  )

let _ = test ()
