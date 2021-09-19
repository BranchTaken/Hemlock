open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap

let test () =
  let test ks0 ks1 = begin
    let ordmap0 = of_klist ks0 in
    let ordmap1 = of_klist ks1 in
    let ordmap = union ~f:merge ordmap0 ordmap1 in
    let kvs = to_alist ordmap in
    (* Compute the number of elements in the triangle defined by folding n times, each time
     * terminating upon encounter of a distinct key. The size of the triangle is insensitive to fold
     * order. *)
    assert ((List.length kvs) = (length ordmap));
    let n = length ordmap in
    let triangle_sum = List.fold kvs ~init:0 ~f:(fun accum (k, _) ->
      accum + fold2_until ordmap0 ordmap1 ~init:0
          ~f:(fun accum kv0_opt kv1_opt ->
            match kv0_opt, kv1_opt with
            | Some (kx, _), Some _
            | Some (kx, _), None
            | None, Some (kx, _) -> (succ accum), (k = kx)
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
  List.iteri test_lists ~f:(fun i ks0 ->
    List.iteri test_lists ~f:(fun j ks1 ->
      if i <= j then test ks0 ks1
    )
  )

let _ = test ()
