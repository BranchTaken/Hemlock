open! Basis.Rudiments
open! Basis
open MapTest
open Map

let test () =
  let test ks = begin
    let map = of_klist ks in
    (* Compute the number of elements in the triangle defined by folding n times, each time
     * terminating upon encounter of a distinct key. The size of the triangle is insensitive to fold
     * order. *)
    assert ((List.length ks) = (length map));
    let n = length map in
    let triangle_sum = List.fold ks ~init:0L ~f:(fun accum k ->
      accum + fold_until map ~init:0L ~f:(fun accum (k1, _) ->
        (succ accum), (k = k1)
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
    [42L; 420L];
    [42L; 420L; 421L];
    [42L; 420L; 4200L];
  ] in
  List.iter test_lists ~f:(fun ks ->
    test ks
  )

let _ = test ()
