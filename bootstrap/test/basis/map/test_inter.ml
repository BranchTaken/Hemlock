open! Basis.Rudiments
open! Basis
open MapTest
open Map

let test () =
  let test ks0 ks1 = begin
    let map0 = of_klist ks0 in
    let map1 = of_klist ks1 in
    let map = inter ~f:merge map0 map1 in
    let kvs = to_alist map in
    List.iter ks0 ~f:(fun k ->
      assert ((mem k map) || (not (mem k map1))));
    List.iter ks1 ~f:(fun k ->
      assert ((mem k map) || (not (mem k map0))));
    List.iter kvs ~f:(fun (k, _) ->
      assert ((mem k map0) && (mem k map1)));
  end in
  let test_disjoint ks0 ks1 = begin
    let map0 = of_klist ks0 in
    let map1 = of_klist ks1 in
    let map = inter ~f:merge map0 map1 in
    assert ((length map) = 0L);
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
  let test_disjoint_list_pairs = [
    ([], [0L]);
    ([0L], [1L]);
    ([0L], [1L; 2L]);
    ([0L; 1L], [2L; 3L]);
    ([0L; 1L], [2L; 3L; 4L]);
    ([0L; 1L; 2L], [3L; 4L; 5L])
  ] in
  List.iteri test_lists ~f:(fun i ks0 ->
    List.iteri test_lists ~f:(fun j ks1 ->
      if i <= j then begin
        test ks0 ks1;
        test ks1 ks0
      end
    )
  );
  List.iter test_disjoint_list_pairs ~f:(fun (ks0, ks1) ->
    test_disjoint ks0 ks1;
    test_disjoint ks0 (List.rev ks1);
    test_disjoint (List.rev ks0) ks1;
    test_disjoint (List.rev ks0) (List.rev ks1);
  )

let _ = test ()
