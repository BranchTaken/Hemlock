open! Basis.Rudiments
open! Basis
open MapTest
open Map

let test () =
  let test ks0 ks1 = begin
    let map0 = of_klist ks0 in
    let map1 = of_klist ks1 in
    let map = union ~f:merge map0 map1 in
    let kvs = to_alist map in
    List.iter ks0 ~f:(fun k -> assert ((mem k map) && (mem k map0)));
    List.iter ks1 ~f:(fun k -> assert ((mem k map) && (mem k map1)));
    List.iter kvs ~f:(fun (k, _) ->
      assert ((mem k map0) || (mem k map1)));
  end in
  let test_disjoint ks0 ks1 = begin
    let map0 = of_klist ks0 in
    let map1 = of_klist ks1 in
    let map = union ~f:merge map0 map1 in
    assert ((length map) = (length map0) + (length map1));
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
    [42; 420];
    [42; 420; 421];
    [42; 420; 4200];
  ] in
  let test_disjoint_list_pairs = [
    ([], [0]);
    ([0], [1]);
    ([0], [1; 2]);
    ([0; 1], [2; 3]);
    ([0; 1], [2; 3; 4]);
    ([0; 1; 2], [3; 4; 5])
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
