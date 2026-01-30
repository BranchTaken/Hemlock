open! Basis.Rudiments
open! Basis
open Bitset

let test () =
  let test ms0 ms1 = begin
    let bitset0 = of_list ms0 in
    let bitset1 = of_list ms1 in
    let bitset = union bitset0 bitset1 in
    let ms = to_list bitset in
    List.iter ms0 ~f:(fun m -> assert ((mem m bitset) && (mem m bitset0)));
    List.iter ms1 ~f:(fun m -> assert ((mem m bitset) && (mem m bitset1)));
    List.iter ms ~f:(fun m -> assert ((mem m bitset0) || (mem m bitset1)));
  end in
  let test_disjoint ms0 ms1 = begin
    let bitset0 = of_list ms0 in
    let bitset1 = of_list ms1 in
    let bitset = union bitset0 bitset1 in
    assert ((length bitset) = (length bitset0) + (length bitset1));
  end in
  let test_lists = [
    [];
    [0L];
    [0L; 1L];
    [0L; 1L; 2L];
    [0L; 1L; 66L];
    [0L; 1L; 66L; 91L];
  ] in
  let test_disjoint_list_pairs = [
    ([], [0L]);
    ([0L], [1L]);
    ([0L], [1L; 2L]);
    ([0L; 1L], [2L; 3L]);
    ([0L; 1L], [2L; 3L; 4L]);
    ([0L; 1L; 2L], [3L; 4L; 5L])
  ] in
  List.iteri test_lists ~f:(fun i ms0 ->
    List.iteri test_lists ~f:(fun j ms1 ->
      if i <= j then begin
        test ms0 ms1;
        test ms1 ms0
      end
    )
  );
  List.iter test_disjoint_list_pairs ~f:(fun (ms0, ms1) ->
    test_disjoint ms0 ms1;
    test_disjoint ms0 (List.rev ms1);
    test_disjoint (List.rev ms0) ms1;
    test_disjoint (List.rev ms0) (List.rev ms1);
  )

let _ = test ()
