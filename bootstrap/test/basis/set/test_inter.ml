open! Basis.Rudiments
open! Basis
open Set

let test () =
  let test ms0 ms1 = begin
    let set0 = of_list (module Uns) ms0 in
    let set1 = of_list (module Uns) ms1 in
    let set = inter set0 set1 in
    let ms = to_list set in
    List.iter ms0 ~f:(fun m -> assert ((mem m set) || (not (mem m set1))));
    List.iter ms1 ~f:(fun m -> assert ((mem m set) || (not (mem m set0))));
    List.iter ms ~f:(fun m -> assert ((mem m set0) && (mem m set1)));
  end in
  let test_disjoint ms0 ms1 = begin
    let set0 = of_list (module Uns) ms0 in
    let set1 = of_list (module Uns) ms1 in
    let set = inter set0 set1 in
    assert ((length set) = 0L);
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
