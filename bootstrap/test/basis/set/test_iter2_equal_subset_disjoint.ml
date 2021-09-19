open! Basis.Rudiments
open! Basis
open SetTest
open Set
open Format

let test () =
  printf "@[";
  let test_equal ms0 ms1 = begin
    let set0 = of_list (module Uns) ms0 in
    let set1 = of_list (module Uns) ms1 in
    assert (equal set0 set1);
    assert (subset set0 set1);
    assert (subset set1 set0);
    assert ((length set0 = 0) || (not (disjoint set0 set1)));
    iter2 ~f:(fun a0_opt a1_opt ->
      match a0_opt, a1_opt with
      | Some _, Some _ -> ()
      | None, Some _
      | Some _, None -> begin
          printf "Should be equal:@,%a@,%a@\n" pp set0 pp set1;
          assert false;
        end
      | None, None -> not_reached ()
    ) set0 set1
  end in
  let test_disjoint ms0 ms1 = begin
    let set0 = of_list (module Uns) ms0 in
    let set1 = of_list (module Uns) ms1 in
    assert (not (equal set0 set1));
    assert (not (subset set0 set1));
    assert ((length set0 = 0) || (not (subset set1 set0)));
    assert (disjoint set0 set1);
    iter2 ~f:(fun a0_opt a1_opt ->
      match a0_opt, a1_opt with
      | Some _, Some _ -> begin
          printf "Should be disjoint:@,%a@,%a@\n" pp set0 pp set1;
          assert false;
        end
      | None, Some _
      | Some _, None -> ()
      | None, None -> not_reached ()
    ) set0 set1
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
  ] in
  let test_disjoint_list_pairs = [
    ([], [0]);
    ([0], [1]);
    ([0], [1; 2]);
    ([0; 1], [2; 3]);
    ([0; 1], [2; 3; 4]);
    ([0; 1; 2], [3; 4; 5])
  ] in
  List.iter test_lists ~f:(fun ms ->
    test_equal ms ms;
    test_equal ms (List.rev ms);
    test_equal (List.rev ms) ms;
    test_equal (List.rev ms) (List.rev ms);
  );
  List.iter test_disjoint_list_pairs ~f:(fun (ms0, ms1) ->
    test_disjoint ms0 ms1;
    test_disjoint ms0 (List.rev ms1);
    test_disjoint (List.rev ms0) ms1;
    test_disjoint (List.rev ms0) (List.rev ms1);
  );
  printf "@]"

let _ = test ()
