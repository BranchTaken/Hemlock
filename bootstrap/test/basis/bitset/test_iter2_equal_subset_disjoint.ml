open! Basis.Rudiments
open! Basis
open Bitset

let test () =
  let test_equal ms0 ms1 = begin
    let bitset0 = of_list ms0 in
    let bitset1 = of_list ms1 in
    assert (equal bitset0 bitset1);
    assert (subset bitset0 bitset1);
    assert (subset bitset1 bitset0);
    assert ((length bitset0 = 0L) || (not (disjoint bitset0 bitset1)));
    iter2 ~f:(fun a0_opt a1_opt ->
      match a0_opt, a1_opt with
      | Some _, Some _ -> ()
      | None, Some _
      | Some _, None -> begin
          File.Fmt.stdout
          |> Fmt.fmt "Should be equal: "
          |> fmt bitset0
          |> Fmt.fmt " "
          |> fmt bitset1
          |> Fmt.fmt "\n"
          |> ignore;
          assert false;
        end
      | None, None -> not_reached ()
    ) bitset0 bitset1
  end in
  let test_disjoint ms0 ms1 = begin
    let bitset0 = of_list ms0 in
    let bitset1 = of_list ms1 in
    assert (not (equal bitset0 bitset1));
    assert (not (subset bitset0 bitset1));
    assert ((length bitset0 = 0L) || (not (subset bitset1 bitset0)));
    assert (disjoint bitset0 bitset1);
    iter2 ~f:(fun a0_opt a1_opt ->
      match a0_opt, a1_opt with
      | Some _, Some _ -> begin
          File.Fmt.stdout
          |> Fmt.fmt "Should be disjoint: "
          |> fmt bitset0
          |> Fmt.fmt " "
          |> fmt bitset1
          |> Fmt.fmt "\n"
          |> ignore;
          assert false;
        end
      | None, Some _
      | Some _, None -> ()
      | None, None -> not_reached ()
    ) bitset0 bitset1
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
  )

let _ = test ()
