open! Basis.Rudiments
open! Basis
open MapTest
open Map

let test () =
  let test_equal ks0 ks1 = begin
    let map0 = of_klist ks0 in
    let map1 = of_klist ks1 in
    assert (equal veq map0 map1);
    assert (subset veq map0 map1);
    assert (subset veq map1 map0);
    assert ((length map0 = 0L) || (not (disjoint map0 map1)));
    iter2 ~f:(fun kv0_opt kv1_opt ->
      match kv0_opt, kv1_opt with
      | Some _, Some _ -> ()
      | None, Some _
      | Some _, None -> begin
          File.Fmt.stdout
          |> Fmt.fmt "Should be equal: "
          |> (fmt Uns.pp) map0
          |> Fmt.fmt " "
          |> (fmt Uns.pp) map1
          |> Fmt.fmt "\n"
          |> ignore;
          assert false;
        end
      | None, None -> not_reached ()
    ) map0 map1
  end in
  let test_disjoint ks0 ks1 = begin
    let map0 = of_klist ks0 in
    let map1 = of_klist ks1 in
    assert (not (equal veq map0 map1));
    assert (not (subset veq map0 map1));
    assert ((length map0 = 0L) || (not (subset veq map1 map0)));
    assert (disjoint map0 map1);
    iter2 ~f:(fun kv0_opt kv1_opt ->
      match kv0_opt, kv1_opt with
      | Some _, Some _ -> begin
          File.Fmt.stdout
          |> Fmt.fmt "Should be disjoint: "
          |> (fmt Uns.pp) map0
          |> Fmt.fmt " "
          |> (fmt Uns.pp) map1
          |> Fmt.fmt "\n"
          |> ignore;
          assert false;
        end
      | None, Some _
      | Some _, None -> ()
      | None, None -> not_reached ()
    ) map0 map1
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
  List.iter test_lists ~f:(fun ks ->
    test_equal ks ks;
    test_equal ks (List.rev ks);
    test_equal (List.rev ks) ks;
    test_equal (List.rev ks) (List.rev ks);
  );
  List.iter test_disjoint_list_pairs ~f:(fun (ks0, ks1) ->
    test_disjoint ks0 ks1;
    test_disjoint ks0 (List.rev ks1);
    test_disjoint (List.rev ks0) ks1;
    test_disjoint (List.rev ks0) (List.rev ks1);
  )

let _ = test ()
