open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap

let test () =
  let test_equal ks0 ks1 = begin
    let ordmap0 = of_klist ks0 in
    let ordmap1 = of_klist ks1 in
    assert (equal ~vequal ordmap0 ordmap1);
    assert (subset ~vsubset ordmap0 ordmap1);
    assert (subset ~vsubset ordmap1 ordmap0);
    assert ((length ordmap0 = 0L) || (not (disjoint ~vdisjoint ordmap0 ordmap1)));
    iter2 ~f:(fun kv0_opt kv1_opt ->
      match kv0_opt, kv1_opt with
      | Some _, Some _ -> ()
      | None, Some _
      | Some _, None -> begin
          File.Fmt.stdout
          |> Fmt.fmt "Should be equal: "
          |> (fmt_internals Bitset.pp) ordmap0
          |> Fmt.fmt ", "
          |> (fmt_internals Bitset.pp) ordmap1
          |> Fmt.fmt "\n"
          |> ignore;
          assert false;
        end
      | None, None -> not_reached ()
    ) ordmap0 ordmap1
  end in
  let test_disjoint ordmap0 ordmap1 = begin
    assert (not (equal ~vequal ordmap0 ordmap1));
    assert (not (subset ~vsubset ordmap0 ordmap1));
    assert ((length ordmap0 = 0L) || (not (subset ~vsubset ordmap1 ordmap0)));
    assert (disjoint ~vdisjoint ordmap0 ordmap1);
    iter2 ~f:(fun kv0_opt kv1_opt ->
      match kv0_opt, kv1_opt with
      | Some (k, v0), Some (_k, v1) -> begin
          match vdisjoint k v0 v1 with
          | true -> ()
          | false -> begin
              File.Fmt.stdout
              |> Fmt.fmt "Should be disjoint: "
              |> (fmt_internals Bitset.pp) ordmap0
              |> Fmt.fmt ", "
              |> (fmt_internals Bitset.pp) ordmap1
              |> Fmt.fmt "\n"
              |> ignore;
              assert false;
            end
        end
      | None, Some _
      | Some _, None -> ()
      | None, None -> not_reached ()
    ) ordmap0 ordmap1
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
    ([0L; 1L; 2L], [3L; 4L; 5L]);

    ([0L], [0L]);
    ([0L], [0L; 1L]);
    ([0L], [0L; 1L; 2L]);
    ([0L], [0L; 1L; 2L; 3L]);
    ([0L], [0L; 1L; 2L; 3L; 4L]);

    ([0L; 1L], [0L]);
    ([0L; 1L], [0L; 1L]);
    ([0L; 1L], [0L; 1L; 2L]);
    ([0L; 1L], [0L; 1L; 2L; 3L]);
    ([0L; 1L], [0L; 1L; 2L; 3L; 4L]);

    ([0L; 1L; 2L], [0L]);
    ([0L; 1L; 2L], [0L; 1L]);
    ([0L; 1L; 2L], [0L; 1L; 2L]);
    ([0L; 1L; 2L], [0L; 1L; 2L; 3L]);
    ([0L; 1L; 2L], [0L; 1L; 2L; 3L; 4L]);

    ([0L; 1L; 2L; 3L], [0L]);
    ([0L; 1L; 2L; 3L], [0L; 1L]);
    ([0L; 1L; 2L; 3L], [0L; 1L; 2L]);
    ([0L; 1L; 2L; 3L], [0L; 1L; 2L; 3L]);
    ([0L; 1L; 2L; 3L], [0L; 1L; 2L; 3L; 4L]);

    ([0L; 1L; 2L; 3L; 4L], [0L]);
    ([0L; 1L; 2L; 3L; 4L], [0L; 1L]);
    ([0L; 1L; 2L; 3L; 4L], [0L; 1L; 2L]);
    ([0L; 1L; 2L; 3L; 4L], [0L; 1L; 2L; 3L]);
    ([0L; 1L; 2L; 3L; 4L], [0L; 1L; 2L; 3L; 4L]);
  ] in
  List.iter test_lists ~f:(fun ks ->
    test_equal ks ks;
    test_equal ks (List.rev ks);
    test_equal (List.rev ks) ks;
    test_equal (List.rev ks) (List.rev ks);
  );
  List.iter test_disjoint_list_pairs ~f:(fun (ks0, ks1) ->
    test_disjoint (of_klist_v ks0 0L) (of_klist_v ks1 1L);
    test_disjoint (of_klist_v ks0 0L) (of_klist_v (List.rev ks1) 1L);
    test_disjoint (of_klist_v (List.rev ks0) 0L) (of_klist_v ks1 1L);
    test_disjoint (of_klist_v (List.rev ks0) 0L) (of_klist_v (List.rev ks1) 1L);
  )

let _ = test ()
