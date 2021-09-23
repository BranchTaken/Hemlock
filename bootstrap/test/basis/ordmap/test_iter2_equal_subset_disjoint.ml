open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap
open Format

let test () =
  printf "@[";
  let test_equal ks0 ks1 = begin
    let ordmap0 = of_klist ks0 in
    let ordmap1 = of_klist ks1 in
    assert (equal veq ordmap0 ordmap1);
    assert (subset veq ordmap0 ordmap1);
    assert (subset veq ordmap1 ordmap0);
    assert ((length ordmap0 = 0L) || (not (disjoint ordmap0 ordmap1)));
    iter2 ~f:(fun kv0_opt kv1_opt ->
      match kv0_opt, kv1_opt with
      | Some _, Some _ -> ()
      | None, Some _
      | Some _, None -> begin
          printf "Should be equal:@,%a@,%a@\n"
            (pp Uns.pp) ordmap0 (pp Uns.pp) ordmap1;
          assert false;
        end
      | None, None -> not_reached ()
    ) ordmap0 ordmap1
  end in
  let test_disjoint ks0 ks1 = begin
    let ordmap0 = of_klist ks0 in
    let ordmap1 = of_klist ks1 in
    assert (not (equal veq ordmap0 ordmap1));
    assert (not (subset veq ordmap0 ordmap1));
    assert ((length ordmap0 = 0L) || (not (subset veq ordmap1 ordmap0)));
    assert (disjoint ordmap0 ordmap1);
    iter2 ~f:(fun kv0_opt kv1_opt ->
      match kv0_opt, kv1_opt with
      | Some _, Some _ -> begin
          printf "Should be disjoint:@,%a@,%a@\n"
            (pp Uns.pp) ordmap0 (pp Uns.pp) ordmap1;
          assert false;
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
  );
  printf "@]"

let _ = test ()
