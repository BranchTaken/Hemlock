open Rudiments

type ('a, 'cmp) t = ('a, unit, 'cmp) Map.t

type ('a, 'cmp) cmper = ('a, 'cmp) Map.cmper

let cmper_m = Map.cmper_m

let cmper = Map.cmper

let empty = Map.empty

let singleton m a =
  Map.singleton m ~k:a ~v:()

let length = Map.length

let is_empty = Map.is_empty

let mem = Map.mem

let choose t =
  match Map.choose t with
  | Some (k, _) -> Some k
  | None -> None

let choose_hlt t =
  let k, _ = Map.choose_hlt t in
  k

let insert a t =
  Map.insert ~k:a ~v:() t

let of_list m alist =
  match alist with
  | [] -> empty m
  | a :: alist' -> begin
      let rec fn alist set = begin
        match alist with
        | [] -> set
        | a :: alist' -> fn alist' (insert a set)
      end in
      fn alist' (singleton m a)
    end

let remove = Map.remove

let fold_until ~init ~f t =
  Map.fold_until ~init ~f:(fun accum (k, _) -> f accum k) t

let fold ~init ~f t =
  fold_until ~init ~f:(fun accum a -> (f accum a), false) t

let iter ~f t =
  fold ~init:() ~f:(fun _ a -> f a) t

let hash_fold t state =
  Map.hash_fold Unit.hash_fold t state

(* Seq. *)
module Seq_poly2_fold2 = struct
  type ('a, 'cmp) container = ('a, 'cmp) t
  type 'a elm = 'a
  type ('a, 'cmp) t = ('a, unit, 'cmp) Map.Seq.t

  let init = Map.Seq.init

  let length = Map.Seq.length

  let next t =
    let (a, _), t' = Map.Seq.next t in
    a, t'

  let next_opt t =
    match Map.Seq.next_opt t with
    | Some ((a, _), t') -> Some (a, t')
    | None -> None

  let cmper = Map.Seq.cmper

  let cmp = Map.Seq.cmp
end
include Seq.Make_poly2_fold2(Seq_poly2_fold2)
module Seq = Seq_poly2_fold2

let equal t0 t1 =
  Map.equal Unit.( = ) t0 t1

let subset t0 t1 =
  Map.subset Unit.( = ) t0 t1

let disjoint t0 t1 =
  Map.disjoint t0 t1

let union t0 t1 =
  Map.union ~f:(fun _ _ _ -> ()) t0 t1

let inter t0 t1 =
  Map.inter ~f:(fun _ _ _ -> ()) t0 t1

let diff = Map.diff

let count ~f t =
  Map.count ~f:(fun (a, _) -> f a) t

let for_any ~f t =
  Map.for_any ~f:(fun (a, _) -> f a) t

let for_all ~f t =
  Map.for_all ~f:(fun (a, _) -> f a) t

let find ~f t =
  match Map.find ~f:(fun (a, _) -> f a) t with
  | Some (a, _) -> Some a
  | None -> None

let find_map ~f t =
  Map.find_map ~f:(fun (a, _) -> f a) t

let filter ~f t =
  Map.filter ~f:(fun (a, _) -> f a) t

let partition_tf ~f t =
  Map.partition_tf ~f:(fun (a, _) -> f a) t

let reduce ~f t =
  Map.kreduce ~f t

let reduce_hlt ~f t =
  Map.kreduce_hlt ~f t

let to_list t =
  fold ~init:[] ~f:(fun accum a -> a :: accum) t

module Set_to_array = struct
  include Seq
  include Array.Seq.Make_poly2(Seq)
end

let to_array t =
  Set_to_array.(to_array (init t))

(******************************************************************************)
(* Begin tests. *)

let pp ppf t =
  let open Format in
  fprintf ppf "@[<h>Set {";
  let cmper = Map.cmper t in
  let t_sorted = Array.sort ~cmp:cmper.cmp (to_array t) in
  Array.iteri t_sorted ~f:(fun i a ->
    if i > 0 then fprintf ppf ";@ ";
    fprintf ppf "%a" cmper.pp a
  );
  fprintf ppf "}@]"

let%expect_test "hash_fold" =
  let open Format in
  printf "@[";
  let rec fn = function
    | [] -> ()
    | l :: lists' -> begin
        let set = of_list (module Usize) l in
        printf "hash_fold (of_list (module Usize) %a) -> %a@\n"
          (List.pp Usize.pp) l
          Hash.pp (Hash.t_of_state (hash_fold set Hash.State.empty));
        fn lists'
      end
  in
  let lists = [
    [];
  ] in
  fn lists;
  printf "@]";

  [%expect{|
    hash_fold (of_list (module Usize) []) -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    |}]

let%expect_test "hash_fold empty" =
  let hash_empty state = begin
    state
    |> hash_fold (empty (module Usize))
  end in
  let e1 =
    Hash.State.empty
    |> hash_empty
  in
  let e2 =
    Hash.State.empty
    |> hash_empty
    |> hash_empty
  in
  assert U128.((Hash.t_of_state e1) <> (Hash.t_of_state e2));

  [%expect{|
    |}]

let%expect_test "empty,cmper_m,singleton,length" =
  let open Format in
  printf "@[";
  let e = empty (module Usize) in
  assert (length e = 0);
  printf "%a@\n" pp e;

  let s = singleton (cmper_m e) 0 in
  assert (length s = 1);
  printf "%a@\n" pp s;
  printf "@]";

  [%expect{|
    Set {}
    Set {0}
    |}]

let%expect_test "mem,insert,subset" =
  let open Format in
  printf "@[";
  let rec test ms set = begin
    match ms with
    | [] -> printf "%a@\n" pp set
    | m :: ms' -> begin
        assert (not (mem m set));
        let set' = insert m set in
        assert (mem m set');
        assert (subset set' set);
        assert (not (subset set set'));
        test ms' set'
      end
  end in
  let ms = [1; 3; 2; 44; 45; 56; 60; 66; 75; 81; 91] in
  test ms (empty (module Usize));
  printf "@]";

  [%expect{|
    Set {1; 2; 3; 44; 45; 56; 60; 66; 75; 81; 91}
    |}]

let%expect_test "of_list duplicate" =
  let open Format in
  printf "@[";
  printf "%a@\n" pp (of_list (module Usize) [0; 0]);
  printf "@]";

  [%expect{|
    Set {0}
    |}]

let%expect_test "of_list,remove" =
  let open Format in
  let geometry = get_geometry () in
  safe_set_geometry ~max_indent:100 ~margin:200;
  printf "@[";
  let test m set descr = begin
    printf "--- %s ---@\n" descr;
    printf "@[<v>remove %a@;<0 2>@[<v>%a ->@,%a@]@]@\n"
      Usize.pp m pp set pp (remove m set)
  end in
  let test_tuples = [
    ([0; 1], 2,          "Not member, elm empty.");
    ([1], 91,            "Not member, elm of different value.");
    ([0], 0,             "Member, length 1 -> 0.");
    ([0; 1], 1,          "Member, length 2 -> 1.");
    ([0; 1; 2], 2,       "Member, length 3 -> 2.");
    ([0; 1; 66], 66,     "Member, subnode elms 2 -> 1.");
    ([0; 1; 66; 91], 91, "Member, subnode elms 3 -> 2.");
  ] in
  List.iter test_tuples ~f:(fun (ms, m, descr) ->
    let set = of_list (module Usize) ms in
    test m set descr
  );
  printf "@]";
  safe_set_geometry ~max_indent:geometry.max_indent ~margin:geometry.margin;

  [%expect{|
    --- Not member, elm empty. ---
    remove 2
      Set {0; 1} ->
      Set {0; 1}
    --- Not member, elm of different value. ---
    remove 91
      Set {1} ->
      Set {1}
    --- Member, length 1 -> 0. ---
    remove 0
      Set {0} ->
      Set {}
    --- Member, length 2 -> 1. ---
    remove 1
      Set {0; 1} ->
      Set {0}
    --- Member, length 3 -> 2. ---
    remove 2
      Set {0; 1; 2} ->
      Set {0; 1}
    --- Member, subnode elms 2 -> 1. ---
    remove 66
      Set {0; 1; 66} ->
      Set {0; 1}
    --- Member, subnode elms 3 -> 2. ---
    remove 91
      Set {0; 1; 66; 91} ->
      Set {0; 1; 66}
    |}]

let%expect_test "of_list,to_list,to_array" =
  let open Format in
  printf "@[<h>";
  let test ms = begin
    let set = of_list (module Usize) ms in
    let list_sorted = List.sort ~cmp:Usize.cmp (to_list set) in
    let array_sorted = Array.sort ~cmp:Usize.cmp (to_array set) in
    printf "of_list %a; to_list -> %a; to_array -> %a\n"
      (List.pp Usize.pp) ms
      (List.pp Usize.pp) list_sorted
      (Array.pp Usize.pp) array_sorted
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
  ] in
  List.iter test_lists ~f:(fun ms ->
    test ms
  );
  printf "@]";

  [%expect{|
    of_list []; to_list -> []; to_array -> [||]
    of_list [0]; to_list -> [0]; to_array -> [|0|]
    of_list [0; 1]; to_list -> [0; 1]; to_array -> [|0; 1|]
    of_list [0; 1; 2]; to_list -> [0; 1; 2]; to_array -> [|0; 1; 2|]
    of_list [0; 1; 66]; to_list -> [0; 1; 66]; to_array -> [|0; 1; 66|]
    of_list [0; 1; 66; 91]; to_list -> [0; 1; 66; 91]; to_array -> [|0; 1; 66; 91|]
    |}]

let%expect_test "choose_hlt" =
  let open Format in
  printf "@[";
  (* test is n^2 time complexity, so keep n small. *)
  let rec test n i set = begin
    match i < n with
    | false -> set
    | true -> begin
        let set' = test n (succ i) (insert i set) in
        let m = choose_hlt set' in
        let set'' = remove m set' in
        assert ((length set') = (length set'') + 1);
        set''
      end
  end in
  let e = empty (module Usize) in
  let _ = test 100 0 e in
  printf "@]";

  [%expect{|
    |}]

let%expect_test "fold_until" =
  let test ms = begin
    let set = of_list (module Usize) ms in
    (* Compute the number of elements in the triangle defined by folding n
     * times, each time terminating upon encounter of a distinct set member.
     * The size of the triangle is insensitive to fold order. *)
    assert ((List.length ms) = (length set));
    let n = length set in
    let triangle_sum = List.fold ms ~init:0 ~f:(fun accum m ->
      accum + fold_until set ~init:0 ~f:(fun accum a ->
        (succ accum), (m = a)
      )
    ) in
    assert (triangle_sum = (n + 1) * n / 2);
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
  ] in
  List.iter test_lists ~f:(fun ms ->
    test ms
  );

  [%expect{|
    |}]

let%expect_test "fold2_until" =
  let test ms0 ms1 = begin
    let set0 = of_list (module Usize) ms0 in
    let set1 = of_list (module Usize) ms1 in
    let set = union set0 set1 in
    let ms = to_list set in
    (* Compute the number of elements in the triangle defined by folding n
     * times, each time terminating upon encounter of a distinct set member.
     * The size of the triangle is insensitive to fold order. *)
    assert ((List.length ms) = (length set));
    let n = length set in
    let triangle_sum = List.fold ms ~init:0 ~f:(fun accum m ->
      accum + fold2_until set0 set1 ~init:0 ~f:(fun accum a0_opt a1_opt ->
        match a0_opt, a1_opt with
        | Some a, Some _
        | Some a, None
        | None, Some a -> (succ accum), (m = a)
        | None, None -> not_reached ()
      )
    ) in
    assert (triangle_sum = (n + 1) * n / 2);
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
  ] in
  List.iteri test_lists ~f:(fun i ms0 ->
    List.iteri test_lists ~f:(fun j ms1 ->
      if i <= j then test ms0 ms1
    )
  );

  [%expect{|
    |}]

let%expect_test "fold2" =
  let open Format in
  printf "@[";
  let pp_pair ppf (a0_opt, a1_opt) = begin
    fprintf ppf "(%a, %a)"
      (Option.pp Usize.pp) a0_opt
      (Option.pp Usize.pp) a1_opt
  end in
  let test ms0 ms1 = begin
    let set0 = of_list (module Usize) ms0 in
    let set1 = of_list (module Usize) ms1 in
    let pairs = fold2 ~init:[] ~f:(fun accum a0_opt a1_opt ->
      (a0_opt, a1_opt) :: accum
    ) set0 set1 in
    let pairs_sorted = List.sort ~cmp:(fun pair0 pair1 ->
      let a_of_pair = function
        | Some a, _
        | _, Some a -> a
        | None, None -> not_reached ()
      in
      let a0 = a_of_pair pair0 in
      let a1 = a_of_pair pair1 in
      Usize.cmp a0 a1
    ) pairs in
    printf "fold2 %a %a -> %a@\n"
      (List.pp Usize.pp) ms0
      (List.pp Usize.pp) ms1
      (List.pp pp_pair) pairs_sorted
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
  ] in
  List.iteri test_lists ~f:(fun i ms0 ->
    List.iteri test_lists ~f:(fun j ms1 ->
      if i <= j then test ms0 ms1
    )
  );
  printf "@]";

  [%expect{|
    fold2 [] [] -> []
    fold2 [] [0] -> [(None, Some 0)]
    fold2 [] [0; 1] -> [(None, Some 0); (None, Some 1)]
    fold2 [] [0; 1; 2] -> [(None, Some 0); (None, Some 1); (None, Some 2)]
    fold2 [] [0; 1; 66] -> [(None, Some 0); (None, Some 1); (None, Some 66)]
    fold2 [] [0; 1; 66; 91] -> [(None, Some 0); (None, Some 1); (None, Some 66); (None, Some 91)]
    fold2 [0] [0] -> [(Some 0, Some 0)]
    fold2 [0] [0; 1] -> [(Some 0, Some 0); (None, Some 1)]
    fold2 [0] [0; 1; 2] -> [(Some 0, Some 0); (None, Some 1); (None, Some 2)]
    fold2 [0] [0; 1; 66] -> [(Some 0, Some 0); (None, Some 1); (None, Some 66)]
    fold2 [0] [0; 1; 66; 91] -> [(Some 0, Some 0); (None, Some 1); (None, Some 66); (None, Some 91)]
    fold2 [0; 1] [0; 1] -> [(Some 0, Some 0); (Some 1, Some 1)]
    fold2 [0; 1] [0; 1; 2] -> [(Some 0, Some 0); (Some 1, Some 1); (None, Some 2)]
    fold2 [0; 1] [0; 1; 66] -> [(Some 0, Some 0); (Some 1, Some 1); (None, Some 66)]
    fold2 [0; 1] [0; 1; 66; 91] -> [(Some 0, Some 0); (Some 1, Some 1); (None, Some 66); (None, Some 91)]
    fold2 [0; 1; 2] [0; 1; 2] -> [(Some 0, Some 0); (Some 1, Some 1); (Some 2, Some 2)]
    fold2 [0; 1; 2] [0; 1; 66] -> [(Some 0, Some 0); (Some 1, Some 1); (Some 2, None); (None, Some 66)]
    fold2 [0; 1; 2] [0; 1; 66; 91] -> [(Some 0, Some 0); (Some 1, Some 1); (Some 2, None); (None, Some 66); (None, Some 91)]
    fold2 [0; 1; 66] [0; 1; 66] -> [(Some 0, Some 0); (Some 1, Some 1); (Some 66, Some 66)]
    fold2 [0; 1; 66] [0; 1; 66; 91] -> [(Some 0, Some 0); (Some 1, Some 1); (Some 66, Some 66); (None, Some 91)]
    fold2 [0; 1; 66; 91] [0; 1; 66; 91] -> [(Some 0, Some 0); (Some 1, Some 1); (Some 66, Some 66); (Some 91, Some 91)]
    |}]

let%expect_test "iter2,equal,subset,disjoint" =
  let open Format in
  printf "@[";
  let test_equal ms0 ms1 = begin
    let set0 = of_list (module Usize) ms0 in
    let set1 = of_list (module Usize) ms1 in
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
    let set0 = of_list (module Usize) ms0 in
    let set1 = of_list (module Usize) ms1 in
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
  printf "@]";

  [%expect{|
    |}]

let%expect_test "union" =
  let test ms0 ms1 = begin
    let set0 = of_list (module Usize) ms0 in
    let set1 = of_list (module Usize) ms1 in
    let set = union set0 set1 in
    let ms = to_list set in
    List.iter ms0 ~f:(fun m -> assert ((mem m set) && (mem m set0)));
    List.iter ms1 ~f:(fun m -> assert ((mem m set) && (mem m set1)));
    List.iter ms ~f:(fun m -> assert ((mem m set0) || (mem m set1)));
  end in
  let test_disjoint ms0 ms1 = begin
    let set0 = of_list (module Usize) ms0 in
    let set1 = of_list (module Usize) ms1 in
    let set = union set0 set1 in
    assert ((length set) = (length set0) + (length set1));
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
  );

  [%expect{|
    |}]

let%expect_test "inter" =
  let test ms0 ms1 = begin
    let set0 = of_list (module Usize) ms0 in
    let set1 = of_list (module Usize) ms1 in
    let set = inter set0 set1 in
    let ms = to_list set in
    List.iter ms0 ~f:(fun m -> assert ((mem m set) || (not (mem m set1))));
    List.iter ms1 ~f:(fun m -> assert ((mem m set) || (not (mem m set0))));
    List.iter ms ~f:(fun m -> assert ((mem m set0) && (mem m set1)));
  end in
  let test_disjoint ms0 ms1 = begin
    let set0 = of_list (module Usize) ms0 in
    let set1 = of_list (module Usize) ms1 in
    let set = inter set0 set1 in
    assert ((length set) = 0);
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
  );

  [%expect{|
    |}]

let%expect_test "diff" =
  let test ms0 ms1 = begin
    let set0 = of_list (module Usize) ms0 in
    let set1 = of_list (module Usize) ms1 in
    let set = diff set0 set1 in
    let ms = to_list set in
    List.iter ms0 ~f:(fun m -> assert ((mem m set) || (mem m set1)));
    List.iter ms1 ~f:(fun m -> assert (not (mem m set)));
    List.iter ms ~f:(fun m -> assert ((mem m set0) && (not (mem m set1))));
  end in
  let test_disjoint ms0 ms1 = begin
    let set0 = of_list (module Usize) ms0 in
    let set1 = of_list (module Usize) ms1 in
    let set = diff set0 set1 in
    assert ((length set) = (length set0));
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
  List.iteri test_lists ~f:(fun i ms0 ->
    List.iteri test_lists ~f:(fun j ms1 ->
      if i <= j then begin
        test ms0 ms1;
        test ms1 ms0;
      end
    )
  );
  List.iter test_disjoint_list_pairs ~f:(fun (ms0, ms1) ->
    test_disjoint ms0 ms1;
    test_disjoint ms0 (List.rev ms1);
    test_disjoint (List.rev ms0) ms1;
    test_disjoint (List.rev ms0) (List.rev ms1);
  );

  [%expect{|
    |}]

let%expect_test "reduce" =
  let open Format in
  printf "@[<h>";
  let test ms = begin
    let set = of_list (module Usize) ms in
    let sum = reduce ~f:( + ) set in
    printf "reduce ~f:( + ) %a -> %a\n"
      (List.pp Usize.pp) ms
      (Option.pp Usize.pp) sum
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
  ] in
  List.iter test_lists ~f:(fun ms ->
    test ms
  );
  printf "@]";

  [%expect{|
    reduce ~f:( + ) [] -> None
    reduce ~f:( + ) [0] -> Some 0
    reduce ~f:( + ) [0; 1] -> Some 1
    reduce ~f:( + ) [0; 1; 2] -> Some 3
    reduce ~f:( + ) [0; 1; 66] -> Some 67
    reduce ~f:( + ) [0; 1; 66; 91] -> Some 158
    |}]

let%expect_test "stress" =
  let open Format in
  printf "@[";
  (* test is n^2 time complexity, so keep n small. *)
  let rec test n i e set = begin
    match i < n with
    | false -> set
    | true -> begin
        let set' = remove i (test n (succ i) e (insert i set)) in
        assert (equal set set');
        assert (equal set (union set set'));
        assert (equal set (inter set set'));
        assert (equal e (diff set set'));
        set'
      end
  end in
  let e = empty (module Usize) in
  let _ = test 100 0 e e in
  printf "@]";

  [%expect{|
    |}]
