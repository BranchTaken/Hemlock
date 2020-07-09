open Rudiments

module T = struct
  type ('a, 'cmp) t = ('a, unit, 'cmp) Ordmap.t
  type 'a elm = 'a

  type ('a, 'cmp) cmper = ('a, 'cmp) Ordmap.cmper

  let length = Ordmap.length

  let is_empty = Ordmap.is_empty

  module Cursor = struct
    module T = struct
      type ('a, 'cmp) container = ('a, 'cmp) t
      type ('a, 'cmp) t = ('a, unit, 'cmp) Ordmap.Cursor.t

      let cmp = Ordmap.Cursor.cmp

      let hd = Ordmap.Cursor.hd

      let tl = Ordmap.Cursor.tl

      let seek = Ordmap.Cursor.seek

      let succ = Ordmap.Cursor.succ

      let pred = Ordmap.Cursor.pred

      let lget t =
        let (a, _) = Ordmap.Cursor.lget t in
        a

      let rget t =
        let (a, _) = Ordmap.Cursor.rget t in
        a

      let container = Ordmap.Cursor.container

      let index = Ordmap.Cursor.index

      let pp ppf t =
        Format.fprintf ppf "@[<h>{index=%a}@]"
          Usize.pp (index t)
    end
    include T
    include Cmpable.Make_poly2(T)
  end
end
include T
include Container_array.Make_poly2_array(T)

let fold_until ~init ~f t =
  Ordmap.fold_until ~init ~f:(fun accum (k, _) -> f accum k) t

let fold_right_until ~init ~f t =
  Ordmap.fold_right_until ~init ~f:(fun (k, _) accum -> f k accum) t

let foldi_until ~init ~f t =
  Ordmap.foldi_until ~init ~f:(fun i accum (k, _) -> f i accum k) t

let fold ~init ~f t =
  Ordmap.fold ~init ~f:(fun accum (k, _) -> f accum k) t

let fold_right ~init ~f t =
  Ordmap.fold_right ~init ~f:(fun (k, _) accum -> f k accum) t

let foldi ~init ~f t =
  Ordmap.foldi ~init ~f:(fun i accum (k, _) -> f i accum k) t

let iter ~f t =
  Ordmap.iter ~f:(fun (k, _) -> f k) t

let iteri ~f t =
  Ordmap.iteri ~f:(fun i (k, _) -> f i k) t

let count ~f t =
  Ordmap.count ~f:(fun (k, _) -> f k) t

let for_any ~f t =
  Ordmap.for_any ~f:(fun (k, _) -> f k) t

let for_all ~f t =
  Ordmap.for_all ~f:(fun (k, _) -> f k) t

let find ~f t =
  match Ordmap.find ~f:(fun (k, _) -> f k) t with
  | Some (k, _) -> Some k
  | None -> None

let find_map ~f t =
  Ordmap.find_map ~f:(fun (k, _) -> f k) t

let findi ~f t =
  match Ordmap.findi ~f:(fun i (k, _) -> f i k) t with
  | Some (k, _) -> Some k
  | None -> None

let findi_map ~f t =
  Ordmap.findi_map ~f:(fun i (k, _) -> f i k) t

let min_elm ~cmp t =
  match Ordmap.min_elm ~cmp:(fun (k0, _) (k1, _) -> cmp k0 k1) t with
  | Some (k, _) -> Some k
  | None -> None

let max_elm ~cmp t =
  match Ordmap.max_elm ~cmp:(fun (k0, _) (k1, _) -> cmp k0 k1) t with
  | Some (k, _) -> Some k
  | None -> None

let to_list t =
  fold_right t ~init:[] ~f:(fun elm accum -> elm :: accum)

let to_list_rev t =
  fold t ~init:[] ~f:(fun accum elm -> elm :: accum)

let hash_fold t state =
  Ordmap.hash_fold Unit.hash_fold t state

let cmper_m = Ordmap.cmper_m

let cmper = Ordmap.cmper

let empty = Ordmap.empty

let singleton m a =
  Ordmap.singleton m ~k:a ~v:()

let mem = Ordmap.mem

let choose t =
  match Ordmap.choose t with
  | Some (k, _) -> Some k
  | None -> None

let choose_hlt t =
  let k, _ = Ordmap.choose_hlt t in
  k

let nth_opt i t =
  match Ordmap.nth_opt i t with
  | Some (a, _) -> Some a
  | None -> None

let nth i t =
  let a, _ = Ordmap.nth i t in
  a

let psearch = Ordmap.psearch

let search = Ordmap.search

let nsearch = Ordmap.nsearch

(* Seq. *)
module Seq_poly2_fold2 = struct
  type ('a, 'cmp) container = ('a, 'cmp) t
  type 'a elm = 'a
  type ('a, 'cmp) t = ('a, unit, 'cmp) Ordmap.Seq.t

  let init = Ordmap.Seq.init

  let length = Ordmap.Seq.length

  let next t =
    let (a, _), t' = Ordmap.Seq.next t in
    a, t'

  let next_opt t =
    match Ordmap.Seq.next_opt t with
    | Some ((a, _), t') -> Some (a, t')
    | None -> None

  let cmper = Ordmap.Seq.cmper

  let cmp = Ordmap.Seq.cmp
end
include Seq.Make_poly2_fold2(Seq_poly2_fold2)

let cmp t0 t1 =
  Ordmap.cmp Unit.cmp t0 t1

let equal t0 t1 =
  Ordmap.equal Unit.( = ) t0 t1

let subset t0 t1 =
  Ordmap.subset Unit.( = ) t0 t1

let disjoint t0 t1 =
  Ordmap.disjoint t0 t1

let insert a t =
  Ordmap.insert ~k:a ~v:() t

let of_list m alist =
  match alist with
  | [] -> empty m
  | a :: alist' -> begin
      let rec fn alist ordset = begin
        match alist with
        | [] -> ordset
        | a :: alist' -> fn alist' (insert a ordset)
      end in
      fn alist' (singleton m a)
    end

let remove = Ordmap.remove

let split a t =
  match Ordmap.split a t with
  | l, Some (a, _), r -> l, Some a, r
  | l, None, r -> l, None, r

let union t0 t1 =
  Ordmap.union ~f:(fun _ _ _ -> ()) t0 t1

let of_array m arr =
  match arr with
  | [||] -> empty m
  | _ -> Array.reduce_hlt (Array.map arr ~f:(fun a -> singleton m a))
    ~f:(fun ordset0 ordset1 -> union ordset0 ordset1)

let inter t0 t1 =
  Ordmap.inter ~f:(fun _ _ _ -> ()) t0 t1

let diff = Ordmap.diff

let filter ~f t =
  Ordmap.filter ~f:(fun (a, _) -> f a) t

let filteri ~f t =
  Ordmap.filteri ~f:(fun i (a, _) -> f i a) t

let partition_tf ~f t =
  Ordmap.partition_tf ~f:(fun (a, _) -> f a) t

let partitioni_tf ~f t =
  Ordmap.partitioni_tf ~f:(fun i (a, _) -> f i a) t

let reduce ~f t =
  Ordmap.kreduce ~f t

let reduce_hlt ~f t =
  Ordmap.kreduce_hlt ~f t

(******************************************************************************)
(* Begin tests. *)

let pp ppf t =
  let open Format in
  fprintf ppf "@[<h>Ordset {";
  let cmper = Ordmap.cmper t in
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
    | arr :: arrs' -> begin
        let ordset = of_array (module Usize) arr in
        printf "hash_fold (of_array (module Usize) %a) -> %a@\n"
          (Array.pp Usize.pp) arr
          Hash.pp (Hash.t_of_state (hash_fold ordset Hash.State.empty));
        fn arrs'
      end
  in
  let arrs = [
    [||];
    [|0|];
    [|0; 1|];
    [|0; 2|]
  ] in
  fn arrs;
  printf "@]";

  [%expect{|
    hash_fold (of_array (module Usize) [||]) -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold (of_array (module Usize) [|0|]) -> 0xf931_3f2a_e691_89fb_c121_c10c_2321_2ab7u128
    hash_fold (of_array (module Usize) [|0; 1|]) -> 0x6c19_38ef_f4fd_7d4d_0b71_57d7_a7a4_58ceu128
    hash_fold (of_array (module Usize) [|0; 2|]) -> 0xb35d_e06b_2347_e0fb_dc0e_7169_33fc_b370u128
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
    Ordset {}
    Ordset {0}
    |}]

let%expect_test "mem,insert,subset" =
  let open Format in
  printf "@[";
  let rec test ms ordset = begin
    match ms with
    | [] -> printf "%a@\n" pp ordset
    | m :: ms' -> begin
        assert (not (mem m ordset));
        let ordset' = insert m ordset in
        assert (mem m ordset');
        assert (subset ordset' ordset);
        assert (not (subset ordset ordset'));
        test ms' ordset'
      end
  end in
  let ms = [1; 3; 2; 44; 45; 56; 60; 66; 75; 81; 91] in
  test ms (empty (module Usize));
  printf "@]";

  [%expect{|
    Ordset {1; 2; 3; 44; 45; 56; 60; 66; 75; 81; 91}
    |}]

let%expect_test "of_list duplicate" =
  let open Format in
  printf "@[";
  printf "%a@\n" pp (of_list (module Usize) [0; 0]);
  printf "@]";

  [%expect{|
    Ordset {0}
    |}]

let%expect_test "of_list,remove" =
  let open Format in
  printf "@[";
  let test m ordset descr = begin
    printf "--- %s ---@\n" descr;
    let ordset' = remove m ordset in
    printf "@[<v>remove %a@;<0 2>@[<v>%a ->@,%a@]@]@\n"
      Usize.pp m pp ordset pp ordset'
  end in
  let test_tuples = [
    ([0; 1], 2,            "Not member.");
    ([0], 0,               "Member, length 1 -> 0.");
    ([0; 1], 1,            "Member, length 2 -> 1.");
    ([0; 1; 2], 2,         "Member, length 3 -> 2.");
  ] in
  List.iter test_tuples ~f:(fun (ms, m, descr) ->
    let ordset = of_list (module Usize) ms in
    test m ordset descr
  );
  printf "@]";

  [%expect{|
    --- Not member. ---
    remove 2
      Ordset {0; 1} ->
      Ordset {0; 1}
    --- Member, length 1 -> 0. ---
    remove 0
      Ordset {0} ->
      Ordset {}
    --- Member, length 2 -> 1. ---
    remove 1
      Ordset {0; 1} ->
      Ordset {0}
    --- Member, length 3 -> 2. ---
    remove 2
      Ordset {0; 1; 2} ->
      Ordset {0; 1}
    |}]

let%expect_test "of_array,cursor" =
  let open Format in
  printf "@[";
  let test_fwd ordset = begin
    let rec fn cursor = begin
      match Cursor.(cursor = (tl ordset)) with
      | true -> printf "@\n"
      | false -> begin
          let i = Cursor.index cursor in
          assert Cursor.((seek (Usize.to_isize i) (hd ordset)) = cursor);
          printf "            %a=%a@\n"
            Cursor.pp cursor
            Usize.pp (Cursor.rget cursor);
          fn (Cursor.succ cursor)
        end
    end in
    printf "cursor fwd:@\n";
    fn (Cursor.hd ordset);
  end in
  let test_rev ordset = begin
    let rec fn cursor = begin
      match Cursor.(cursor = (hd ordset)) with
      | true -> printf "@\n"
      | false -> begin
          let i = Cursor.index cursor in
          assert Cursor.((seek (Usize.to_isize i) (hd ordset)) = cursor);
          printf "            %a=%a@\n"
            Cursor.pp cursor
            Usize.pp (Cursor.lget cursor);
          fn (Cursor.pred cursor)
        end
    end in
    printf "cursor rev:@\n";
    fn (Cursor.tl ordset);
  end in
  let test ms = begin
    let ordset = of_array (module Usize) ms in
    printf "of_array %a -> @,%a@\n"
      (Array.pp Usize.pp) ms
      pp ordset;
    test_fwd ordset;
    test_rev ordset
  end in
  let test_arrays = [
    [||];
    [|0; 1; 4; 5; 3; 2|];
  ] in
  List.iter test_arrays ~f:(fun ms ->
    test ms
  );
  printf "@]";

  [%expect{|
    of_array [||] ->
    Ordset {}
    cursor fwd:

    cursor rev:

    of_array [|0; 1; 4; 5; 3; 2|] ->
    Ordset {0; 1; 2; 3; 4; 5}
    cursor fwd:
                {index=0}=0
                {index=1}=1
                {index=2}=2
                {index=3}=3
                {index=4}=4
                {index=5}=5

    cursor rev:
                {index=6}=5
                {index=5}=4
                {index=4}=3
                {index=3}=2
                {index=2}=1
                {index=1}=0
    |}]

let%expect_test "of_list,to_list,to_array" =
  let open Format in
  printf "@[<h>";
  let test ms = begin
    let ordset = of_list (module Usize) ms in
    printf "of_list %a; to_list -> %a; to_array -> %a\n"
      (List.pp Usize.pp) ms
      (List.pp Usize.pp) (to_list ordset)
      (Array.pp Usize.pp) (to_array ordset)
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

let%expect_test "search,nth" =
  let open Format in
  let test_search ordset key_max = begin
    printf "%a@\n" pp ordset;
    for probe = 0 to key_max do
      printf "  %a -> %s, %s, %s@\n" Usize.pp probe
        (match psearch probe ordset with
          | None -> "<"
          | Some (Cmp.Lt, i) -> asprintf "<[%a]=%a"
              Usize.pp i Usize.pp (nth i ordset)
          | Some (Cmp.Eq, i) -> asprintf "=[%a]=%a"
              Usize.pp i Usize.pp (nth i ordset)
          | Some (Cmp.Gt, i) -> asprintf ">[%a]=%a"
              Usize.pp i Usize.pp (nth i ordset)
        )
        (match search probe ordset with
          | None -> "<>"
          | Some i -> asprintf "=%a" Usize.pp (nth i ordset)
        )
        (match nsearch probe ordset with
          | Some (Cmp.Lt, i) -> asprintf "<[%a]=%a"
              Usize.pp i Usize.pp (nth i ordset)
          | Some (Cmp.Eq, i) -> asprintf "=[%a]=%a"
              Usize.pp i Usize.pp (nth i ordset)
          | Some (Cmp.Gt, i) -> asprintf ">[%a]=%a"
              Usize.pp i Usize.pp (nth i ordset)
          | None -> ">"
        );
    done
  end in
  printf "@[";
  for len = 0 to 3 do
    let ordset = of_array (module Usize)
      (Array.init len ~f:(fun i -> i * 2 + 1)) in
    let key_max = len * 2 in
    test_search ordset key_max
  done;
  printf "@]";

  [%expect{|
    Ordset {}
      0 -> <, <>, >
    Ordset {1}
      0 -> <[0]=1, <>, <[0]=1
      1 -> =[0]=1, =1, =[0]=1
      2 -> >[0]=1, <>, >[0]=1
    Ordset {1; 3}
      0 -> <[0]=1, <>, <[0]=1
      1 -> =[0]=1, =1, =[0]=1
      2 -> >[0]=1, <>, <[1]=3
      3 -> =[1]=3, =3, =[1]=3
      4 -> >[1]=3, <>, >[1]=3
    Ordset {1; 3; 5}
      0 -> <[0]=1, <>, <[0]=1
      1 -> =[0]=1, =1, =[0]=1
      2 -> >[0]=1, <>, <[1]=3
      3 -> =[1]=3, =3, =[1]=3
      4 -> >[1]=3, <>, <[2]=5
      5 -> =[2]=5, =5, =[2]=5
      6 -> >[2]=5, <>, >[2]=5
    |}]

let%expect_test "choose_hlt" =
  let open Format in
  printf "@[";
  (* test is n^2 time complexity, so keep n small. *)
  let rec test n i ordset = begin
    match i < n with
    | false -> ordset
    | true -> begin
        let ordset' = test n (succ i) (insert i ordset) in
        let m = choose_hlt ordset' in
        let ordset'' = remove m ordset' in
        assert ((length ordset') = (length ordset'') + 1);
        ordset''
      end
  end in
  let e = empty (module Usize) in
  let _ = test 100 0 e in
  printf "@]";

  [%expect{|
    |}]

let%expect_test "fold_until" =
  let test ms = begin
    let ordset = of_list (module Usize) ms in
    (* Compute the number of elements in the triangle defined by folding n
     * times, each time terminating upon encounter of a distinct set member.
     * The size of the triangle is insensitive to fold order. *)
    assert ((List.length ms) = (length ordset));
    let n = length ordset in
    let triangle_sum = List.fold ms ~init:0 ~f:(fun accum m ->
      accum + fold_until ordset ~init:0 ~f:(fun accum a ->
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

let%expect_test "fold_right_until" =
  let test ms = begin
    let ordset = of_list (module Usize) ms in
    (* Compute the number of elements in the triangle defined by folding n
     * times, each time terminating upon encounter of a distinct set member.
     * The size of the triangle is insensitive to fold order. *)
    assert ((List.length ms) = (length ordset));
    let n = length ordset in
    let triangle_sum = List.fold ms ~init:0 ~f:(fun accum m ->
      accum + fold_right_until ordset ~init:0 ~f:(fun a accum ->
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
    let ordset0 = of_list (module Usize) ms0 in
    let ordset1 = of_list (module Usize) ms1 in
    let ordset = union ordset0 ordset1 in
    let ms = to_list ordset in
    (* Compute the number of elements in the triangle defined by folding n
     * times, each time terminating upon encounter of a distinct set member.
     * The size of the triangle is insensitive to fold order. *)
    assert ((List.length ms) = (length ordset));
    let n = length ordset in
    let triangle_sum = List.fold ms ~init:0 ~f:(fun accum m ->
      accum + fold2_until ordset0 ordset1 ~init:0 ~f:(fun accum a0_opt a1_opt ->
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
    let ordset0 = of_list (module Usize) ms0 in
    let ordset1 = of_list (module Usize) ms1 in
    let pairs = fold2 ~init:[] ~f:(fun accum a0_opt a1_opt ->
      (a0_opt, a1_opt) :: accum
    ) ordset0 ordset1 in
    printf "fold2 %a %a -> %a@\n"
      (List.pp Usize.pp) ms0
      (List.pp Usize.pp) ms1
      (List.pp pp_pair) pairs
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
    fold2 [] [0; 1] -> [(None, Some 1); (None, Some 0)]
    fold2 [] [0; 1; 2] -> [(None, Some 2); (None, Some 1); (None, Some 0)]
    fold2 [] [0; 1; 66] -> [(None, Some 66); (None, Some 1); (None, Some 0)]
    fold2 [] [0; 1; 66; 91] -> [(None, Some 91); (None, Some 66); (None, Some 1); (None, Some 0)]
    fold2 [0] [0] -> [(Some 0, Some 0)]
    fold2 [0] [0; 1] -> [(None, Some 1); (Some 0, Some 0)]
    fold2 [0] [0; 1; 2] -> [(None, Some 2); (None, Some 1); (Some 0, Some 0)]
    fold2 [0] [0; 1; 66] -> [(None, Some 66); (None, Some 1); (Some 0, Some 0)]
    fold2 [0] [0; 1; 66; 91] -> [(None, Some 91); (None, Some 66); (None, Some 1); (Some 0, Some 0)]
    fold2 [0; 1] [0; 1] -> [(Some 1, Some 1); (Some 0, Some 0)]
    fold2 [0; 1] [0; 1; 2] -> [(None, Some 2); (Some 1, Some 1); (Some 0, Some 0)]
    fold2 [0; 1] [0; 1; 66] -> [(None, Some 66); (Some 1, Some 1); (Some 0, Some 0)]
    fold2 [0; 1] [0; 1; 66; 91] -> [(None, Some 91); (None, Some 66); (Some 1, Some 1); (Some 0, Some 0)]
    fold2 [0; 1; 2] [0; 1; 2] -> [(Some 2, Some 2); (Some 1, Some 1); (Some 0, Some 0)]
    fold2 [0; 1; 2] [0; 1; 66] -> [(None, Some 66); (Some 2, None); (Some 1, Some 1); (Some 0, Some 0)]
    fold2 [0; 1; 2] [0; 1; 66; 91] -> [(None, Some 91); (None, Some 66); (Some 2, None); (Some 1, Some 1); (Some 0, Some 0)]
    fold2 [0; 1; 66] [0; 1; 66] -> [(Some 66, Some 66); (Some 1, Some 1); (Some 0, Some 0)]
    fold2 [0; 1; 66] [0; 1; 66; 91] -> [(None, Some 91); (Some 66, Some 66); (Some 1, Some 1); (Some 0, Some 0)]
    fold2 [0; 1; 66; 91] [0; 1; 66; 91] -> [(Some 91, Some 91); (Some 66, Some 66); (Some 1, Some 1); (Some 0, Some 0)]
    |}]

let%expect_test "iter2,equal,subset,disjoint" =
  let open Format in
  printf "@[";
  let test_equal ms0 ms1 = begin
    let ordset0 = of_list (module Usize) ms0 in
    let ordset1 = of_list (module Usize) ms1 in
    assert (equal ordset0 ordset1);
    assert (subset ordset0 ordset1);
    assert (subset ordset1 ordset0);
    assert ((length ordset0 = 0) || (not (disjoint ordset0 ordset1)));
    iter2 ~f:(fun a0_opt a1_opt ->
      match a0_opt, a1_opt with
      | Some _, Some _ -> ()
      | None, Some _
      | Some _, None -> begin
          printf "Should be equal:@,%a@,%a@\n" pp ordset0 pp ordset1;
          assert false;
        end
      | None, None -> not_reached ()
    ) ordset0 ordset1
  end in
  let test_disjoint ms0 ms1 = begin
    let ordset0 = of_list (module Usize) ms0 in
    let ordset1 = of_list (module Usize) ms1 in
    assert (not (equal ordset0 ordset1));
    assert (not (subset ordset0 ordset1));
    assert ((length ordset0 = 0) || (not (subset ordset1 ordset0)));
    assert (disjoint ordset0 ordset1);
    iter2 ~f:(fun a0_opt a1_opt ->
      match a0_opt, a1_opt with
      | Some _, Some _ -> begin
          printf "Should be disjoint:@,%a@,%a@\n" pp ordset0 pp ordset1;
          assert false;
        end
      | None, Some _
      | Some _, None -> ()
      | None, None -> not_reached ()
    ) ordset0 ordset1
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
    let ordset0 = of_list (module Usize) ms0 in
    let ordset1 = of_list (module Usize) ms1 in
    let ordset = union ordset0 ordset1 in
    let ms = to_list ordset in
    List.iter ms0 ~f:(fun m -> assert ((mem m ordset) && (mem m ordset0)));
    List.iter ms1 ~f:(fun m -> assert ((mem m ordset) && (mem m ordset1)));
    List.iter ms ~f:(fun m -> assert ((mem m ordset0) || (mem m ordset1)));
  end in
  let test_disjoint ms0 ms1 = begin
    let ordset0 = of_list (module Usize) ms0 in
    let ordset1 = of_list (module Usize) ms1 in
    let ordset = union ordset0 ordset1 in
    assert ((length ordset) = (length ordset0) + (length ordset1));
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
    let ordset0 = of_list (module Usize) ms0 in
    let ordset1 = of_list (module Usize) ms1 in
    let ordset = inter ordset0 ordset1 in
    let ms = to_list ordset in
    List.iter ms0 ~f:(fun m ->
      assert ((mem m ordset) || (not (mem m ordset1))));
    List.iter ms1 ~f:(fun m ->
      assert ((mem m ordset) || (not (mem m ordset0))));
    List.iter ms ~f:(fun m -> assert ((mem m ordset0) && (mem m ordset1)));
  end in
  let test_disjoint ms0 ms1 = begin
    let ordset0 = of_list (module Usize) ms0 in
    let ordset1 = of_list (module Usize) ms1 in
    let ordset = inter ordset0 ordset1 in
    assert ((length ordset) = 0);
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
    let ordset0 = of_list (module Usize) ms0 in
    let ordset1 = of_list (module Usize) ms1 in
    let ordset = diff ordset0 ordset1 in
    let ms = to_list ordset in
    List.iter ms0 ~f:(fun m -> assert ((mem m ordset) || (mem m ordset1)));
    List.iter ms1 ~f:(fun m -> assert (not (mem m ordset)));
    List.iter ms ~f:(fun m ->
      assert ((mem m ordset0) && (not (mem m ordset1))));
  end in
  let test_disjoint ms0 ms1 = begin
    let ordset0 = of_list (module Usize) ms0 in
    let ordset1 = of_list (module Usize) ms1 in
    let ordset = diff ordset0 ordset1 in
    assert ((length ordset) = (length ordset0));
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

let%expect_test "filter" =
  let open Format in
  printf "@[<h>";
  let test arr = begin
    let ordset = of_array (module Usize) arr in
    let ordset' = filter ordset ~f:(fun mem -> mem % 2 = 0) in
    let arr' = to_array ordset' in
    printf "%a -> %a@\n"
      (Array.pp Usize.pp) arr
      (Array.pp Usize.pp) arr'
  end in
  for n = 0 to 6 do
    let arr = Array.init n ~f:(fun i -> i) in
    test arr
  done;
  printf "@]";

  [%expect{|
    [||] -> [||]
    [|0|] -> [|0|]
    [|0; 1|] -> [|0|]
    [|0; 1; 2|] -> [|0; 2|]
    [|0; 1; 2; 3|] -> [|0; 2|]
    [|0; 1; 2; 3; 4|] -> [|0; 2; 4|]
    [|0; 1; 2; 3; 4; 5|] -> [|0; 2; 4|]
    |}]

let%expect_test "filteri" =
  let open Format in
  printf "@[<h>";
  let test arr = begin
    let ordset = of_array (module Usize) arr in
    let ordset' = filteri ordset ~f:(fun i _mem -> i % 2 = 0) in
    let arr' = to_array ordset' in
    printf "%a -> %a@\n"
      (Array.pp Usize.pp) arr
      (Array.pp Usize.pp) arr'
  end in
  for n = 0 to 6 do
    let arr = Array.init n ~f:(fun i -> i * 10) in
    test arr
  done;
  printf "@]";

  [%expect{|
    [||] -> [||]
    [|0|] -> [|0|]
    [|0; 10|] -> [|0|]
    [|0; 10; 20|] -> [|0; 20|]
    [|0; 10; 20; 30|] -> [|0; 20|]
    [|0; 10; 20; 30; 40|] -> [|0; 20; 40|]
    [|0; 10; 20; 30; 40; 50|] -> [|0; 20; 40|]
    |}]

let%expect_test "partition_tf" =
  let open Format in
  printf "@[<h>";
  let test arr = begin
    let ordset = of_array (module Usize) arr in
    let t_ordset, f_ordset = partition_tf ordset ~f:(fun mem -> mem % 2 = 0) in
    let t_arr = to_array t_ordset in
    let f_arr = to_array f_ordset in
    printf "%a -> %a / %a@\n"
      (Array.pp Usize.pp) arr
      (Array.pp Usize.pp) t_arr
      (Array.pp Usize.pp) f_arr
  end in
  for n = 0 to 6 do
    let arr = Array.init n ~f:(fun i -> i) in
    test arr
  done;
  printf "@]";

  [%expect{|
    [||] -> [||] / [||]
    [|0|] -> [|0|] / [||]
    [|0; 1|] -> [|0|] / [|1|]
    [|0; 1; 2|] -> [|0; 2|] / [|1|]
    [|0; 1; 2; 3|] -> [|0; 2|] / [|1; 3|]
    [|0; 1; 2; 3; 4|] -> [|0; 2; 4|] / [|1; 3|]
    [|0; 1; 2; 3; 4; 5|] -> [|0; 2; 4|] / [|1; 3; 5|]
    |}]

let%expect_test "partitioni_tf" =
  let open Format in
  printf "@[<h>";
  let test arr = begin
    let ordset = of_array (module Usize) arr in
    let t_ordset, f_ordset = partitioni_tf ordset ~f:(fun i _mem -> i % 2 = 0) in
    let t_arr = to_array t_ordset in
    let f_arr = to_array f_ordset in
    printf "%a -> %a / %a@\n"
      (Array.pp Usize.pp) arr
      (Array.pp Usize.pp) t_arr
      (Array.pp Usize.pp) f_arr
  end in
  for n = 0 to 6 do
    let arr = Array.init n ~f:(fun i -> i * 10) in
    test arr
  done;
  printf "@]";

  [%expect{|
    [||] -> [||] / [||]
    [|0|] -> [|0|] / [||]
    [|0; 10|] -> [|0|] / [|10|]
    [|0; 10; 20|] -> [|0; 20|] / [|10|]
    [|0; 10; 20; 30|] -> [|0; 20|] / [|10; 30|]
    [|0; 10; 20; 30; 40|] -> [|0; 20; 40|] / [|10; 30|]
    [|0; 10; 20; 30; 40; 50|] -> [|0; 20; 40|] / [|10; 30; 50|]
    |}]

let%expect_test "reduce" =
  let open Format in
  printf "@[<h>";
  let test ms = begin
    let ordset = of_list (module Usize) ms in
    let sum = reduce ~f:( + ) ordset in
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
  let rec test n i e ordset = begin
    match i < n with
    | false -> ordset
    | true -> begin
        let ordset' = remove i (test n (succ i) e (insert i ordset)) in
        assert (equal ordset ordset');
        assert (equal ordset (union ordset ordset'));
        assert (equal ordset (inter ordset ordset'));
        assert (equal e (diff ordset ordset'));
        ordset'
      end
  end in
  let e = empty (module Usize) in
  let _ = test 100 0 e e in
  printf "@]";

  [%expect{|
    |}]

let%expect_test "stress2" =
  let open Format in
  printf "@[";
  (* test is n^2 time complexity, so keep n small. *)
  let rec test n i e ordset = begin
    match i < n with
    | false -> ordset
    | true -> begin
        (* Hash i in order to test semi-random insertion order. *)
        let h = Hash.(t_of_state (Usize.hash_fold i State.empty)) in
        let ordset' = remove h (test n (succ i) e (insert h ordset)) in
        assert (equal ordset ordset');
        assert (equal ordset (union ordset ordset'));
        assert (equal ordset (inter ordset ordset'));
        assert (equal e (diff ordset ordset'));
        ordset'
      end
  end in
  let e = empty (module U128) in
  let _ = test 100 0 e e in
  printf "@]";

  [%expect{|
    |}]
