open Rudiments0
open RudimentsInt0

module T = struct
  type 'a t = 'a list
  type 'a elm = 'a

  let length t =
    let rec fn l i =
      match l with
      | [] -> i
      | _ :: tl -> fn tl (Uns.succ i)
    in
    fn t 0

  let rev t =
    let rec fn l r =
      match r with
      | [] -> l
      | hd :: tl -> fn (hd :: l) tl
    in
    fn [] t

  module Cursor = struct
    module T = struct
      type 'a container = 'a t
      type 'a t = {
        list: 'a container;
        left_rev: 'a container;
        right: 'a container;
        index: uns;
      }

      let cmp t0 t1 =
        (* == is excessively vague in OCaml. *)
        assert ((t0.list == t1.list)
                || (Stdlib.( = ) t0.list t1.list));
        Uns.cmp t0.index t1.index

      let hd list =
        {list; left_rev=[]; right=list; index=0}

      let tl list =
        let rec fn l r len = begin
          match r with
          | [] -> l, len
          | hd :: tl -> fn (hd :: l) tl (succ len)
        end in
        let left_rev, len = fn [] list 0 in
        {list; left_rev; right=[]; index=len}

      let pred t =
        match t.left_rev with
        | [] -> halt "At beginning of list"
        | hd :: tl -> {t with left_rev=tl; right=(hd :: t.right);
             index=(Uns.pred t.index)}

      let succ t =
        match t.right with
        | [] -> halt "At end of list"
        | hd :: tl -> {t with left_rev=(hd :: t.left_rev); right=tl;
             index=(Uns.succ t.index)}

      let lget t =
        match t.left_rev with
        | [] -> halt "At beginning of list"
        | hd :: _ -> hd

      let rget t =
        match t.right with
        | [] -> halt "At end of list"
        | hd :: _ -> hd

      let prev t =
        match t.left_rev with
        | [] -> halt "At beginning of list"
        | hd :: tl -> hd, {t with left_rev=tl; right=(hd :: t.right);
             index=(Uns.pred t.index)}

      let next t =
        match t.right with
        | [] -> halt "At end of list"
        | hd :: tl -> hd, {t with left_rev=(hd :: t.left_rev); right=tl;
             index=(Uns.succ t.index)}

      let container t =
        t.list

      let index t =
        t.index

      let seek i t =
        let rec seek_rev i t = begin
          match Sint.(i = (kv 0)) with
          | true -> t
          | false -> seek_rev Sint.(succ i) (pred t)
        end in
        let rec seek_fwd i t = begin
          match Sint.(i = (kv 0)) with
          | true -> t
          | false -> seek_fwd Sint.(pred i) (succ t)
        end in
        match Sint.(cmp i (kv 0)) with
        | Cmp.Lt -> seek_rev i t
        | Cmp.Eq -> t
        | Cmp.Gt -> seek_fwd i t
    end
    include T
    include Cmpable.MakePoly(T)
  end

  let cmp cmp_elm t0 t1 =
    let rec fn t0 t1 = begin
      match t0, t1 with
      | [], [] -> Cmp.Eq
      | [], _ -> Cmp.Lt
      | _, [] -> Cmp.Gt
      | elm0 :: t0', elm1 :: t1' -> begin
          match cmp_elm elm0 elm1 with
          | Cmp.Eq -> fn t0' t1'
          | cmp_result -> cmp_result
        end
    end in
    fn t0 t1

  let cmp_length t0 t1 =
    let rec fn t0 t1 = begin
      match t0, t1 with
      | [], [] -> Cmp.Eq
      | [], _ -> Cmp.Lt
      | _, [] -> Cmp.Gt
      | _ :: t0', _ :: t1' -> fn t0' t1'
    end in
    fn t0 t1

  let cmp_length_with t limit =
    let rec fn t limit = begin
      match t, limit with
      | [], 0 -> Cmp.Eq
      | [], _ -> Cmp.Lt
      | _ :: _, 0 -> Cmp.Gt
      | _ :: t', _ -> fn t' (pred limit)
    end in
    fn t limit
end
include T
include ContainerCommon.MakePolyFold(T)

let hash_fold hash_fold_a t state =
  foldi t ~init:state ~f:(fun i state elm ->
    state
    |> Uns.hash_fold i
    |> hash_fold_a elm
  )
  |> Uns.hash_fold (length t)

let init n ~f =
  let rec fn t rem = begin
    match rem with
    | 0 -> t
    | _ -> begin
        let i = pred rem in
        let t' = (f i) :: t in
        fn t' i
      end
  end in
  fn [] n

let is_empty t =
  (length t) = 0

let hd t =
  match t with
  | [] -> halt "Empty list has no head"
  | hd :: _ -> hd

let tl t =
  match t with
  | [] -> halt "Empty list has no tail"
  | _ :: tl -> tl

let nth_opt i t =
  let rec fn t i = begin
    match t, i with
    | [], _ -> None
    | hd :: _, 0 -> Some hd
    | _ :: t', _ -> fn t' (pred i)
  end in
  fn t i

let nth i t =
  match nth_opt i t with
  | None -> halt "Out of bounds"
  | Some elm -> elm

let push elm t =
  elm :: t

let pop t =
  match t with
  | [] -> halt "Empty list has no head"
  | hd :: tl -> hd, tl

let concat t0 t1 =
  let rec fn left_rev right = begin
    match left_rev with
    | [] -> right
    | hd :: left_rev' -> fn left_rev' (hd :: right)
  end in
  fn (rev t0) t1

let ( @ ) = concat

let rev_concat t0 t1 =
  let rec fn t0 t1 = begin
    match t0 with
    | [] -> t1
    | elm :: t0' -> fn t0' (elm :: t1)
  end in
  fn t0 t1

let concat_unordered = rev_concat

let rev_join ?sep ts concat_fn =
  let rec fn first t ts_rev = begin
    match ts_rev with
    | [] -> t
    | hd :: ts_rev' -> begin
        let t' = match first, sep with
          | true, _
          | _, None -> concat_fn hd t
          | false, Some sep -> concat_fn hd (rev_concat sep t)
        in
        fn false t' ts_rev'
      end
  end in
  fn true [] ts

let join ?sep ts =
  let rev_sep = match sep with
    | None -> None
    | Some sep -> Some (rev sep)
  in
  rev_join ?sep:rev_sep (rev ts) concat

let join_unordered ?sep ts =
  rev_join ?sep ts rev_concat

let zip t0 t1 =
  let rec fn z t0 t1 = begin
    match t0, t1 with
    | [], [] -> z
    | e0 :: t0', e1 :: t1' -> fn ((e0, e1) :: z) t0' t1'
    | _ :: _, []
    | [], _ :: _ -> halt "Unequal list lengths"
  end in
  rev (fn [] t0 t1)

let unzip t =
  let rec fn t0 t1 z = begin
    match z with
    | [] -> t0, t1
    | (e0, e1) :: z' -> fn (e0 :: t0) (e1 :: t1) z'
  end in
  fn [] [] (rev t)

let split i t =
  let rec fn t i = begin
    match t, i with
    | _, 0 -> [], t
    | [], _ -> halt "Out of bounds"
    | (e :: t'), _ -> begin
        let t0, t1 = fn t' (pred i) in
        (e :: t0), t1
      end
  end in
  fn t i

let rev_split i t =
  let rec fn t0 t1 i = begin
    match t1, i with
    | _, 0 -> t0, t1
    | [], _ -> halt "Out of bounds"
    | (e :: t1'), _ -> fn (e :: t0) t1' (pred i)
  end in
  fn [] t i

let split_until ~f t =
  let rec fn t1 = begin
    match t1 with
    | [] -> [], t1
    | elm :: t1' -> begin
        if f elm then [], t1
        else begin
          let t0, t1'' = fn t1' in
          (elm :: t0), t1''
        end
      end
  end in
  fn t

let rev_split_until ~f t =
  let rec fn t0 t1 = begin
    match t1 with
    | [] -> t0, t1
    | elm :: t1' -> begin
        if f elm then t0, t1
        else fn (elm :: t0) t1'
      end
  end in
  fn [] t

let take i t =
  fst (split i t)

let rev_take i t =
  fst (rev_split i t)

let take_until ~f t =
  fst (split_until ~f t)

let rev_take_until ~f t =
  fst (rev_split_until ~f t)

let drop i t =
  let rec fn t1 i = begin
    match t1, i with
    | _, 0 -> t1
    | [], _ -> halt "Out of bounds"
    | (_ :: t1'), _ -> fn t1' (pred i)
  end in
  fn t i

let drop_until ~f t =
  let rec fn t1 = begin
    match t1 with
    | [] -> t1
    | elm :: t1' -> begin
        if f elm then t1
        else fn t1'
      end
  end in
  fn t

let partition_tf ~f t =
  let rec fn t = begin
    match t with
    | [] -> [], []
    | elm :: t' -> begin
        let t_true, t_false = fn t' in
        if f elm then (elm :: t_true), t_false
        else t_true, (elm :: t_false)
      end
  end in
  fn t

let rev_partition_tf ~f t =
  let rec fn t_true t_false t = begin
    match t with
    | [] -> t_true, t_false
    | elm :: t' -> begin
        if f elm then fn (elm :: t_true) t_false t'
        else fn t_true (elm :: t_false) t'
      end
  end in
  fn [] [] t

let groupi ~break t =
  let rec fn_elm elm_left t i break = begin
    match t with
    | [] -> [elm_left], [], i
    | elm_right :: t' -> begin
        if break i elm_left elm_right then [elm_left], t, i
        else begin
          let g, t'', i' = fn_elm elm_right t' (Uns.succ i) break in
          (elm_left :: g), t'', i'
        end
      end
  end and fn_group t i break = begin
  match t with
  | [] -> []
  | elm :: t' -> begin
      let g, t'', i' = fn_elm elm t' (Uns.succ i) break in
      g :: (fn_group t'' i' break)
    end
end in
  fn_group t 0 break

let group ~break t =
  groupi t ~break:(fun _ elm0 elm1 -> break elm0 elm1)

let rev_groupi ~break t =
  let rec fn gs g t i = begin
    match g, t with
    | [], [] -> gs
    | _ :: _, [] -> g :: gs
    | [], elm :: t' -> fn gs [elm] t' (Uns.succ i)
    | elm_left :: _, elm_right :: t' -> begin
        let i' = Uns.succ i in
        if break i elm_left elm_right then fn (g :: gs) [elm_right] t' i'
        else fn gs (elm_right :: g) t' i'
      end
  end in
  fn [] [] t 0

let rev_group ~break t =
  rev_groupi t ~break:(fun _ elm0 elm1 -> break elm0 elm1)

let mapi ~f t =
  let rec fn t i f = begin
    match t with
    | [] -> []
    | elm :: t' -> (f i elm) :: fn t' (Uns.succ i) f
  end in
  fn t 0 f

let map ~f t =
  mapi t ~f:(fun _ elm -> f elm)

let rev_mapi ~f t =
  let rec fn t i f accum = begin
    match t with
    | [] -> accum
    | elm :: t' -> fn t' (Uns.succ i) f ((f i elm) :: accum)
  end in
  fn t 0 f []

let rev_map ~f t =
  rev_mapi t ~f:(fun _ elm -> f elm)

let rev_map_concat ~f t0 t1 =
  let rec fn rem t = begin
    match rem with
    | [] -> t
    | elm :: rem' -> fn rem' ((f elm) :: t)
  end in
  fn t0 t1

let foldi_map ~init ~f t =
  let rec fn a_list i f accum = begin
    match a_list with
    | [] -> accum, []
    | a_elm :: a_list' -> begin
        let i' = Uns.succ i in
        let accum', b_elm = f i accum a_elm in
        let accum'', b_list = fn a_list' i' f accum' in
        accum'', (b_elm :: b_list)
      end
  end in
  fn t 0 f init

let fold_map ~init ~f t =
  foldi_map t ~init ~f:(fun _ accum a -> f accum a)

let rev_foldi_map ~init ~f t =
  let rec fn a_list i f accum b_list = begin
    match a_list with
    | [] -> accum, b_list
    | a_elm :: a_list' -> begin
        let i' = Uns.succ i in
        let accum', b_elm = f i accum a_elm in
        let b_list' = b_elm :: b_list in
        fn a_list' i' f accum' b_list'
      end
  end in
  fn t 0 f init []

let rev_fold_map ~init ~f t =
  rev_foldi_map t ~init ~f:(fun _ accum a -> f accum a)

let filteri ~f t =
  let rec fn t i f = begin
    match t with
    | [] -> []
    | elm :: t' -> begin
        let i' = Uns.succ i in
        let keep = f i elm in
        let accum = fn t' i' f in
        match keep with
        | false -> accum
        | true -> elm :: accum
      end
  end in
  fn t 0 f

let filter ~f t =
  filteri t ~f:(fun _ elm -> f elm)

let rev_filteri ~f t =
  let rec fn t i f accum = begin
    match t with
    | [] -> accum
    | elm :: t' -> begin
        let i' = Uns.succ i in
        match f i elm with
        | false -> fn t' i' f accum
        | true -> fn t' i' f (elm :: accum)
      end
  end in
  fn t 0 f []

let rev_filter ~f t =
  rev_filteri t ~f:(fun _ elm -> f elm)

let foldi2_until ~init ~f t0 t1 =
  let rec fn t0 t1 i f accum = begin
    match t0, t1 with
    | [], [] -> accum
    | _ :: _, []
    | [], _ :: _ -> halt "List lengths differ"
    | elm0 :: t0', elm1 :: t1' -> begin
        let i' = Uns.succ i in
        let accum', until = f i accum elm0 elm1 in
        match until with
        | true -> accum'
        | false -> fn t0' t1' i' f accum'
      end
  end in
  fn t0 t1 0 f init

let fold2_until ~init ~f t0 t1 =
  foldi2_until t0 t1 ~init ~f:(fun _ accum a b -> f accum a b)

let foldi2 ~init ~f t0 t1 =
  foldi2_until t0 t1 ~init
    ~f:(fun i accum elm0 elm1 -> (f i accum elm0 elm1), false)

let fold2 ~init ~f t0 t1 =
  foldi2 t0 t1 ~init ~f:(fun _ accum a b -> f accum a b)

let iteri2 ~f t0 t1 =
  let rec fn t0 t1 i f = begin
    match t0, t1 with
    | [], [] -> ()
    | _ :: _, []
    | [], _ :: _ -> halt "List lengths differ"
    | elm0 :: t0', elm1 :: t1' -> begin
        f i elm0 elm1;
        fn t0' t1' (Uns.succ i) f
      end
  end in
  fn t0 t1 0 f

let iter2 ~f t0 t1 =
  iteri2 t0 t1 ~f:(fun _ a b -> f a b)

let mapi2 ~f t0 t1 =
  let rec fn t0 t1 i f = begin
    match t0, t1 with
    | [], [] -> []
    | _ :: _, []
    | [], _ :: _ -> halt "List lengths differ"
    | elm0 :: t0', elm1 :: t1' -> begin
        let i' = Uns.succ i in
        (f i elm0 elm1) :: (fn t0' t1' i' f)
      end
  end in
  fn t0 t1 0 f

let map2 ~f t0 t1 =
  mapi2 t0 t1 ~f:(fun _ a b -> f a b)

let rev_mapi2 ~f t0 t1 =
  let rec fn t0 t1 i f accum = begin
    match t0, t1 with
    | [], [] -> accum
    | _ :: _, []
    | [], _ :: _ -> halt "List lengths differ"
    | elm0 :: t0', elm1 :: t1' -> begin
        let i' = Uns.succ i in
        let accum' = (f i elm0 elm1) :: accum in
        fn t0' t1' i' f accum'
      end
  end in
  fn t0 t1 0 f []

let rev_map2 ~f t0 t1 =
  rev_mapi2 t0 t1 ~f:(fun _ a b -> f a b)

let foldi2_map ~init ~f t0 t1 =
  let rec fn t0 t1 i f accum = begin
    match t0, t1 with
    | [], [] -> accum, []
    | _ :: _, []
    | [], _ :: _ -> halt "List lengths differ"
    | elm0 :: t0', elm1 :: t1' -> begin
        let i' = Uns.succ i in
        let accum', elm = f i accum elm0 elm1 in
        let accum'', map = fn t0' t1' i' f accum' in
        accum'', (elm :: map)
      end
  end in
  fn t0 t1 0 f init

let fold2_map ~init ~f t0 t1 =
  foldi2_map t0 t1 ~init ~f:(fun _ accum a b -> f accum a b)

let rev_foldi2_map ~init ~f t0 t1 =
  let rec fn t0 t1 i f accum map = begin
    match t0, t1 with
    | [], [] -> accum, map
    | _ :: _, []
    | [], _ :: _ -> halt "List lengths differ"
    | elm0 :: t0', elm1 :: t1' -> begin
        let i' = Uns.succ i in
        let accum', elm = f i accum elm0 elm1 in
        let map' = elm :: map in
        fn t0' t1' i' f accum' map'
      end
  end in
  fn t0 t1 0 f init []

let rev_fold2_map ~init ~f t0 t1 =
  rev_foldi2_map t0 t1 ~init ~f:(fun _ accum a b -> f accum a b)

let pp pp_elm ppf t =
  let open Format in
  let rec pp_elms ppf = function
    | [] -> ()
    | elm :: [] -> fprintf ppf "%a" pp_elm elm
    | elm :: t' -> begin
        fprintf ppf "%a;@ " pp_elm elm;
        pp_elms ppf t'
      end
  in
  fprintf ppf "@[<h>[%a]@]" pp_elms t

module Assoc = struct
  type nonrec ('a, 'b) t = ('a * 'b) t

  let add a b t =
    (a, b) :: t

  let find a ~cmp t =
    let rec fn t a cmp = begin
      match t with
      | [] -> None
      | (k, v) :: t' -> begin
          match cmp a k with
          | Cmp.Eq -> Some v
          | Cmp.Lt
          | Cmp.Gt -> fn t' a cmp
        end
    end in
    fn t a cmp

  let find_hlt a ~cmp t =
    match find a ~cmp t with
    | None -> halt "Key not found"
    | Some v -> v

  let mem a ~cmp t =
    match find a ~cmp t with
    | None -> false
    | Some _ -> true

  let remove_impl a ~cmp t =
    let rec fn t a cmp = begin
      match t with
      | [] -> None
      | elm :: t' -> begin
          let k, _ = elm in
          match cmp a k with
          | Cmp.Eq -> Some t'
          | Cmp.Lt
          | Cmp.Gt -> begin
              match fn t' a cmp with
              | None -> None
              | Some t'' -> Some (elm :: t'')
            end
        end
    end in
    fn t a cmp

  let remove a ~cmp t =
    match remove_impl a ~cmp t with
    | None -> t
    | Some t' -> t'

  let remove_hlt a ~cmp t =
    match remove_impl a ~cmp t with
    | None -> halt "Key not found"
    | Some t' -> t'

  let map ~f t =
    let rec fn t f = begin
      match t with
      | [] -> []
      | (k, v) :: t' -> (k, (f v)) :: (fn t' f)
    end in
    fn t f

  let inverse t =
    let rec fn t result = begin
      match t with
      | [] -> result
      | (k, v) :: t' -> fn t' ((v, k) :: result)
    end in
    fn t []
end

(******************************************************************************)
(* Begin tests. *)

let%expect_test "hash_fold" =
  let open Format in
  printf "@[<h>";
  let rec fn lists = begin
    match lists with
    | [] -> ()
    | l :: lists' -> begin
        printf "hash_fold %a -> %a\n"
          (pp Uns.pp) l
          Hash.pp (Hash.t_of_state
            (hash_fold Uns.hash_fold l Hash.State.empty));
        fn lists'
      end
  end in
  let lists = [
    [];
    [0];
    [0; 0];
    [0; 1]
  ] in
  fn lists;
  printf "@]";

  [%expect{|
    hash_fold [] -> 0xf255_7dfc_c4e8_fe52_28df_63b7_cc57_c3cbu128
    hash_fold [0] -> 0x5813_89d3_d926_9f3c_ffb4_92a6_b118_4dfcu128
    hash_fold [0; 0] -> 0x322f_9602_7919_2b02_9116_8e4e_6e9c_67b0u128
    hash_fold [0; 1] -> 0xa627_52f5_ea2f_d3c0_39a9_536b_6256_1d94u128
    |}]

let%expect_test "hash_fold empty" =
  let hash_empty state = begin
    state
    |> hash_fold Unit.hash_fold []
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
  assert Sint.((u128_compare (Hash.t_of_state e1) (Hash.t_of_state e2)) <>
      kv 0);

  [%expect{|
    |}]

let%expect_test "cmp" =
  let open Format in
  let test_cmp lst0 lst1 = begin
    printf " -> %s\n" (match cmp Uns.cmp lst0 lst1 with
      | Cmp.Lt -> "Lt"
      | Cmp.Eq -> "Eq"
      | Cmp.Gt -> "Gt"
    )
  end in
  let rec test_with_lists lists lists0 lists1 = begin
    match lists0, lists1 with
    | [], _ -> ()
    | _ :: lists0', [] -> test_with_lists lists lists0' lists
    | list0 :: _, list1 :: lists1' -> begin
        printf "cmp %a %a -> " (pp Uns.pp) list0 (pp Uns.pp) list1;
        test_cmp list0 list1;
        test_with_lists lists lists0 lists1'
      end
  end in
  let lists = [
    [];
    [0];
    [1];
    [0; 1];
    [0; 2];
    [1; 2]
  ] in
  printf "@[<h>";
  test_with_lists lists lists lists;
  printf "@]";

  [%expect{|
    cmp [] [] ->  -> Eq
    cmp [] [0] ->  -> Lt
    cmp [] [1] ->  -> Lt
    cmp [] [0; 1] ->  -> Lt
    cmp [] [0; 2] ->  -> Lt
    cmp [] [1; 2] ->  -> Lt
    cmp [0] [] ->  -> Gt
    cmp [0] [0] ->  -> Eq
    cmp [0] [1] ->  -> Lt
    cmp [0] [0; 1] ->  -> Lt
    cmp [0] [0; 2] ->  -> Lt
    cmp [0] [1; 2] ->  -> Lt
    cmp [1] [] ->  -> Gt
    cmp [1] [0] ->  -> Gt
    cmp [1] [1] ->  -> Eq
    cmp [1] [0; 1] ->  -> Gt
    cmp [1] [0; 2] ->  -> Gt
    cmp [1] [1; 2] ->  -> Lt
    cmp [0; 1] [] ->  -> Gt
    cmp [0; 1] [0] ->  -> Gt
    cmp [0; 1] [1] ->  -> Lt
    cmp [0; 1] [0; 1] ->  -> Eq
    cmp [0; 1] [0; 2] ->  -> Lt
    cmp [0; 1] [1; 2] ->  -> Lt
    cmp [0; 2] [] ->  -> Gt
    cmp [0; 2] [0] ->  -> Gt
    cmp [0; 2] [1] ->  -> Lt
    cmp [0; 2] [0; 1] ->  -> Gt
    cmp [0; 2] [0; 2] ->  -> Eq
    cmp [0; 2] [1; 2] ->  -> Lt
    cmp [1; 2] [] ->  -> Gt
    cmp [1; 2] [0] ->  -> Gt
    cmp [1; 2] [1] ->  -> Gt
    cmp [1; 2] [0; 1] ->  -> Gt
    cmp [1; 2] [0; 2] ->  -> Gt
    cmp [1; 2] [1; 2] ->  -> Eq
    |}]

let%expect_test "cmp_length" =
  let open Format in
  let test_cmp_length lst0 lst1 = begin
    printf " -> %s\n" (match cmp_length lst0 lst1 with
      | Cmp.Lt -> "Lt"
      | Cmp.Eq -> "Eq"
      | Cmp.Gt -> "Gt"
    )
  end in
  let rec test_with_lists lists lists0 lists1 = begin
    match lists0, lists1 with
    | [], _ -> ()
    | _ :: lists0', [] -> test_with_lists lists lists0' lists
    | list0 :: _, list1 :: lists1' -> begin
        printf "cmp_length %a %a" (pp Uns.pp) list0 (pp Uns.pp) list1;
        test_cmp_length list0 list1;
        test_with_lists lists lists0 lists1'
      end
  end in
  let lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2]
  ] in
  printf "@[<h>";
  test_with_lists lists lists lists;
  printf "@]";

  [%expect{|
    cmp_length [] [] -> Eq
    cmp_length [] [0] -> Lt
    cmp_length [] [0; 1] -> Lt
    cmp_length [] [0; 1; 2] -> Lt
    cmp_length [0] [] -> Gt
    cmp_length [0] [0] -> Eq
    cmp_length [0] [0; 1] -> Lt
    cmp_length [0] [0; 1; 2] -> Lt
    cmp_length [0; 1] [] -> Gt
    cmp_length [0; 1] [0] -> Gt
    cmp_length [0; 1] [0; 1] -> Eq
    cmp_length [0; 1] [0; 1; 2] -> Lt
    cmp_length [0; 1; 2] [] -> Gt
    cmp_length [0; 1; 2] [0] -> Gt
    cmp_length [0; 1; 2] [0; 1] -> Gt
    cmp_length [0; 1; 2] [0; 1; 2] -> Eq
    |}]

let%expect_test "cmp_length_with" =
  let open Format in
  let test_cmp_length_with lst limit = begin
    printf " (limit=%a -> %s)"
      Uns.pp limit (match cmp_length_with lst limit with
      | Cmp.Lt -> "Lt"
      | Cmp.Eq -> "Eq"
      | Cmp.Gt -> "Gt"
    )
  end in
  let rec test_with_lists lists = begin
    match lists with
    | [] -> ()
    | list :: lists' -> begin
        printf "cmp_length_with %a" (pp Uns.pp) list;
        for limit = 0 to 3 do
          printf "%s" (if limit = 0 then ": " else ", ");
          test_cmp_length_with list limit;
        done;
        printf "\n";
        test_with_lists lists'
      end
  end in
  let lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2]
  ] in
  printf "@[<h>";
  test_with_lists lists;
  printf "@]";

  [%expect{|
    cmp_length_with []:  (limit=0 -> Eq),  (limit=1 -> Lt),  (limit=2 -> Lt),  (limit=3 -> Lt)
    cmp_length_with [0]:  (limit=0 -> Gt),  (limit=1 -> Eq),  (limit=2 -> Lt),  (limit=3 -> Lt)
    cmp_length_with [0; 1]:  (limit=0 -> Gt),  (limit=1 -> Gt),  (limit=2 -> Eq),  (limit=3 -> Lt)
    cmp_length_with [0; 1; 2]:  (limit=0 -> Gt),  (limit=1 -> Gt),  (limit=2 -> Gt),  (limit=3 -> Eq)
    |}]

let%expect_test "init" =
  let open Format in
  for i = 0 to 3 do
    printf "%a\n" (pp Uns.pp) (init i ~f:(fun j -> j));
  done;

  [%expect{|
    []
    [0]
    [0; 1]
    [0; 1; 2]
    |}]

let%expect_test "nth_opt" =
  let open Format in
  let l = [0; 1] in
  for i = 0 to 2 do
    match nth_opt i l with
    | None -> printf "%a -> None\n" Uns.pp i
    | Some x -> printf "%a -> Some %a\n" Uns.pp i Uns.pp x
  done;

  [%expect{|
    0 -> Some 0
    1 -> Some 1
    2 -> None
    |}]

let%expect_test "concat,@,rev_concat" =
  let open Format in
  let list_pairs = [
    ([], []);
    ([0; 1], []);
    ([0; 1], [2; 3]);
    ([], [2; 3]);
  ] in
  printf "@[<h>";
  iter list_pairs ~f:(fun (a, b) ->
    printf "concat %a %a -> %a\n"
      (pp Uns.pp) a
      (pp Uns.pp) b
      (pp Uns.pp) (concat a b);
    printf "       %a %@ %a -> %a\n"
      (pp Uns.pp) a
      (pp Uns.pp) b
      (pp Uns.pp) (a @ b);
    printf "rev_concat %a %a -> %a\n"
      (pp Uns.pp) a
      (pp Uns.pp) b
      (pp Uns.pp) (rev_concat a b);
    (* Brittle test; change in conjunction with implementation. *)
    printf "concat_unordered %a %a -> %a\n"
      (pp Uns.pp) a
      (pp Uns.pp) b
      (pp Uns.pp) (concat_unordered a b)
  );
  printf "@]";

  [%expect{|
    concat [] [] -> []
           [] @ [] -> []
    rev_concat [] [] -> []
    concat_unordered [] [] -> []
    concat [0; 1] [] -> [0; 1]
           [0; 1] @ [] -> [0; 1]
    rev_concat [0; 1] [] -> [1; 0]
    concat_unordered [0; 1] [] -> [1; 0]
    concat [0; 1] [2; 3] -> [0; 1; 2; 3]
           [0; 1] @ [2; 3] -> [0; 1; 2; 3]
    rev_concat [0; 1] [2; 3] -> [1; 0; 2; 3]
    concat_unordered [0; 1] [2; 3] -> [1; 0; 2; 3]
    concat [] [2; 3] -> [2; 3]
           [] @ [2; 3] -> [2; 3]
    rev_concat [] [2; 3] -> [2; 3]
    concat_unordered [] [2; 3] -> [2; 3]
    |}]

let%expect_test "join,join_unordered" =
  let open Format in
  let list_lists = [
    [];

    [[]];
    [[0; 1]];

    [[]; []];
    [[0; 1]; [2; 3]];

    [[]; []; []];
    [[0; 1]; [2; 3]; [4; 5]];
  ] in
  printf "@[<h>";
  iter list_lists ~f:(fun lists ->
    printf "join";
    iter lists ~f:(fun l -> printf " %a" (pp Uns.pp) l);
    printf " -> %a\n" (pp Uns.pp) (join lists);

    printf "join ~sep:[6; 7]";
    iter lists ~f:(fun l -> printf " %a" (pp Uns.pp) l);
    printf " -> %a\n" (pp Uns.pp) (join ~sep:[6; 7] lists);

    (* Brittle test; change in conjunction with implementation. *)
    printf "join_unordered ~sep:[6; 7]";
    iter lists ~f:(fun l -> printf " %a" (pp Uns.pp) l);
    printf " -> %a\n" (pp Uns.pp) (join_unordered ~sep:[6; 7] lists);
  );
  printf "@]";

  [%expect{|
    join -> []
    join ~sep:[6; 7] -> []
    join_unordered ~sep:[6; 7] -> []
    join [] -> []
    join ~sep:[6; 7] [] -> []
    join_unordered ~sep:[6; 7] [] -> []
    join [0; 1] -> [0; 1]
    join ~sep:[6; 7] [0; 1] -> [0; 1]
    join_unordered ~sep:[6; 7] [0; 1] -> [1; 0]
    join [] [] -> []
    join ~sep:[6; 7] [] [] -> [6; 7]
    join_unordered ~sep:[6; 7] [] [] -> [7; 6]
    join [0; 1] [2; 3] -> [0; 1; 2; 3]
    join ~sep:[6; 7] [0; 1] [2; 3] -> [0; 1; 6; 7; 2; 3]
    join_unordered ~sep:[6; 7] [0; 1] [2; 3] -> [3; 2; 7; 6; 1; 0]
    join [] [] [] -> []
    join ~sep:[6; 7] [] [] [] -> [6; 7; 6; 7]
    join_unordered ~sep:[6; 7] [] [] [] -> [7; 6; 7; 6]
    join [0; 1] [2; 3] [4; 5] -> [0; 1; 2; 3; 4; 5]
    join ~sep:[6; 7] [0; 1] [2; 3] [4; 5] -> [0; 1; 6; 7; 2; 3; 6; 7; 4; 5]
    join_unordered ~sep:[6; 7] [0; 1] [2; 3] [4; 5] -> [5; 4; 7; 6; 3; 2; 7; 6; 1; 0]
    |}]

let%expect_test "nth,length,is_empty" =
  let open Format in
  let test_length lst = begin
    printf "[";
    for i = 0 to pred (length lst) do
      if i > 0 then printf "; ";
      printf "%a" Uns.pp (nth i lst);
    done;
    printf "]: length=%a, is_empty=%B\n"
      Uns.pp (length lst) (is_empty lst)
  end in
  test_length [];
  test_length [0];
  test_length [0; 1];
  test_length [0; 1; 2];

  [%expect{|
    []: length=0, is_empty=true
    [0]: length=1, is_empty=false
    [0; 1]: length=2, is_empty=false
    [0; 1; 2]: length=3, is_empty=false
    |}]

let%expect_test "push,pop,hd,tl" =
  let open Format in
  let test_pop_push lst = begin
    printf "%a -> " (pp Uns.pp) lst;
    let hd_, tl_ = pop lst in
    assert (hd_ = (hd lst));
    let () = match cmp Uns.cmp tl_ (tl lst) with
      | Cmp.Eq -> ()
      | _ -> assert false
    in
    printf "%a %a -> %a = %a\n"
      Uns.pp hd_
      (pp Uns.pp) tl_
      (pp Uns.pp) (push hd_ tl_)
      (pp Uns.pp) (hd_ :: tl_)
  end in
  let lists = [
    [0];
    [0; 1];
    [0; 1; 2];
  ] in
  printf "@[<h>";
  iter lists ~f:(fun lst -> test_pop_push lst);
  printf "@]";

  [%expect{|
    [0] -> 0 [] -> [0] = [0]
    [0; 1] -> 0 [1] -> [0; 1] = [0; 1]
    [0; 1; 2] -> 0 [1; 2] -> [0; 1; 2] = [0; 1; 2]
    |}]

let%expect_test "rev" =
  let open Format in
  let lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
  ] in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    printf "rev %a -> %a\n"
      (pp Uns.pp) l
      (pp Uns.pp) (rev l)
  );
  printf "@]";

  [%expect{|
    rev [] -> []
    rev [0] -> [0]
    rev [0; 1] -> [1; 0]
    rev [0; 1; 2] -> [2; 1; 0]
    |}]

let%expect_test "zip,unzip" =
  let open Format in
  let list_pairs = [
    ([], []);
    ([0], [1]);
    ([0; 1], [2; 3]);
    ([0; 1; 2], [3; 4; 5])
  ] in
  let pp_pair ppf (a, b) = fprintf ppf "(%a,@ %a)" Uns.pp a Uns.pp b in
  printf "@[<h>";
  iter list_pairs ~f:(fun (t0, t1) ->
    let z = zip t0 t1 in
    let t0', t1' = unzip z in
    printf "zip/unzip %a %a -> %a -> %a %a\n"
      (pp Uns.pp) t0
      (pp Uns.pp) t1
      (pp pp_pair) z
      (pp Uns.pp) t0'
      (pp Uns.pp) t1'
  );
  printf "@]";

  [%expect{|
    zip/unzip [] [] -> [] -> [] []
    zip/unzip [0] [1] -> [(0, 1)] -> [0] [1]
    zip/unzip [0; 1] [2; 3] -> [(0, 2); (1, 3)] -> [0; 1] [2; 3]
    zip/unzip [0; 1; 2] [3; 4; 5] -> [(0, 3); (1, 4); (2, 5)] -> [0; 1; 2] [3; 4; 5]
    |}]

let%expect_test "split,rev_split,take,rev_take,drop" =
  let open Format in
  let lists = [
    [];
    [0];
    [0; 1]
  ] in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    for i = 0 to length l do
      let a, b = split i l in
      printf "split/take,drop %a %a -> %a, %a / %a, %a\n"
        Uns.pp i
        (pp Uns.pp) l
        (pp Uns.pp) a
        (pp Uns.pp) b
        (pp Uns.pp) (take i l)
        (pp Uns.pp) (drop i l)
      ;

      let a, b = rev_split i l in
      printf "rev_split/rev_take,drop %a %a -> %a, %a / %a, %a\n"
        Uns.pp i
        (pp Uns.pp) l
        (pp Uns.pp) a
        (pp Uns.pp) b
        (pp Uns.pp) (rev_take i l)
        (pp Uns.pp) (drop i l)
    done
  );
  printf "@]";

  [%expect{|
    split/take,drop 0 [] -> [], [] / [], []
    rev_split/rev_take,drop 0 [] -> [], [] / [], []
    split/take,drop 0 [0] -> [], [0] / [], [0]
    rev_split/rev_take,drop 0 [0] -> [], [0] / [], [0]
    split/take,drop 1 [0] -> [0], [] / [0], []
    rev_split/rev_take,drop 1 [0] -> [0], [] / [0], []
    split/take,drop 0 [0; 1] -> [], [0; 1] / [], [0; 1]
    rev_split/rev_take,drop 0 [0; 1] -> [], [0; 1] / [], [0; 1]
    split/take,drop 1 [0; 1] -> [0], [1] / [0], [1]
    rev_split/rev_take,drop 1 [0; 1] -> [0], [1] / [0], [1]
    split/take,drop 2 [0; 1] -> [0; 1], [] / [0; 1], []
    rev_split/rev_take,drop 2 [0; 1] -> [1; 0], [] / [1; 0], []
    |}]

let%expect_test "[rev_]split_until,[rev_]take_until,drop_until" =
  let open Format in
  let lists = [
    [];
    [0];
    [0; 1];
  ] in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    for i = 0 to length l do
      let f elm = (elm >= i) in
      let l0, l1 = split_until ~f l in
      printf ("split_until/take_until,drop_until %a " ^^
          "~f:(fun elm -> elm >= %a) -> %a %a / %a %a\n")
        (pp Uns.pp) l
        Uns.pp i
        (pp Uns.pp) l0
        (pp Uns.pp) l1
        (pp Uns.pp) (take_until ~f l)
        (pp Uns.pp) (drop_until ~f l)
      ;

      let rl0, rl1 = rev_split_until ~f l in
      printf ("rev_split_until/rev_take_until,drop_until %a " ^^
          "~f:(fun elm -> elm >= %a) -> %a %a / %a %a\n")
        (pp Uns.pp) l
        Uns.pp i
        (pp Uns.pp) rl0
        (pp Uns.pp) rl1
        (pp Uns.pp) (rev_take_until ~f l)
        (pp Uns.pp) (drop_until ~f l)
    done
  );
  printf "@]";

  [%expect{|
    split_until/take_until,drop_until [] ~f:(fun elm -> elm >= 0) -> [] [] / [] []
    rev_split_until/rev_take_until,drop_until [] ~f:(fun elm -> elm >= 0) -> [] [] / [] []
    split_until/take_until,drop_until [0] ~f:(fun elm -> elm >= 0) -> [] [0] / [] [0]
    rev_split_until/rev_take_until,drop_until [0] ~f:(fun elm -> elm >= 0) -> [] [0] / [] [0]
    split_until/take_until,drop_until [0] ~f:(fun elm -> elm >= 1) -> [0] [] / [0] []
    rev_split_until/rev_take_until,drop_until [0] ~f:(fun elm -> elm >= 1) -> [0] [] / [0] []
    split_until/take_until,drop_until [0; 1] ~f:(fun elm -> elm >= 0) -> [] [0; 1] / [] [0; 1]
    rev_split_until/rev_take_until,drop_until [0; 1] ~f:(fun elm -> elm >= 0) -> [] [0; 1] / [] [0; 1]
    split_until/take_until,drop_until [0; 1] ~f:(fun elm -> elm >= 1) -> [0] [1] / [0] [1]
    rev_split_until/rev_take_until,drop_until [0; 1] ~f:(fun elm -> elm >= 1) -> [0] [1] / [0] [1]
    split_until/take_until,drop_until [0; 1] ~f:(fun elm -> elm >= 2) -> [0; 1] [] / [0; 1] []
    rev_split_until/rev_take_until,drop_until [0; 1] ~f:(fun elm -> elm >= 2) -> [1; 0] [] / [1; 0] []
  |}]

let%expect_test "[rev_]partition_tf" =
  let open Format in
  let lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 2; 3];
  ] in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    let even x = (x % 2 = 0) in
    let l_true, l_false = partition_tf l ~f:even in
    let rl_true, rl_false = rev_partition_tf l ~f:even in
    printf "[rev_]partition_tf %a ~f:even -> %a %a / %a %a\n"
      (pp Uns.pp) l
      (pp Uns.pp) l_true
      (pp Uns.pp) l_false
      (pp Uns.pp) rl_true
      (pp Uns.pp) rl_false
  );
  printf "@]";

  [%expect{|
    [rev_]partition_tf [] ~f:even -> [] [] / [] []
    [rev_]partition_tf [0] ~f:even -> [0] [] / [0] []
    [rev_]partition_tf [0; 1] ~f:even -> [0] [1] / [0] [1]
    [rev_]partition_tf [0; 1; 2] ~f:even -> [0; 2] [1] / [2; 0] [1]
    [rev_]partition_tf [0; 1; 2; 3] ~f:even -> [0; 2] [1; 3] / [2; 0] [3; 1]
  |}]

let%expect_test "[rev_]group" =
  let open Format in
  let lists = [
    [];
    [0];
    [0; 1];

    [0; 0];

    [0; 0; 0];

    [0; 0; 1; 1];
    [0; 1; 1; 2; 2; 3];

    [0; 0; 0; 0];
  ] in
  let eq x0 x1 = (x0 = x1) in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    printf "[rev_]group %a ~break:eq -> %a / %a\n"
      (pp Uns.pp) l
      (pp (pp Uns.pp)) (group l ~break:eq)
      (pp (pp Uns.pp)) (rev_group l ~break:eq)
  );
  printf "@]";

  [%expect{|
    [rev_]group [] ~break:eq -> [] / []
    [rev_]group [0] ~break:eq -> [[0]] / [[0]]
    [rev_]group [0; 1] ~break:eq -> [[0; 1]] / [[1; 0]]
    [rev_]group [0; 0] ~break:eq -> [[0]; [0]] / [[0]; [0]]
    [rev_]group [0; 0; 0] ~break:eq -> [[0]; [0]; [0]] / [[0]; [0]; [0]]
    [rev_]group [0; 0; 1; 1] ~break:eq -> [[0]; [0; 1]; [1]] / [[1]; [1; 0]; [0]]
    [rev_]group [0; 1; 1; 2; 2; 3] ~break:eq -> [[0; 1]; [1; 2]; [2; 3]] / [[3; 2]; [2; 1]; [1; 0]]
    [rev_]group [0; 0; 0; 0] ~break:eq -> [[0]; [0]; [0]; [0]] / [[0]; [0]; [0]; [0]]
  |}]

let%expect_test "[rev_]groupi" =
  let open Format in
  let lists = [
    [];
    [9];
    [9; 9];

    [0; 1];
    [9; 1; 2; 9];

    [0; 1; 2];
    [9; 1; 2; 9; 4; 5; 9];
  ] in
  let inds i x0 x1 = (i = x1) && (i = succ x0) in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    printf "[rev_]groupi %a ~break:inds -> %a / %a\n"
      (pp Uns.pp) l
      (pp (pp Uns.pp)) (groupi l ~break:inds)
      (pp (pp Uns.pp)) (rev_groupi l ~break:inds)
  );
  printf "@]";

  [%expect{|
    [rev_]groupi [] ~break:inds -> [] / []
    [rev_]groupi [9] ~break:inds -> [[9]] / [[9]]
    [rev_]groupi [9; 9] ~break:inds -> [[9; 9]] / [[9; 9]]
    [rev_]groupi [0; 1] ~break:inds -> [[0]; [1]] / [[1]; [0]]
    [rev_]groupi [9; 1; 2; 9] ~break:inds -> [[9; 1]; [2; 9]] / [[9; 2]; [1; 9]]
    [rev_]groupi [0; 1; 2] ~break:inds -> [[0]; [1]; [2]] / [[2]; [1]; [0]]
    [rev_]groupi [9; 1; 2; 9; 4; 5; 9] ~break:inds -> [[9; 1]; [2; 9; 4]; [5; 9]] / [[9; 5]; [4; 9; 2]; [1; 9]]
  |}]

let%expect_test "[rev_]mapi" =
  let open Format in
  let lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 2; 3];
  ] in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    let f i elm = elm + i * 10 in
    printf "[rev_]mapi %a -> %a / %a\n"
      (pp Uns.pp) l
      (pp Uns.pp) (mapi l ~f)
      (pp Uns.pp) (rev_mapi l ~f)
  );
  printf "@]";

  [%expect{|
    [rev_]mapi [] -> [] / []
    [rev_]mapi [0] -> [0] / [0]
    [rev_]mapi [0; 1] -> [0; 11] / [11; 0]
    [rev_]mapi [0; 1; 2] -> [0; 11; 22] / [22; 11; 0]
    [rev_]mapi [0; 1; 2; 3] -> [0; 11; 22; 33] / [33; 22; 11; 0]
  |}]

let%expect_test "rev_map_concat" =
  let open Format in
  let list_pairs = [
    ([], []);
    ([0; 1], []);
    ([0], [1]);
    ([], [0; 1]);

    ([0; 1; 2], []);
    ([0; 1], [2]);
    ([0], [1; 2]);
    ([], [1; 2; 3]);

    ([0; 1; 2; 3], []);
    ([0; 1; 2], [3]);
    ([0; 1], [2; 3]);
    ([0], [1; 2; 3]);
    ([], [0; 1; 2; 3])
  ] in
  printf "@[<h>";
  iter list_pairs ~f:(fun (a, b) ->
    printf "rev_map_concat %a %a -> %a\n"
      (pp Uns.pp) a
      (pp Uns.pp) b
      (pp Uns.pp) (rev_map_concat a b ~f:(fun elm -> elm + 10))
  );
  printf "@]";

  [%expect{|
    rev_map_concat [] [] -> []
    rev_map_concat [0; 1] [] -> [11; 10]
    rev_map_concat [0] [1] -> [10; 1]
    rev_map_concat [] [0; 1] -> [0; 1]
    rev_map_concat [0; 1; 2] [] -> [12; 11; 10]
    rev_map_concat [0; 1] [2] -> [11; 10; 2]
    rev_map_concat [0] [1; 2] -> [10; 1; 2]
    rev_map_concat [] [1; 2; 3] -> [1; 2; 3]
    rev_map_concat [0; 1; 2; 3] [] -> [13; 12; 11; 10]
    rev_map_concat [0; 1; 2] [3] -> [12; 11; 10; 3]
    rev_map_concat [0; 1] [2; 3] -> [11; 10; 2; 3]
    rev_map_concat [0] [1; 2; 3] -> [10; 1; 2; 3]
    rev_map_concat [] [0; 1; 2; 3] -> [0; 1; 2; 3]
  |}]

let%expect_test "[rev_]fold_mapi" =
  let open Format in
  let lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 2; 3];
  ] in
  printf "@[<h>";
  let f i accum elm = (elm :: accum), (elm + i * 10) in
  iter lists ~f:(fun l ->
    let accum, b_list = foldi_map l ~init:[] ~f in
    printf "    fold_mapi %a -> accum=%a, b_list=%a\n"
      (pp Uns.pp) l
      (pp Uns.pp) accum
      (pp Uns.pp) b_list
    ;

    let accum, b_list = rev_foldi_map l ~init:[] ~f in
    printf "rev_fold_mapi %a -> accum=%a, b_list=%a\n"
      (pp Uns.pp) l
      (pp Uns.pp) accum
      (pp Uns.pp) b_list
  );
  printf "@]";

  [%expect{|
        fold_mapi [] -> accum=[], b_list=[]
    rev_fold_mapi [] -> accum=[], b_list=[]
        fold_mapi [0] -> accum=[0], b_list=[0]
    rev_fold_mapi [0] -> accum=[0], b_list=[0]
        fold_mapi [0; 1] -> accum=[1; 0], b_list=[0; 11]
    rev_fold_mapi [0; 1] -> accum=[1; 0], b_list=[11; 0]
        fold_mapi [0; 1; 2] -> accum=[2; 1; 0], b_list=[0; 11; 22]
    rev_fold_mapi [0; 1; 2] -> accum=[2; 1; 0], b_list=[22; 11; 0]
        fold_mapi [0; 1; 2; 3] -> accum=[3; 2; 1; 0], b_list=[0; 11; 22; 33]
    rev_fold_mapi [0; 1; 2; 3] -> accum=[3; 2; 1; 0], b_list=[33; 22; 11; 0]
  |}]

let%expect_test "[rev_]filteri" =
  let open Format in
  let lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 2; 3];
    [0; 1; 2; 3; 4];
  ] in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    let f i _ = (i % 2 = 0) in
    printf "[rev_]filteri %a -> %a / %a\n"
      (pp Uns.pp) l
      (pp Uns.pp) (filteri l ~f)
      (pp Uns.pp) (rev_filteri l ~f)
  );
  printf "@]";

  [%expect{|
    [rev_]filteri [] -> [] / []
    [rev_]filteri [0] -> [0] / [0]
    [rev_]filteri [0; 1] -> [0] / [0]
    [rev_]filteri [0; 1; 2] -> [0; 2] / [2; 0]
    [rev_]filteri [0; 1; 2; 3] -> [0; 2] / [2; 0]
    [rev_]filteri [0; 1; 2; 3; 4] -> [0; 2; 4] / [4; 2; 0]
  |}]

let%expect_test "foldi2" =
  let open Format in
  let list_pairs = [
    ([], []);

    ([100], [200]);
    ([100; 110], [200; 210]);
    ([100; 110; 120], [200; 210; 220]);
  ] in
  let f i accum a_elm b_elm = begin
    (i + a_elm + b_elm) :: accum
  end in
  printf "@[<h>";
  iter list_pairs ~f:(fun (a, b) ->
    printf "foldi2 %a %a -> %a\n"
      (pp Uns.pp) a
      (pp Uns.pp) b
      (pp Uns.pp) (foldi2 a b ~init:[] ~f)
    ;
  );
  printf "@]";

  [%expect{|
    foldi2 [] [] -> []
    foldi2 [100] [200] -> [300]
    foldi2 [100; 110] [200; 210] -> [321; 300]
    foldi2 [100; 110; 120] [200; 210; 220] -> [342; 321; 300]
  |}]

let%expect_test "foldi2_until" =
  let open Format in
  let list_pairs = [
    ([], []);

    ([100], [200]);
    ([100; 110], [200; 210]);
    ([100; 110; 120], [200; 210; 220]);
  ] in
  printf "@[<h>";
  iter list_pairs ~f:(fun (a, b) ->
    let f i accum a_elm b_elm = begin
      let len = length a in
      let limit = len - 2 in
      ((i + a_elm + b_elm) :: accum), (i = limit)
    end in
    printf "foldi2 %a %a -> %a\n"
      (pp Uns.pp) a
      (pp Uns.pp) b
      (pp Uns.pp) (foldi2_until a b ~init:[] ~f)
  );
  printf "@]";

  [%expect{|
    foldi2 [] [] -> []
    foldi2 [100] [200] -> [300]
    foldi2 [100; 110] [200; 210] -> [300]
    foldi2 [100; 110; 120] [200; 210; 220] -> [321; 300]
  |}]

let%expect_test "iteri2" =
  let open Format in
  let list_pairs = [
    ([], []);
    ([0], [1]);
    ([0; 1], [2; 3]);
    ([0; 1; 2], [3; 4; 5]);
  ] in
  printf "@[<h>";
  iter list_pairs ~f:(fun (a, b) ->
    printf "iter2 %a %a ->"
      (pp Uns.pp) a
      (pp Uns.pp) b
    ;
    let f i a b = begin
      printf " (i=%a, a=%a, b=%a)" Uns.pp i Uns.pp a Uns.pp b
    end in
    iteri2 a b ~f;
    printf "\n"
  );
  printf "@]";

  [%expect{|
    iter2 [] [] ->
    iter2 [0] [1] -> (i=0, a=0, b=1)
    iter2 [0; 1] [2; 3] -> (i=0, a=0, b=2) (i=1, a=1, b=3)
    iter2 [0; 1; 2] [3; 4; 5] -> (i=0, a=0, b=3) (i=1, a=1, b=4) (i=2, a=2, b=5)
  |}]

let%expect_test "[rev_]mapi2" =
  let open Format in
  let list_pairs = [
    ([], []);
    ([10], [100]);
    ([10; 20], [100; 200]);
    ([10; 20; 30], [100; 200; 300]);
  ] in
  let f i a b = (b + a + i + 1) in
  printf "@[<h>";
  iter list_pairs ~f:(fun (a, b) ->
    printf "    mapi2 %a %a -> %a\n"
      (pp Uns.pp) a
      (pp Uns.pp) b
      (pp Uns.pp) (mapi2 a b ~f)
    ;

    printf "rev_mapi2 %a %a -> %a\n"
      (pp Uns.pp) a
      (pp Uns.pp) b
      (pp Uns.pp) (rev_mapi2 a b ~f)
  );
  printf "@]";

  [%expect{|
        mapi2 [] [] -> []
    rev_mapi2 [] [] -> []
        mapi2 [10] [100] -> [111]
    rev_mapi2 [10] [100] -> [111]
        mapi2 [10; 20] [100; 200] -> [111; 222]
    rev_mapi2 [10; 20] [100; 200] -> [222; 111]
        mapi2 [10; 20; 30] [100; 200; 300] -> [111; 222; 333]
    rev_mapi2 [10; 20; 30] [100; 200; 300] -> [333; 222; 111]
  |}]

let%expect_test "[rev_]foldi2_map" =
  let open Format in
  let list_pairs = [
    ([], []);
    ([10], [100]);
    ([10; 20], [100; 200]);
    ([10; 20; 30], [100; 200; 300]);
  ] in
  let f i accum a b = begin
    let sum = (b + a + i + 1) in
    (accum + sum), sum
  end in
  printf "@[<h>";
  iter list_pairs ~f:(fun (a, b) ->
    let accum, c = foldi2_map a b ~init:0 ~f in
    printf "    foldi2_map %a %a -> %a, %a\n"
      (pp Uns.pp) a
      (pp Uns.pp) b
      Uns.pp accum
      (pp Uns.pp) c
    ;

    let accum, c = rev_foldi2_map a b ~init:0 ~f in
    printf "rev_foldi2_map %a %a -> %a, %a\n"
      (pp Uns.pp) a
      (pp Uns.pp) b
      Uns.pp accum
      (pp Uns.pp) c
  );
  printf "@]";

  [%expect{|
        foldi2_map [] [] -> 0, []
    rev_foldi2_map [] [] -> 0, []
        foldi2_map [10] [100] -> 111, [111]
    rev_foldi2_map [10] [100] -> 111, [111]
        foldi2_map [10; 20] [100; 200] -> 333, [111; 222]
    rev_foldi2_map [10; 20] [100; 200] -> 333, [222; 111]
        foldi2_map [10; 20; 30] [100; 200; 300] -> 666, [111; 222; 333]
    rev_foldi2_map [10; 20; 30] [100; 200; 300] -> 666, [333; 222; 111]
  |}]

let%expect_test "Assoc" =
  let open Format in
  let assocs = [
    [];
    [(0, 10)];
    [(0, 10); (1, 11)];

    [(0, 10); (0, 11); (1, 12)];
    [(0, 10); (1, 11); (0, 12)];
    [(1, 10); (0, 11); (0, 12)];
    [(0, 10); (1, 11); (1, 12); (2, 13)];
  ] in
  let pp_assoc ppf (a, b) = fprintf ppf "(%a,@ %a)" Uns.pp a Uns.pp b in
  let missing = 3 in
  let cmp = Uns.cmp in
  printf "@[<h>";
  iter assocs ~f:(fun assoc ->
    printf "%a\n"
      (pp pp_assoc) assoc
    ;
    iter assoc ~f:(fun (k, _) ->
      printf "find_hlt/mem %a -> %a / %b\n"
        Uns.pp k
        Uns.pp (Assoc.find_hlt k ~cmp assoc)
        (Assoc.mem k ~cmp assoc)
    );

    printf "find/mem %a -> " Uns.pp missing;
    (match (Assoc.find missing ~cmp assoc),
          (Assoc.mem missing ~cmp assoc); with
      | None, b -> printf "None / %b" b
      | Some v, b -> printf "%a / %b" Uns.pp v b
    );
    printf "\n";

    iter assoc ~f:(fun (k, _) ->
      printf "remove_hlt %a -> %a\n"
        Uns.pp k
        (pp pp_assoc) (Assoc.remove_hlt k ~cmp assoc)
    );
    printf "remove %a -> %a\n"
      Uns.pp missing
      (pp pp_assoc) (Assoc.remove missing ~cmp assoc)
    ;
    printf "map -> %a\n"
      (pp pp_assoc) (Assoc.map assoc ~f:(fun v -> v * 2))
    ;

    printf "inverse -> %a\n"
      (pp pp_assoc) (Assoc.inverse assoc);

    printf "\n"
  );
  printf "@]";

  [%expect{|
    []
    find/mem 3 -> None / false
    remove 3 -> []
    map -> []
    inverse -> []

    [(0, 10)]
    find_hlt/mem 0 -> 10 / true
    find/mem 3 -> None / false
    remove_hlt 0 -> []
    remove 3 -> [(0, 10)]
    map -> [(0, 20)]
    inverse -> [(10, 0)]

    [(0, 10); (1, 11)]
    find_hlt/mem 0 -> 10 / true
    find_hlt/mem 1 -> 11 / true
    find/mem 3 -> None / false
    remove_hlt 0 -> [(1, 11)]
    remove_hlt 1 -> [(0, 10)]
    remove 3 -> [(0, 10); (1, 11)]
    map -> [(0, 20); (1, 22)]
    inverse -> [(11, 1); (10, 0)]

    [(0, 10); (0, 11); (1, 12)]
    find_hlt/mem 0 -> 10 / true
    find_hlt/mem 0 -> 10 / true
    find_hlt/mem 1 -> 12 / true
    find/mem 3 -> None / false
    remove_hlt 0 -> [(0, 11); (1, 12)]
    remove_hlt 0 -> [(0, 11); (1, 12)]
    remove_hlt 1 -> [(0, 10); (0, 11)]
    remove 3 -> [(0, 10); (0, 11); (1, 12)]
    map -> [(0, 20); (0, 22); (1, 24)]
    inverse -> [(12, 1); (11, 0); (10, 0)]

    [(0, 10); (1, 11); (0, 12)]
    find_hlt/mem 0 -> 10 / true
    find_hlt/mem 1 -> 11 / true
    find_hlt/mem 0 -> 10 / true
    find/mem 3 -> None / false
    remove_hlt 0 -> [(1, 11); (0, 12)]
    remove_hlt 1 -> [(0, 10); (0, 12)]
    remove_hlt 0 -> [(1, 11); (0, 12)]
    remove 3 -> [(0, 10); (1, 11); (0, 12)]
    map -> [(0, 20); (1, 22); (0, 24)]
    inverse -> [(12, 0); (11, 1); (10, 0)]

    [(1, 10); (0, 11); (0, 12)]
    find_hlt/mem 1 -> 10 / true
    find_hlt/mem 0 -> 11 / true
    find_hlt/mem 0 -> 11 / true
    find/mem 3 -> None / false
    remove_hlt 1 -> [(0, 11); (0, 12)]
    remove_hlt 0 -> [(1, 10); (0, 12)]
    remove_hlt 0 -> [(1, 10); (0, 12)]
    remove 3 -> [(1, 10); (0, 11); (0, 12)]
    map -> [(1, 20); (0, 22); (0, 24)]
    inverse -> [(12, 0); (11, 0); (10, 1)]

    [(0, 10); (1, 11); (1, 12); (2, 13)]
    find_hlt/mem 0 -> 10 / true
    find_hlt/mem 1 -> 11 / true
    find_hlt/mem 1 -> 11 / true
    find_hlt/mem 2 -> 13 / true
    find/mem 3 -> None / false
    remove_hlt 0 -> [(1, 11); (1, 12); (2, 13)]
    remove_hlt 1 -> [(0, 10); (1, 12); (2, 13)]
    remove_hlt 1 -> [(0, 10); (1, 12); (2, 13)]
    remove_hlt 2 -> [(0, 10); (1, 11); (1, 12)]
    remove 3 -> [(0, 10); (1, 11); (1, 12); (2, 13)]
    map -> [(0, 20); (1, 22); (1, 24); (2, 26)]
    inverse -> [(13, 2); (12, 1); (11, 1); (10, 0)]
  |}]
