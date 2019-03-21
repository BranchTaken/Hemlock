open Rudiments

module T = struct
  type 'a t = 'a list
  type 'a elm = 'a

  let length t =
    let rec fn l i =
      match l with
      | [] -> i
      | _ :: tl -> fn tl (Uint.succ i)
    in
    fn t (Uint.kv 0)

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
          index: uint;
      }

      let cmp t0 t1 =
        (* == is excessively vague in OCaml. *)
        assert ((t0.list == t1.list)
                || (Stdlib.( = ) t0.list t1.list));
        Uint.cmp t0.index t1.index

      let hd list =
        {list; left_rev=[]; right=list; index=Uint.kv 0}

      let tl list =
        let rec fn l r len = begin
          match r with
          | [] -> l, len
          | hd :: tl -> fn (hd :: l) tl Uint.(succ len)
        end in
        let left_rev, len = fn [] list Uint.(kv 0) in
        {list; left_rev; right=[]; index=len}

      let succ t =
        match t.right with
        | [] -> halt "At end of list"
        | hd :: tl -> {t with left_rev=(hd :: t.left_rev); right=tl;
             index=(Uint.succ t.index)}

      let pred t =
        match t.left_rev with
        | [] -> halt "At beginning of list"
        | hd :: tl -> {t with left_rev=tl; right=(hd :: t.right);
             index=(Uint.pred t.index)}

      let lget t =
        match t.left_rev with
        | [] -> halt "At beginning of list"
        | hd :: _ -> hd

      let rget t =
        match t.right with
        | [] -> halt "At end of list"
        | hd :: _ -> hd

      let container t =
        t.list

      let index t =
        t.index

      let seek t i =
        let rec seek_rev t i = begin
          match i with
          | 0 -> t
          | i -> seek_rev (pred t) Int.(succ i)
        end in
        let rec seek_fwd t i = begin
          match i with
          | 0 -> t
          | i -> seek_fwd (succ t) Int.(pred i)
        end in
        match i with
        | 0 -> t
        | i when (i < 0) -> seek_rev t i
        | i -> seek_fwd t i
    end
    include T
    include Cmpable.Make_poly(T)
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
      | [], z when Uint.(z = zero) -> Cmp.Eq
      | [], _ -> Cmp.Lt
      | _ :: _, z when Uint.(z = zero) -> Cmp.Gt
      | _ :: t', _ -> fn t' Uint.(pred limit)
    end in
    fn t limit
end
include T
include Container_common.Make_poly_fold(T)

let init n ~f =
  let rec fn t rem = begin
    match Uint.(rem = (kv 0)) with
    | true -> t
    | false -> begin
        let i = Uint.(pred rem) in
        let t' = (f i) :: t in
        fn t' i
      end
  end in
  fn [] n

let is_empty t =
  Uint.((length t) = (kv 0))

let hd t =
  match t with
  | [] -> halt "Empty list has no head"
  | hd :: _ -> hd

let tl t =
  match t with
  | [] -> halt "Empty list has no tail"
  | _ :: tl -> tl

let nth_opt t i =
  let rec fn t i = begin
    match t, i with
    | [], _ -> None
    | hd :: _, i when Uint.(i = (kv 0)) -> Some hd
    | _ :: t', _ -> fn t' Uint.(pred i)
  end in
  fn t i

let nth t i =
  match nth_opt t i with
  | None -> halt "Out of bounds"
  | Some elm -> elm

let push t elm =
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

let split t i =
  let rec fn t i = begin
    match t, Uint.(to_int i) with
    | _, 0 -> [], t
    | [], _ -> halt "Out of bounds"
    | (e :: t'), _ -> begin
        let t0, t1 = fn t' Uint.(pred i) in
        (e :: t0), t1
      end
  end in
  fn t i

let rev_split t i =
  let rec fn t0 t1 i = begin
    match t1, Uint.(to_int i) with
    | _, 0 -> t0, t1
    | [], _ -> halt "Out of bounds"
    | (e :: t1'), _ -> fn (e :: t0) t1' Uint.(pred i)
  end in
  fn [] t i

let split_until t ~f =
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

let rev_split_until t ~f =
  let rec fn t0 t1 = begin
    match t1 with
    | [] -> t0, t1
    | elm :: t1' -> begin
        if f elm then t0, t1
        else fn (elm :: t0) t1'
      end
  end in
  fn [] t

let take t i =
  fst (split t i)

let rev_take t i =
  fst (rev_split t i)

let take_until t ~f =
  fst (split_until t ~f)

let rev_take_until t ~f =
  fst (rev_split_until t ~f)

let drop t i =
  let rec fn t1 i = begin
    match t1, Uint.(to_int i) with
    | _, 0 -> t1
    | [], _ -> halt "Out of bounds"
    | (_ :: t1'), _ -> fn t1' Uint.(pred i)
  end in
  fn t i

let drop_until t ~f =
  let rec fn t1 = begin
    match t1 with
    | [] -> t1
    | elm :: t1' -> begin
        if f elm then t1
        else fn t1'
      end
  end in
  fn t

let partition_tf t ~f =
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

let rev_partition_tf t ~f =
  let rec fn t_true t_false t = begin
    match t with
    | [] -> t_true, t_false
    | elm :: t' -> begin
        if f elm then fn (elm :: t_true) t_false t'
        else fn t_true (elm :: t_false) t'
      end
  end in
  fn [] [] t

let groupi t ~break =
  let rec fn_elm elm_left t i break = begin
    match t with
    | [] -> [elm_left], [], i
    | elm_right :: t' -> begin
        if break i elm_left elm_right then [elm_left], t, i
        else begin
          let g, t'', i' = fn_elm elm_right t' (Uint.succ i) break in
          (elm_left :: g), t'', i'
        end
      end
  end and fn_group t i break = begin
    match t with
    | [] -> []
    | elm :: t' -> begin
        let g, t'', i' = fn_elm elm t' (Uint.succ i) break in
        g :: (fn_group t'' i' break)
      end
  end in
  fn_group t (Uint.kv 0) break

let group t ~break =
  groupi t ~break:(fun _ elm0 elm1 -> break elm0 elm1)

let rev_groupi t ~break =
  let rec fn gs g t i = begin
    match g, t with
    | [], [] -> gs
    | _ :: _, [] -> g :: gs
    | [], elm :: t' -> fn gs [elm] t' (Uint.succ i)
    | elm_left :: _, elm_right :: t' -> begin
        let i' = Uint.succ i in
        if break i elm_left elm_right then fn (g :: gs) [elm_right] t' i'
        else fn gs (elm_right :: g) t' i'
      end
  end in
  fn [] [] t (Uint.kv 0)

let rev_group t ~break =
  rev_groupi t ~break:(fun _ elm0 elm1 -> break elm0 elm1)

let mapi t ~f =
  let rec fn t i f = begin
    match t with
    | [] -> []
    | elm :: t' -> (f i elm) :: fn t' (Uint.succ i) f
  end in
  fn t (Uint.kv 0) f

let map t ~f =
  mapi t ~f:(fun _ elm -> f elm)

let rev_mapi t ~f =
  let rec fn t i f accum = begin
    match t with
    | [] -> accum
    | elm :: t' -> fn t' (Uint.succ i) f ((f i elm) :: accum)
  end in
  fn t (Uint.kv 0) f []

let rev_map t ~f =
  rev_mapi t ~f:(fun _ elm -> f elm)

let rev_map_concat t0 t1 ~f =
  let rec fn rem t = begin
    match rem with
    | [] -> t
    | elm :: rem' -> fn rem' ((f elm) :: t)
  end in
  fn t0 t1

let foldi_map t ~init ~f =
  let rec fn a_list i f accum = begin
    match a_list with
    | [] -> accum, []
    | a_elm :: a_list' -> begin
        let i' = Uint.succ i in
        let accum', b_elm = f i accum a_elm in
        let accum'', b_list = fn a_list' i' f accum' in
        accum'', (b_elm :: b_list)
      end
  end in
  fn t (Uint.kv 0) f init

let fold_map t ~init ~f =
  foldi_map t ~init ~f:(fun _ accum a -> f accum a)

let rev_foldi_map t ~init ~f =
  let rec fn a_list i f accum b_list = begin
    match a_list with
    | [] -> accum, b_list
    | a_elm :: a_list' -> begin
        let i' = Uint.succ i in
        let accum', b_elm = f i accum a_elm in
        let b_list' = b_elm :: b_list in
        fn a_list' i' f accum' b_list'
      end
  end in
  fn t (Uint.kv 0) f init []

let rev_fold_map t ~init ~f =
  rev_foldi_map t ~init ~f:(fun _ accum a -> f accum a)

let filteri t ~f =
  let rec fn t i f = begin
    match t with
    | [] -> []
    | elm :: t' -> begin
        let i' = Uint.succ i in
        let keep = f i elm in
        let accum = fn t' i' f in
        match keep with
        | false -> accum
        | true -> elm :: accum
      end
  end in
  fn t (Uint.kv 0) f

let filter t ~f =
  filteri t ~f:(fun _ elm -> f elm)

let rev_filteri t ~f =
  let rec fn t i f accum = begin
    match t with
    | [] -> accum
    | elm :: t' -> begin
        let i' = Uint.succ i in
        match f i elm with
        | false -> fn t' i' f accum
        | true -> fn t' i' f (elm :: accum)
      end
  end in
  fn t (Uint.kv 0) f []

let rev_filter t ~f =
  rev_filteri t ~f:(fun _ elm -> f elm)

let foldi2_until t0 t1 ~init ~f =
  let rec fn t0 t1 i f accum = begin
    match t0, t1 with
    | [], [] -> accum
    | _ :: _, []
    | [], _ :: _ -> halt "List lengths differ"
    | elm0 :: t0', elm1 :: t1' -> begin
        let i' = Uint.succ i in
        let accum', until = f i accum elm0 elm1 in
        match until with
        | true -> accum'
        | false -> fn t0' t1' i' f accum'
      end
  end in
  fn t0 t1 (Uint.kv 0) f init

let fold2_until t0 t1 ~init ~f =
  foldi2_until t0 t1 ~init ~f:(fun _ accum a b -> f accum a b)

let foldi2 t0 t1 ~init ~f =
  foldi2_until t0 t1 ~init
    ~f:(fun i accum elm0 elm1 -> (f i accum elm0 elm1), false)

let fold2 t0 t1 ~init ~f =
  foldi2 t0 t1 ~init ~f:(fun _ accum a b -> f accum a b)

let iteri2 t0 t1 ~f =
  let rec fn t0 t1 i f = begin
    match t0, t1 with
    | [], [] -> ()
    | _ :: _, []
    | [], _ :: _ -> halt "List lengths differ"
    | elm0 :: t0', elm1 :: t1' -> begin
        f i elm0 elm1;
        fn t0' t1' (Uint.succ i) f
      end
  end in
  fn t0 t1 (Uint.kv 0) f

let iter2 t0 t1 ~f =
  iteri2 t0 t1 ~f:(fun _ a b -> f a b)

let mapi2 t0 t1 ~f =
  let rec fn t0 t1 i f = begin
    match t0, t1 with
    | [], [] -> []
    | _ :: _, []
    | [], _ :: _ -> halt "List lengths differ"
    | elm0 :: t0', elm1 :: t1' -> begin
        let i' = Uint.succ i in
        (f i elm0 elm1) :: (fn t0' t1' i' f)
      end
  end in
  fn t0 t1 (Uint.kv 0) f

let map2 t0 t1 ~f =
  mapi2 t0 t1 ~f:(fun _ a b -> f a b)

let rev_mapi2 t0 t1 ~f =
  let rec fn t0 t1 i f accum = begin
    match t0, t1 with
    | [], [] -> accum
    | _ :: _, []
    | [], _ :: _ -> halt "List lengths differ"
    | elm0 :: t0', elm1 :: t1' -> begin
        let i' = Uint.succ i in
        let accum' = (f i elm0 elm1) :: accum in
        fn t0' t1' i' f accum'
      end
  end in
  fn t0 t1 (Uint.kv 0) f []

let rev_map2 t0 t1 ~f =
  rev_mapi2 t0 t1 ~f:(fun _ a b -> f a b)

let foldi2_map t0 t1 ~init ~f =
  let rec fn t0 t1 i f accum = begin
    match t0, t1 with
    | [], [] -> accum, []
    | _ :: _, []
    | [], _ :: _ -> halt "List lengths differ"
    | elm0 :: t0', elm1 :: t1' -> begin
        let i' = Uint.succ i in
        let accum', elm = f i accum elm0 elm1 in
        let accum'', map = fn t0' t1' i' f accum' in
        accum'', (elm :: map)
      end
  end in
  fn t0 t1 (Uint.kv 0) f init

let fold2_map t0 t1 ~init ~f =
  foldi2_map t0 t1 ~init ~f:(fun _ accum a b -> f accum a b)

let rev_foldi2_map t0 t1 ~init ~f =
  let rec fn t0 t1 i f accum map = begin
    match t0, t1 with
    | [], [] -> accum, map
    | _ :: _, []
    | [], _ :: _ -> halt "List lengths differ"
    | elm0 :: t0', elm1 :: t1' -> begin
        let i' = Uint.succ i in
        let accum', elm = f i accum elm0 elm1 in
        let map' = elm :: map in
        fn t0' t1' i' f accum' map'
      end
  end in
  fn t0 t1 (Uint.kv 0) f init []

let rev_fold2_map t0 t1 ~init ~f =
  rev_foldi2_map t0 t1 ~init ~f:(fun _ accum a b -> f accum a b)

module Assoc = struct
  type nonrec ('a, 'b) t = ('a * 'b) t

  let add t a b =
    (a, b) :: t

  let find t a ~cmp =
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

  let find_hlt t a ~cmp =
    match find t a ~cmp with
    | None -> halt "Key not found"
    | Some v -> v

  let mem t a ~cmp =
    match find t a ~cmp with
    | None -> false
    | Some _ -> true

  let remove_impl t a ~cmp =
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

  let remove t a ~cmp =
    match remove_impl t a ~cmp with
    | None -> t
    | Some t' -> t'

  let remove_hlt t a ~cmp =
    match remove_impl t a ~cmp with
    | None -> halt "Key not found"
    | Some t' -> t'

  let map t ~f =
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

(*******************************************************************************
 * Begin tests.
 *)

let%expect_test "cmp" =
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  let test_cmp lst0 lst1 = begin
    printf " -> %s\n" (match cmp Int.cmp lst0 lst1 with
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
        printf "cmp ";
        print_uint_list list0;
        printf " ";
        print_uint_list list1;
        printf " -> ";
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
  test_with_lists lists lists lists;

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
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
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
        printf "cmp_length ";
        print_uint_list list0;
        printf " ";
        print_uint_list list1;
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
  test_with_lists lists lists lists;

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
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  let test_cmp_length_with lst limit = begin
    printf " (limit=%u -> %s)"
      Uint.(to_int limit) (match cmp_length_with lst limit with
      | Cmp.Lt -> "Lt"
      | Cmp.Eq -> "Eq"
      | Cmp.Gt -> "Gt"
    )
  end in
  let rec test_with_lists lists = begin
    match lists with
    | [] -> ()
    | list :: lists' -> begin
        printf "cmp_length_with ";
        print_uint_list list;
        for limit = 0 to 3 do
          printf "%s" (if limit = 0 then ": " else ", ");
          test_cmp_length_with list (Uint.of_int limit);
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
  test_with_lists lists;

  [%expect{|
    cmp_length_with []:  (limit=0 -> Eq),  (limit=1 -> Lt),  (limit=2 -> Lt),  (limit=3 -> Lt)
    cmp_length_with [0]:  (limit=0 -> Gt),  (limit=1 -> Eq),  (limit=2 -> Lt),  (limit=3 -> Lt)
    cmp_length_with [0; 1]:  (limit=0 -> Gt),  (limit=1 -> Gt),  (limit=2 -> Eq),  (limit=3 -> Lt)
    cmp_length_with [0; 1; 2]:  (limit=0 -> Gt),  (limit=1 -> Gt),  (limit=2 -> Gt),  (limit=3 -> Eq)
    |}]

let%expect_test "init" =
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  for i = 0 to 3 do
    print_uint_list (init Uint.(kv i) ~f:(fun j -> Uint.to_int j));
    printf "\n";
  done;

  [%expect{|
    []
    [0]
    [0; 1]
    [0; 1; 2]
    |}]

let%expect_test "nth_opt" =
  let open Printf in
  let l = [0; 1] in
  for i = 0 to 2 do
    match nth_opt l Uint.(of_int i) with
    | None -> printf "%u -> None\n" i
    | Some x -> printf "%u -> Some %u\n" i x
  done;

  [%expect{|
    0 -> Some 0
    1 -> Some 1
    2 -> None
    |}]

let%expect_test "concat,@,rev_concat" =
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  let list_pairs = [
    ([], []);
    ([0; 1], []);
    ([0; 1], [2; 3]);
    ([], [2; 3]);
  ] in
  iter list_pairs ~f:(fun (a, b) ->
    printf "concat ";
    print_uint_list a;
    printf " ";
    print_uint_list b;
    printf " -> ";
    print_uint_list (concat a b);
    printf "\n";

    printf "       ";
    print_uint_list a;
    printf " @ ";
    print_uint_list b;
    printf " -> ";
    print_uint_list (a @ b);
    printf "\n";

    printf "rev_concat ";
    print_uint_list a;
    printf " ";
    print_uint_list b;
    printf " -> ";
    print_uint_list (rev_concat a b);
    printf "\n";

    (* Brittle test; change in conjunction with implementation. *)
    printf "concat_unordered ";
    print_uint_list a;
    printf " ";
    print_uint_list b;
    printf " -> ";
    print_uint_list (concat_unordered a b);
    printf "\n";
  );
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
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  let list_lists = [
    [];

    [[]];
    [[0; 1]];

    [[]; []];
    [[0; 1]; [2; 3]];

    [[]; []; []];
    [[0; 1]; [2; 3]; [4; 5]];
  ] in
  iter list_lists ~f:(fun lists ->
    printf "join";
    iter lists ~f:(fun l ->
      printf " ";
      print_uint_list l
    );
    printf " -> ";
    print_uint_list (join lists);
    printf "\n";

    printf "join ~sep:[6; 7]";
    iter lists ~f:(fun l ->
      printf " ";
      print_uint_list l
    );
    printf " -> ";
    print_uint_list (join ~sep:[6; 7] lists);
    printf "\n";

    (* Brittle test; change in conjunction with implementation. *)
    printf "join_unordered ~sep:[6; 7]";
    iter lists ~f:(fun l ->
      printf " ";
      print_uint_list l
    );
    printf " -> ";
    print_uint_list (join_unordered ~sep:[6; 7] lists);
    printf "\n";
  );
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
  let open Printf in
  let test_length lst = begin
    printf "[";
    for i = 0 to Uint.(to_int (pred (length lst))) do
      let i = Uint.of_int i in
      if Uint.(i > (kv 0)) then printf "; ";
      printf "%u" (nth lst i);
    done;
    printf "]: length=%u, is_empty=%B\n"
      (Uint.to_int (length lst)) (is_empty lst)
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
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  let test_pop_push lst = begin
    print_uint_list lst;
    printf " -> ";
    let hd_, tl_ = pop lst in
    assert Int.(hd_ = (hd lst));
    let () = match cmp Int.cmp tl_ (tl lst) with
      | Cmp.Eq -> ()
      | _ -> assert false
    in
    printf "%u " hd_;
    print_uint_list tl_;
    printf " -> ";
    print_uint_list (push tl_ hd_);
    printf " = ";
    print_uint_list (hd_ :: tl_);
    printf "\n"
  end in
  let lists = [
    [0];
    [0; 1];
    [0; 1; 2];
  ] in
  iter lists ~f:(fun lst -> test_pop_push lst);

  [%expect{|
    [0] -> 0 [] -> [0] = [0]
    [0; 1] -> 0 [1] -> [0; 1] = [0; 1]
    [0; 1; 2] -> 0 [1; 2] -> [0; 1; 2] = [0; 1; 2]
    |}]

let%expect_test "rev" =
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  let lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
  ] in
  iter lists ~f:(fun l ->
    printf "rev ";
    print_uint_list l;
    printf " -> ";
    print_uint_list (rev l);
    printf "\n"
  );

  [%expect{|
    rev [] -> []
    rev [0] -> [0]
    rev [0; 1] -> [1; 0]
    rev [0; 1; 2] -> [2; 1; 0]
    |}]

let%expect_test "zip,unzip" =
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  let print_uint_zlist zlist = begin
    printf "[";
    let rec fn zlist = begin
      match zlist with
      | [] -> ()
      | (a, b) :: ztl -> begin
          printf "; (%u, %u)" a b;
          fn ztl
        end
    end in
    let () = match zlist with
      | [] -> ()
      | (a, b) :: ztl -> begin
          printf "(%u, %u)" a b;
          fn ztl
        end
    in
    printf "]"
  end in
  let list_pairs = [
    ([], []);
    ([0], [1]);
    ([0; 1], [2; 3]);
    ([0; 1; 2], [3; 4; 5])
  ] in
  iter list_pairs ~f:(fun (t0, t1) ->
    printf "zip/unzip ";
    print_uint_list t0;
    printf " ";
    print_uint_list t1;
    printf " -> ";
    let z = zip t0 t1 in
    print_uint_zlist z;
    printf " -> ";
    let t0', t1' = unzip z in
    print_uint_list t0';
    printf " ";
    print_uint_list t1';
    printf "\n"
  );

  [%expect{|
    zip/unzip [] [] -> [] -> [] []
    zip/unzip [0] [1] -> [(0, 1)] -> [0] [1]
    zip/unzip [0; 1] [2; 3] -> [(0, 2); (1, 3)] -> [0; 1] [2; 3]
    zip/unzip [0; 1; 2] [3; 4; 5] -> [(0, 3); (1, 4); (2, 5)] -> [0; 1; 2] [3; 4; 5]
    |}]

let%expect_test "split,rev_split,take,rev_take,drop" =
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  let lists = [
    [];
    [0];
    [0; 1]
  ] in
  iter lists ~f:(fun l ->
    for i = 0 to Uint.(to_int (length l)) do
      printf "split/take,drop ";
      print_uint_list l;
      printf " %u -> " i;
      let a, b = split l (Uint.of_int i) in
      print_uint_list a;
      printf ", ";
      print_uint_list b;
      printf " / ";
      print_uint_list (take l (Uint.of_int i));
      printf ", ";
      print_uint_list (drop l (Uint.of_int i));
      printf "\n";

      printf "rev_split/rev_take,drop ";
      print_uint_list l;
      printf " %u -> " i;
      let a, b = rev_split l (Uint.of_int i) in
      print_uint_list a;
      printf ", ";
      print_uint_list b;
      printf " / ";
      print_uint_list (rev_take l (Uint.of_int i));
      printf ", ";
      print_uint_list (drop l (Uint.of_int i));
      printf "\n"
    done
  );

  [%expect{|
    split/take,drop [] 0 -> [], [] / [], []
    rev_split/rev_take,drop [] 0 -> [], [] / [], []
    split/take,drop [0] 0 -> [], [0] / [], [0]
    rev_split/rev_take,drop [0] 0 -> [], [0] / [], [0]
    split/take,drop [0] 1 -> [0], [] / [0], []
    rev_split/rev_take,drop [0] 1 -> [0], [] / [0], []
    split/take,drop [0; 1] 0 -> [], [0; 1] / [], [0; 1]
    rev_split/rev_take,drop [0; 1] 0 -> [], [0; 1] / [], [0; 1]
    split/take,drop [0; 1] 1 -> [0], [1] / [0], [1]
    rev_split/rev_take,drop [0; 1] 1 -> [0], [1] / [0], [1]
    split/take,drop [0; 1] 2 -> [0; 1], [] / [0; 1], []
    rev_split/rev_take,drop [0; 1] 2 -> [1; 0], [] / [1; 0], []
    |}]

let%expect_test "[rev_]split_until,[rev_]take_until,drop_until" =
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  let lists = [
    [];
    [0];
    [0; 1];
  ] in
  iter lists ~f:(fun l ->
    for i = 0 to Uint.(to_int (length l)) do
      let f elm = elm >= i in
      printf "split_until/take_until,drop_until ";
      print_uint_list l;
      printf " ~f:(fun elm -> elm >= %u) -> " i;
      let l0, l1 = split_until l ~f in
      print_uint_list l0;
      printf " ";
      print_uint_list l1;
      printf " / ";
      print_uint_list (take_until l ~f);
      printf " ";
      print_uint_list (drop_until l ~f);
      printf "\n";

      printf "rev_split_until/rev_take_until,drop_until ";
      print_uint_list l;
      printf " ~f:(fun elm -> elm >= %u) -> " i;
      let rl0, rl1 = rev_split_until l ~f in
      print_uint_list rl0;
      printf " ";
      print_uint_list rl1;
      printf " / ";
      print_uint_list (rev_take_until l ~f);
      printf " ";
      print_uint_list (drop_until l ~f);
      printf "\n"
    done
  );

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
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  let lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 2; 3];
  ] in
  iter lists ~f:(fun l ->
    let even x = (x mod 2) = 0 in
    printf "[rev_]partition_tf ";
    print_uint_list l;
    printf " ~f:even -> ";
    let l_true, l_false = partition_tf l ~f:even in
    print_uint_list l_true;
    printf " ";
    print_uint_list l_false;

    printf " / ";
    let rl_true, rl_false = rev_partition_tf l ~f:even in
    print_uint_list rl_true;
    printf " ";
    print_uint_list rl_false;
    printf "\n";
  );

  [%expect{|
    [rev_]partition_tf [] ~f:even -> [] [] / [] []
    [rev_]partition_tf [0] ~f:even -> [0] [] / [0] []
    [rev_]partition_tf [0; 1] ~f:even -> [0] [1] / [0] [1]
    [rev_]partition_tf [0; 1; 2] ~f:even -> [0; 2] [1] / [2; 0] [1]
    [rev_]partition_tf [0; 1; 2; 3] ~f:even -> [0; 2] [1; 3] / [2; 0] [3; 1]
  |}]

let%expect_test "[rev_]group" =
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  let print_uint_list_list lsts = begin
    printf "[";
    let rec fn lsts = begin
      match lsts with
      | [] -> ()
      | lst :: lsts' -> begin
          printf "; ";
          print_uint_list lst;
          fn lsts'
        end
    end in
    let () = match lsts with
      | [] -> ()
      | lst :: lsts' -> begin
          print_uint_list lst;
          fn lsts'
        end
    in
    printf "]"
  end in
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
  let eq x0 x1 = x0 = x1 in
  iter lists ~f:(fun l ->
    printf "[rev_]group ";
    print_uint_list l;
    printf " ~break:eq -> ";
    print_uint_list_list (group l ~break:eq);
    printf " / ";
    print_uint_list_list (rev_group l ~break:eq);
    printf "\n";
  );

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
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  let print_uint_list_list lsts = begin
    printf "[";
    let rec fn lsts = begin
      match lsts with
      | [] -> ()
      | lst :: lsts' -> begin
          printf "; ";
          print_uint_list lst;
          fn lsts'
        end
    end in
    let () = match lsts with
      | [] -> ()
      | lst :: lsts' -> begin
          print_uint_list lst;
          fn lsts'
        end
    in
    printf "]"
  end in
  let lists = [
    [];
    [9];
    [9; 9];

    [0; 1];
    [9; 1; 2; 9];

    [0; 1; 2];
    [9; 1; 2; 9; 4; 5; 9];
  ] in
  let inds i x0 x1 = ((Uint.to_int i) = x1) && ((Uint.to_int i) = succ x0) in
  iter lists ~f:(fun l ->
    printf "[rev_]groupi ";
    print_uint_list l;
    printf " ~break:inds -> ";
    print_uint_list_list (groupi l ~break:inds);
    printf " / ";
    print_uint_list_list (rev_groupi l ~break:inds);
    printf "\n";
  );

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
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  let lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 2; 3];
  ] in
  iter lists ~f:(fun l ->
    printf "[rev_]mapi ";
    print_uint_list l;
    printf " -> ";
    let f i elm = elm + (Uint.to_int i) * 10 in
    print_uint_list (mapi l ~f);
    printf " / ";
    print_uint_list (rev_mapi l ~f);
    printf "\n"
  );

  [%expect{|
    [rev_]mapi [] -> [] / []
    [rev_]mapi [0] -> [0] / [0]
    [rev_]mapi [0; 1] -> [0; 11] / [11; 0]
    [rev_]mapi [0; 1; 2] -> [0; 11; 22] / [22; 11; 0]
    [rev_]mapi [0; 1; 2; 3] -> [0; 11; 22; 33] / [33; 22; 11; 0]
  |}]

let%expect_test "rev_map_concat" =
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
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
  iter list_pairs ~f:(fun (a, b) ->
    printf "rev_map_concat ";
    print_uint_list a;
    printf " ";
    print_uint_list b;
    printf " -> ";
    let f elm = elm + 10 in
    print_uint_list (rev_map_concat a b ~f);
    printf "\n"
  );

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
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  let lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 2; 3];
  ] in
  let f i accum elm = (elm :: accum), (elm + (Uint.to_int i) * 10) in
  iter lists ~f:(fun l ->
    printf "    fold_mapi ";
    print_uint_list l;
    printf " -> accum=";
    let accum, b_list = foldi_map l ~init:[] ~f in
    print_uint_list accum;
    printf ", b_list=";
    print_uint_list b_list;
    printf "\n";

    printf "rev_fold_mapi ";
    print_uint_list l;
    printf " -> accum=";
    let accum, b_list = rev_foldi_map l ~init:[] ~f in
    print_uint_list accum;
    printf ", b_list=";
    print_uint_list b_list;
    printf "\n"
  );

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
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  let lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 2; 3];
    [0; 1; 2; 3; 4];
  ] in
  iter lists ~f:(fun l ->
    printf "[rev_]filteri ";
    print_uint_list l;
    printf " -> ";
    let f i _ = Uint.(i % (kv 2) = (kv 0)) in
    print_uint_list (filteri l ~f);
    printf " / ";
    print_uint_list (rev_filteri l ~f);
    printf "\n"
  );

  [%expect{|
    [rev_]filteri [] -> [] / []
    [rev_]filteri [0] -> [0] / [0]
    [rev_]filteri [0; 1] -> [0] / [0]
    [rev_]filteri [0; 1; 2] -> [0; 2] / [2; 0]
    [rev_]filteri [0; 1; 2; 3] -> [0; 2] / [2; 0]
    [rev_]filteri [0; 1; 2; 3; 4] -> [0; 2; 4] / [4; 2; 0]
  |}]

let%expect_test "foldi2" =
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  let list_pairs = [
    ([], []);

    ([100], [200]);
    ([100; 110], [200; 210]);
    ([100; 110; 120], [200; 210; 220]);
  ] in
  iter list_pairs ~f:(fun (a, b) ->
    printf "foldi2 ";
    print_uint_list a;
    printf " ";
    print_uint_list b;
    printf " -> ";
    let f i accum a_elm b_elm = begin
      (Uint.to_int i + a_elm + b_elm) :: accum
    end in
    print_uint_list (foldi2 a b ~init:[] ~f);
    printf "\n"
  );

  [%expect{|
    foldi2 [] [] -> []
    foldi2 [100] [200] -> [300]
    foldi2 [100; 110] [200; 210] -> [321; 300]
    foldi2 [100; 110; 120] [200; 210; 220] -> [342; 321; 300]
  |}]

let%expect_test "foldi2_until" =
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  let list_pairs = [
    ([], []);

    ([100], [200]);
    ([100; 110], [200; 210]);
    ([100; 110; 120], [200; 210; 220]);
  ] in
  iter list_pairs ~f:(fun (a, b) ->
    printf "foldi2 ";
    print_uint_list a;
    printf " ";
    print_uint_list b;
    printf " -> ";
    let f i accum a_elm b_elm = begin
      let len = length a in
      let limit = Uint.(len - (kv 2)) in
      ((Uint.to_int i + a_elm + b_elm) :: accum), Uint.(i = limit)
    end in
    print_uint_list (foldi2_until a b ~init:[] ~f);
    printf "\n"
  );

  [%expect{|
    foldi2 [] [] -> []
    foldi2 [100] [200] -> [300]
    foldi2 [100; 110] [200; 210] -> [300]
    foldi2 [100; 110; 120] [200; 210; 220] -> [321; 300]
  |}]

let%expect_test "iteri2" =
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  let list_pairs = [
    ([], []);
    ([0], [1]);
    ([0; 1], [2; 3]);
    ([0; 1; 2], [3; 4; 5]);
  ] in
  iter list_pairs ~f:(fun (a, b) ->
    printf "iter2 ";
    print_uint_list a;
    printf " ";
    print_uint_list b;
    printf " ->";
    let f i a b = begin
      printf " (i=%u, a=%u, b=%u)" (Uint.to_int i) a b
    end in
    iteri2 a b ~f;
    printf "\n"
  );

  [%expect{|
    iter2 [] [] ->
    iter2 [0] [1] -> (i=0, a=0, b=1)
    iter2 [0; 1] [2; 3] -> (i=0, a=0, b=2) (i=1, a=1, b=3)
    iter2 [0; 1; 2] [3; 4; 5] -> (i=0, a=0, b=3) (i=1, a=1, b=4) (i=2, a=2, b=5)
  |}]

let%expect_test "[rev_]mapi2" =
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  let list_pairs = [
    ([], []);
    ([10], [100]);
    ([10; 20], [100; 200]);
    ([10; 20; 30], [100; 200; 300]);
  ] in
  let f i a b = b + a + (Uint.to_int i) + 1 in
  iter list_pairs ~f:(fun (a, b) ->
    printf "    mapi2 ";
    print_uint_list a;
    printf " ";
    print_uint_list b;
    printf " -> ";
    print_uint_list (mapi2 a b ~f);
    printf "\n";

    printf "rev_mapi2 ";
    print_uint_list a;
    printf " ";
    print_uint_list b;
    printf " -> ";
    print_uint_list (rev_mapi2 a b ~f);
    printf "\n";
  );

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
  let open Printf in
  let print_uint_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "; %u" hd;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | hd :: tl -> begin
          printf "%u" hd;
          fn tl
        end
    in
    printf "]"
  end in
  let list_pairs = [
    ([], []);
    ([10], [100]);
    ([10; 20], [100; 200]);
    ([10; 20; 30], [100; 200; 300]);
  ] in
  let f i accum a b = begin
    let sum = b + a + (Uint.to_int i) + 1 in
    accum + sum, sum
  end in
  iter list_pairs ~f:(fun (a, b) ->
    printf "    foldi2_map ";
    print_uint_list a;
    printf " ";
    print_uint_list b;
    printf " -> ";
    let accum, c = foldi2_map a b ~init:0 ~f in
    printf "%u, " accum;
    print_uint_list c;
    printf "\n";

    printf "rev_foldi2_map ";
    print_uint_list a;
    printf " ";
    print_uint_list b;
    printf " -> ";
    let accum, c = rev_foldi2_map a b ~init:0 ~f in
    printf "%u, " accum;
    print_uint_list c;
    printf "\n";
  );

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
  let open Printf in
  let print_uint_pair_list lst = begin
    printf "[";
    let rec fn lst = begin
      match lst with
      | [] -> ()
      | (a, b) :: tl -> begin
          printf "; (%u, %u)" a b;
          fn tl
        end
    end in
    let () = match lst with
      | [] -> ()
      | (a, b) :: tl -> begin
          printf "(%u, %u)" a b;
          fn tl
        end
    in
    printf "]"
  end in
  let assocs = [
    [];
    [(0, 10)];
    [(0, 10); (1, 11)];

    [(0, 10); (0, 11); (1, 12)];
    [(0, 10); (1, 11); (0, 12)];
    [(1, 10); (0, 11); (0, 12)];
    [(0, 10); (1, 11); (1, 12); (2, 13)];
  ] in
  let missing = 3 in
  let cmp = Int.cmp in
  iter assocs ~f:(fun assoc ->
    print_uint_pair_list assoc;
    printf "\n";
    iter assoc ~f:(fun (k, _) ->
      printf "find_hlt/mem %u -> %u / %b\n"
        k (Assoc.find_hlt assoc k ~cmp) (Assoc.mem assoc k ~cmp)
    );

    printf "find/mem %u -> " missing;
    (match (Assoc.find assoc missing ~cmp),
          (Assoc.mem assoc missing ~cmp); with
      | None, b -> printf "None / %b" b
      | Some v, b -> printf "%u / %b" v b
    );
    printf "\n";

    iter assoc ~f:(fun (k, _) ->
      printf "remove_hlt %u -> " k;
      print_uint_pair_list (Assoc.remove_hlt assoc k ~cmp);
      printf "\n"
    );
    printf "remove %u -> " missing;
    print_uint_pair_list (Assoc.remove assoc missing ~cmp);
    printf "\n";

    printf "map -> ";
    print_uint_pair_list (Assoc.map assoc ~f:(fun v -> v * 2));
    printf "\n";

    printf "inverse -> ";
    print_uint_pair_list (Assoc.inverse assoc);
    printf "\n";

    printf "\n"
  );

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
