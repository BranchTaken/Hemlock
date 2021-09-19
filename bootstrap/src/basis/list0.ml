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
        assert ((t0.list == t1.list) || (Stdlib.( = ) t0.list t1.list));
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
        | hd :: tl -> {t with left_rev=tl; right=(hd :: t.right); index=(Uns.pred t.index)}

      let succ t =
        match t.right with
        | [] -> halt "At end of list"
        | hd :: tl -> {t with left_rev=(hd :: t.left_rev); right=tl; index=(Uns.succ t.index)}

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
        | hd :: tl -> hd, {t with left_rev=tl; right=(hd :: t.right); index=(Uns.pred t.index)}

      let next t =
        match t.right with
        | [] -> halt "At end of list"
        | hd :: tl -> hd, {t with left_rev=(hd :: t.left_rev); right=tl; index=(Uns.succ t.index)}

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
