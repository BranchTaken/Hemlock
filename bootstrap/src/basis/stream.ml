open Rudiments

type 'a elm =
  | Nil
  | Cons of 'a * 'a t
and 'a t = 'a elm lazy_t

let empty = lazy Nil

let init n ~f =
  let rec fn i n f = lazy begin
    match i < n with
    | false -> Nil
    | true -> begin
        let elm = f i in
        let t = fn (succ i) n f in
        Cons(elm, t)
      end
  end in
  fn 0 n f

let length t =
  let rec fn t i = begin
    match t with
    | lazy Nil -> i
    | lazy (Cons(_, t')) -> fn t' (succ i)
  end in
  fn t 0

let is_empty = function
  | lazy Nil -> true
  | lazy (Cons(_, _)) -> false

let hd = function
  | lazy Nil -> halt "Empty stream has no head"
  | lazy (Cons(elm, _)) -> elm

let tl = function
  | lazy Nil -> halt "Empty stream has no tail"
  | lazy (Cons(_, t)) -> t

(* Non-lazy internal function of push (conventionally named push').  It is
 * exposed outside of push since it is useful within non-lazy sections of other
 * stream functions. *)
let push' elm s =
  Cons(elm, s)

let push elm s =
  lazy (push' elm s)

let pop = function
  | lazy Nil -> halt "Empty stream has no head"
  | lazy (Cons(elm, t)) -> (elm, t)

let rec concat t0 t1 = lazy begin
  match t0 with
  | lazy Nil -> Lazy.force t1
  | lazy (Cons(elm, t0')) -> push' elm (concat t0' t1)
end

let rec take n t =
  lazy begin
    match t, n with
    | _, 0 -> Nil
    | lazy Nil, _ -> Lazy.force t
    | lazy (Cons(elm, t')), _ -> push' elm (take (pred n) t')
  end

let drop n t =
  let rec drop' n t = begin
    match t, n with
    | _, 0 -> Lazy.force t
    | lazy Nil, _ -> Lazy.force t
    | lazy (Cons(_, t')), _ -> drop' (pred n) t'
  end in
  lazy (drop' n t)

let split n t =
  take n t, drop n t

let rev t =
  let rec rev' t r = begin
    match t, r with
    | lazy Nil, _ -> Lazy.force r
    | lazy (Cons(elm, t')), _ -> rev' t' (push elm r)
  end in
  lazy (rev' t empty)

let rev_take n t =
  rev (take n t)

let rev_split n t =
  rev_take n t, drop n t

let pp pp_elm ppf t =
  let open Format in
  fprintf ppf "@[<h>";
  let rec fn t = begin
    match t with
    | lazy Nil -> fprintf ppf "Nil"
    | lazy (Cons(elm, t')) -> begin
        fprintf ppf "(%a@ " pp_elm elm;
        fn t';
        fprintf ppf ")"
      end
  end in
  fn t;
  fprintf ppf "@]"

(******************************************************************************)
(* Begin tests. *)

let%expect_test "empty" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let t = empty in
  printf "empty = %a\n" ppt t;
  printf "@]";

  [%expect{|
    empty = Nil
    |}]

let%expect_test "init" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let rec test_init_up_to i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t = init i ~f:(fun i -> i) in
        printf "init %a ~f:(fun i -> i) = %a\n" Usize.pp i ppt t;
        test_init_up_to (succ i) n
      end
  end in
  test_init_up_to 0 3;
  printf "@]";

  [%expect{|
    init 0 ~f:(fun i -> i) = Nil
    init 1 ~f:(fun i -> i) = (0 Nil)
    init 2 ~f:(fun i -> i) = (0 (1 Nil))
    init 3 ~f:(fun i -> i) = (0 (1 (2 Nil)))
    |}]

let%expect_test "length" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let rec test_length_up_to i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t = init i ~f:(fun i -> i) in
        let l = length t in
        printf "length %a = %a\n" ppt t Usize.pp l;
        test_length_up_to (succ i) n
      end
  end in
  test_length_up_to 0 3;

  [%expect{|
    length Nil = 0
    length (0 Nil) = 1
    length (0 (1 Nil)) = 2
    length (0 (1 (2 Nil))) = 3
    |}]

let%expect_test "is_empty" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let rec test_is_empty_up_to i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t = init i ~f:(fun i -> i) in
        let e = is_empty t in
        printf "is_empty %a = %b\n" ppt t e;
        test_is_empty_up_to (succ i) n
      end
  end in
  test_is_empty_up_to 0 3;

  [%expect{|
    is_empty Nil = true
    is_empty (0 Nil) = false
    is_empty (0 (1 Nil)) = false
    is_empty (0 (1 (2 Nil))) = false
    |}]

let%expect_test "hd" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let rec test_hd_up_to i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t = init ~f:(fun i -> i) i in
        let elm = hd t in
        printf "hd %a = %a\n" ppt t Usize.pp elm;
        test_hd_up_to (succ i) n
      end
  end in
  (* halts if we start at 0 *)
  test_hd_up_to 1 4;
  printf "@]";

  [%expect{|
    hd (0 Nil) = 0
    hd (0 (1 Nil)) = 0
    hd (0 (1 (2 Nil))) = 0
    hd (0 (1 (2 (3 Nil)))) = 0
    |}]

let%expect_test "tl" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let rec test_tl_up_to i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t = init ~f:(fun i -> i) i in
        let t' = tl t in
        printf "tl %a = %a\n" ppt t ppt t';
        test_tl_up_to (succ i) n
      end
  end in
  (* halts if we start at 0 *)
  test_tl_up_to 1 4;
  printf "@]";

  [%expect{|
    tl (0 Nil) = Nil
    tl (0 (1 Nil)) = (1 Nil)
    tl (0 (1 (2 Nil))) = (1 (2 Nil))
    tl (0 (1 (2 (3 Nil)))) = (1 (2 (3 Nil)))
    |}]

let%expect_test "push" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let rec test_push_up_to t i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t' = push i t in
        printf "push %a %a = %a\n" Usize.pp i ppt t ppt t';
        test_push_up_to t' (succ i) n
      end
  end in
  test_push_up_to empty 0 3;
  printf "@]";

  [%expect{|
    push 0 Nil = (0 Nil)
    push 1 (0 Nil) = (1 (0 Nil))
    push 2 (1 (0 Nil)) = (2 (1 (0 Nil)))
    push 3 (2 (1 (0 Nil))) = (3 (2 (1 (0 Nil))))
    |}]

let%expect_test "pop" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let rec test_pop t = begin
    match t with
    | lazy Nil -> ()
    | _ -> begin
        let elm, t' = pop t in
        printf "pop %a = %a %a\n" ppt t Usize.pp elm ppt t';
        test_pop t'
      end
  end in
  test_pop (init 3 ~f:(fun i -> i));
  printf "@]";

  [%expect{|
    pop (0 (1 (2 Nil))) = 0 (1 (2 Nil))
    pop (1 (2 Nil)) = 1 (2 Nil)
    pop (2 Nil) = 2 Nil
    |}]

let%expect_test "concat" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let rec test_concat_up_to i l n = begin
    match i <= l, l <= n with
    | _, false -> ()
    | false, _ -> test_concat_up_to 0 (succ l) n
    | true, true -> begin
        let t0 = init ~f:(fun i -> i) i in
        let t1 = init ~f:(fun j -> i + j) (l - i) in
        let t = concat t0 t1 in
        printf "concat %a %a = %a\n" ppt t0 ppt t1 ppt t;
        test_concat_up_to (succ i) l n
      end
  end in
  test_concat_up_to 0 0 3;
  printf "@]";

  [%expect{|
    concat Nil Nil = Nil
    concat Nil (0 Nil) = (0 Nil)
    concat (0 Nil) Nil = (0 Nil)
    concat Nil (0 (1 Nil)) = (0 (1 Nil))
    concat (0 Nil) (1 Nil) = (0 (1 Nil))
    concat (0 (1 Nil)) Nil = (0 (1 Nil))
    concat Nil (0 (1 (2 Nil))) = (0 (1 (2 Nil)))
    concat (0 Nil) (1 (2 Nil)) = (0 (1 (2 Nil)))
    concat (0 (1 Nil)) (2 Nil) = (0 (1 (2 Nil)))
    concat (0 (1 (2 Nil))) Nil = (0 (1 (2 Nil)))
    |}]

let%expect_test "split" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let rec test_split_up_to i l n =
    match i <= l, l <= n with
    | _, false -> ()
    | false, _ -> test_split_up_to 0 (succ l) n
    | true, true -> begin
        let t = init l ~f:(fun i -> i) in
        let t0, t1 = split i t in
        printf "split %a %a = %a %a\n" Usize.pp i ppt t ppt t0 ppt t1;
        test_split_up_to (succ i) l n
      end in
  test_split_up_to 0 0 3;
  printf "@]";

  [%expect{|
    split 0 Nil = Nil Nil
    split 0 (0 Nil) = Nil (0 Nil)
    split 1 (0 Nil) = (0 Nil) Nil
    split 0 (0 (1 Nil)) = Nil (0 (1 Nil))
    split 1 (0 (1 Nil)) = (0 Nil) (1 Nil)
    split 2 (0 (1 Nil)) = (0 (1 Nil)) Nil
    split 0 (0 (1 (2 Nil))) = Nil (0 (1 (2 Nil)))
    split 1 (0 (1 (2 Nil))) = (0 Nil) (1 (2 Nil))
    split 2 (0 (1 (2 Nil))) = (0 (1 Nil)) (2 Nil)
    split 3 (0 (1 (2 Nil))) = (0 (1 (2 Nil))) Nil
    |}]

let%expect_test "rev_split" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let rec test_rev_split_up_to i l n =
    match i <= l, l <= n with
    | _, false -> ()
    | false, _ -> test_rev_split_up_to 0 (succ l) n
    | true, true -> begin
        let t = init l ~f:(fun i -> i) in
        let t0, t1 = rev_split i t in
        printf "rev_split %a %a = %a %a\n" Usize.pp i ppt t ppt t0 ppt t1;
        test_rev_split_up_to (succ i) l n
      end in
  test_rev_split_up_to 0 0 3;
  printf "@]";

  [%expect{|
    rev_split 0 Nil = Nil Nil
    rev_split 0 (0 Nil) = Nil (0 Nil)
    rev_split 1 (0 Nil) = (0 Nil) Nil
    rev_split 0 (0 (1 Nil)) = Nil (0 (1 Nil))
    rev_split 1 (0 (1 Nil)) = (0 Nil) (1 Nil)
    rev_split 2 (0 (1 Nil)) = (1 (0 Nil)) Nil
    rev_split 0 (0 (1 (2 Nil))) = Nil (0 (1 (2 Nil)))
    rev_split 1 (0 (1 (2 Nil))) = (0 Nil) (1 (2 Nil))
    rev_split 2 (0 (1 (2 Nil))) = (1 (0 Nil)) (2 Nil)
    rev_split 3 (0 (1 (2 Nil))) = (2 (1 (0 Nil))) Nil
    |}]

let%expect_test "take" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let rec test_take_up_to i l n =
    (* l + 1 so that we test taking one more than the stream contains *)
    match i <= l + 1, l <= n with
    | _, false -> ()
    | false, _ -> test_take_up_to 0 (succ l) n
    | true, true -> begin
        let t = init l ~f:(fun i -> i) in
        let t' = take i t in
        printf "take %a %a = %a\n" ppt t Usize.pp i ppt t';
        test_take_up_to (succ i) l n
      end in
  test_take_up_to 0 0 3;
  printf "@]";

  [%expect{|
    take Nil 0 = Nil
    take Nil 1 = Nil
    take (0 Nil) 0 = Nil
    take (0 Nil) 1 = (0 Nil)
    take (0 Nil) 2 = (0 Nil)
    take (0 (1 Nil)) 0 = Nil
    take (0 (1 Nil)) 1 = (0 Nil)
    take (0 (1 Nil)) 2 = (0 (1 Nil))
    take (0 (1 Nil)) 3 = (0 (1 Nil))
    take (0 (1 (2 Nil))) 0 = Nil
    take (0 (1 (2 Nil))) 1 = (0 Nil)
    take (0 (1 (2 Nil))) 2 = (0 (1 Nil))
    take (0 (1 (2 Nil))) 3 = (0 (1 (2 Nil)))
    take (0 (1 (2 Nil))) 4 = (0 (1 (2 Nil)))
    |}]

let%expect_test "rev_take" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let rec test_rev_take_up_to i l n =
    (* l + 1 so that we test taking one more than the stream contains *)
    match i <= l + 1, l <= n with
    | _, false -> ()
    | false, _ -> test_rev_take_up_to 0 (succ l) n
    | true, true -> begin
        let t = init l ~f:(fun i -> i) in
        let t' = rev_take i t in
        printf "rev_take %a %a = %a\n" Usize.pp i ppt t ppt t';
        test_rev_take_up_to (succ i) l n
      end in
  test_rev_take_up_to 0 0 3;
  printf "@]";

  [%expect{|
    rev_take 0 Nil = Nil
    rev_take 1 Nil = Nil
    rev_take 0 (0 Nil) = Nil
    rev_take 1 (0 Nil) = (0 Nil)
    rev_take 2 (0 Nil) = (0 Nil)
    rev_take 0 (0 (1 Nil)) = Nil
    rev_take 1 (0 (1 Nil)) = (0 Nil)
    rev_take 2 (0 (1 Nil)) = (1 (0 Nil))
    rev_take 3 (0 (1 Nil)) = (1 (0 Nil))
    rev_take 0 (0 (1 (2 Nil))) = Nil
    rev_take 1 (0 (1 (2 Nil))) = (0 Nil)
    rev_take 2 (0 (1 (2 Nil))) = (1 (0 Nil))
    rev_take 3 (0 (1 (2 Nil))) = (2 (1 (0 Nil)))
    rev_take 4 (0 (1 (2 Nil))) = (2 (1 (0 Nil)))
    |}]

let%expect_test "drop" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let rec test_drop_up_to i l n =
    (* l + 1 so that we test dropping one more than the stream contains *)
    match i <= l + 1, l <= n with
    | _, false -> ()
    | false, _ -> test_drop_up_to 0 (succ l) n
    | true, true -> begin
        let t = init l ~f:(fun i -> i) in
        let t' = drop i t in
        printf "drop %a %a = %a\n" Usize.pp i ppt t ppt t';
        test_drop_up_to (succ i) l n
      end in
  test_drop_up_to 0 0 3;
  printf "@]";

  [%expect{|
    drop 0 Nil = Nil
    drop 1 Nil = Nil
    drop 0 (0 Nil) = (0 Nil)
    drop 1 (0 Nil) = Nil
    drop 2 (0 Nil) = Nil
    drop 0 (0 (1 Nil)) = (0 (1 Nil))
    drop 1 (0 (1 Nil)) = (1 Nil)
    drop 2 (0 (1 Nil)) = Nil
    drop 3 (0 (1 Nil)) = Nil
    drop 0 (0 (1 (2 Nil))) = (0 (1 (2 Nil)))
    drop 1 (0 (1 (2 Nil))) = (1 (2 Nil))
    drop 2 (0 (1 (2 Nil))) = (2 Nil)
    drop 3 (0 (1 (2 Nil))) = Nil
    drop 4 (0 (1 (2 Nil))) = Nil
    |}]

let%expect_test "rev" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let rec test_rev_up_to i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t = init i ~f:(fun i -> i) in
        let t' = rev t in
        printf "rev %a = %a\n" ppt t ppt t';
        test_rev_up_to (succ i) n
      end
  end in
  test_rev_up_to 0 3;
  printf "@]";

  [%expect{|
    rev Nil = Nil
    rev (0 Nil) = (0 Nil)
    rev (0 (1 Nil)) = (1 (0 Nil))
    rev (0 (1 (2 Nil))) = (2 (1 (0 Nil)))
    |}]
