open Rudiments

type 'a t =
  usize * 'a Stream.t * 'a Stream.t * usize * 'a Stream.t * 'a Stream.t

(* Increment size for rebuilding; must be either 2 or 3.  Reference:
 * Double-ended queues section in Purely Functional Data Structures by Chris
 * Okasaki. *)
let c = 2

let empty = (
  0, lazy Stream.Nil, lazy Stream.Nil,
  0, lazy Stream.Nil, lazy Stream.Nil
)

let length (lf, _, _, lr, _, _) =
  lf + lr

let is_empty (lf, _, _, lr, _, _) =
  lf + lr = 0

let exec1 = function
  | lazy (Stream.Cons(_, s')) -> s'
  | s -> s

let exec2 s =
  exec1 (exec1 s)

let rec rotate_rev (f, r, a) =
  lazy begin
    match f with
    | lazy Stream.Nil -> Lazy.force (Stream.concat (Stream.rev r) a)
    | lazy (Stream.Cons(elm, f')) -> begin
        let r' = Stream.drop c r in
        let a' = Stream.concat (Stream.rev (Stream.take c r)) a in
        Lazy.force (Stream.push elm (rotate_rev (f', r', a')))
      end
  end

let rec rotate_drop f j r =
  lazy begin
    match j < c, f with
    | true, _ -> Lazy.force (rotate_rev (f, (Stream.drop j r), Stream.empty))
    | false, lazy Stream.Nil -> not_reached ()
    | false, lazy (Stream.Cons(elm, f')) ->
      Lazy.force (Stream.push elm (rotate_drop f' (j - c) (Stream.drop c r)))
  end

let check t =
  let lf, f, _, lr, r, _ = t in
  if lf > (c * lr) + 1 then
    let i = (lf + lr) / 2 in
    let j = lf + lr - i in
    let f' = Stream.take i f in
    let r' = rotate_drop r i f in
    (i, f', f', j, r', r')
  else if lr > (c * lf) + 1 then
    let j = (lf + lr) / 2 in
    let i = lf + lr - j in
    let r' = Stream.take j r in
    let f' = rotate_drop f j r in
    (i, f', f', j, r', r')
  else
    t

let hd (_, f, _, _, r, _) =
  match f, r with
  | lazy Stream.Nil, lazy Stream.Nil -> not_reached ()
  | lazy Stream.Nil, lazy (Stream.Cons(elm, _)) -> elm
  | lazy (Stream.Cons(elm, _)), _ -> elm

let tl (lf, f, sf, lr, r, sr) =
  match f, r with
  | lazy Stream.Nil, lazy Stream.Nil -> not_reached ()
  | lazy Stream.Nil, lazy (Stream.Cons(_, _)) -> empty
  | lazy (Stream.Cons(_, f')), _ -> begin
      let lf' = lf - 1 in
      let sf' = exec2 sf in
      let sr' = exec2 sr in
      check (lf', f', sf', lr, r, sr')
    end

let rev (lf, f, sf, lr, r, sr) =
  (lr, r, sr, lf, f, sf)

let back t =
  hd (rev t)

let front t =
  rev (tl (rev t))

let push elm (lf, f, sf, lr, r, sr) =
  let lf' = lf + 1 in
  let f' = Stream.push elm f in
  let sf' = exec1 sf in
  let sr' = exec1 sr in
  check (lf', f', sf', lr, r, sr')

let push_back elm t =
  rev (push elm (rev t))

let pop t =
  hd t, tl t

let pop_back t =
  back t, front t

let pp pp_elm ppf (_lf, f, _sf, _lr, r, _sr) =
  let open Format in
  fprintf ppf "@[<h>";
  let rec fn s = begin
    match s with
    | lazy Stream.Nil -> ()
    | lazy (Cons(elm, lazy Stream.Nil)) -> fprintf ppf "%a" pp_elm elm;
    | lazy (Cons(elm, s')) -> begin
        fprintf ppf "%a,@ " pp_elm elm;
        fn s'
      end
  end in
  fprintf ppf "(";
  fn f;
  fprintf ppf "@ |@ ";
  fn (Stream.rev r);
  fprintf ppf ")";
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
    empty = ( | )
    |}]

let%expect_test "length" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let l = length t in
        printf "length %a = %a\n" ppt t Usize.pp l;
        fn (succ i) n (push i t)
      end
  end in
  fn 0 3 empty;

  [%expect{|
    length ( | ) = 0
    length (0 | ) = 1
    length (1 | 0) = 2
    length (2, 1 | 0) = 3
    |}]

let%expect_test "is_empty" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let e = is_empty t in
        printf "is_empty %a = %b\n" ppt t e;
        fn (succ i) n (push i t);
        fn (succ i) n (push_back i t)
      end
  end in
  fn 0 1 empty;

  [%expect{|
    is_empty ( | ) = true
    is_empty (0 | ) = false
    is_empty ( | 0) = false
    |}]

let%expect_test "hd" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t' = push_back i t in
        let elm = hd t' in
        printf "hd %a = %a\n" ppt t' Usize.pp elm;
        fn (succ i) n t'
      end
  end in
  fn 0 4 empty;
  printf "@]";

  [%expect{|
    hd ( | 0) = 0
    hd (0 | 1) = 0
    hd (0 | 1, 2) = 0
    hd (0 | 1, 2, 3) = 0
    hd (0, 1, 2 | 3, 4) = 0
    |}]

let%expect_test "tl" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t' = push_back i t in
        let t'' = tl t' in
        printf "tl %a = %a\n" ppt t' ppt t'';
        fn (succ i) n t'
      end
  end in
  fn 0 4 empty;
  printf "@]";

  [%expect{|
    tl ( | 0) = ( | )
    tl (0 | 1) = ( | 1)
    tl (0 | 1, 2) = (1 | 2)
    tl (0 | 1, 2, 3) = (1, 2 | 3)
    tl (0, 1, 2 | 3, 4) = (1, 2 | 3, 4)
    |}]

let%expect_test "back" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t' = push i t in
        let elm = back t' in
        printf "back %a = %a\n" ppt t' Usize.pp elm;
        fn (succ i) n t'
      end
  end in
  fn 0 4 empty;
  printf "@]";

  [%expect{|
    back (0 | ) = 0
    back (1 | 0) = 0
    back (2, 1 | 0) = 0
    back (3, 2, 1 | 0) = 0
    back (4, 3 | 2, 1, 0) = 0
    |}]

let%expect_test "front" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t' = push i t in
        let t'' = front t' in
        printf "front %a = %a\n" ppt t' ppt t'';
        fn (succ i) n t'
      end
  end in
  fn 0 4 empty;
  printf "@]";

  [%expect{|
    front (0 | ) = ( | )
    front (1 | 0) = (1 | )
    front (2, 1 | 0) = (2 | 1)
    front (3, 2, 1 | 0) = (3 | 2, 1)
    front (4, 3 | 2, 1, 0) = (4, 3 | 2, 1)
    |}]

let%expect_test "push" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t' = push i t in
        printf "push %a %a = %a\n" Usize.pp i ppt t ppt t';
        fn (succ i) n t'
      end
  end in
  fn 0 4 empty;
  printf "@]";

  [%expect{|
    push 0 ( | ) = (0 | )
    push 1 (0 | ) = (1 | 0)
    push 2 (1 | 0) = (2, 1 | 0)
    push 3 (2, 1 | 0) = (3, 2, 1 | 0)
    push 4 (3, 2, 1 | 0) = (4, 3 | 2, 1, 0)
    |}]

let%expect_test "push_back" =
  let open Format in
  let ppt = (pp Usize.pp) in
  printf "@[<h>";
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t' = push_back i t in
        printf "push_back %a %a = %a\n" Usize.pp i ppt t ppt t';
        fn (succ i) n t'
      end
  end in
  fn 0 4 empty;
  printf "@]";

  [%expect{|
    push_back 0 ( | ) = ( | 0)
    push_back 1 ( | 0) = (0 | 1)
    push_back 2 (0 | 1) = (0 | 1, 2)
    push_back 3 (0 | 1, 2) = (0 | 1, 2, 3)
    push_back 4 (0 | 1, 2, 3) = (0, 1, 2 | 3, 4)
    |}]
