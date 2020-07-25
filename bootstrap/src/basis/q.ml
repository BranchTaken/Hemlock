open Rudiments0

type 'a t = uns * 'a Stream.t * 'a list * 'a Stream.t

let empty = (0, lazy Stream.Nil, [], lazy Stream.Nil)

let exec (l, f, r, s) =
  let rec rotate (l, f, r, s) = lazy begin
    match f, r with
    | _, [] -> not_reached ()
    | lazy Stream.Nil, y :: _ -> Lazy.force (Stream.push y s)
    | lazy (Stream.Cons(elm, f')), y :: y' -> begin
        let s' = Stream.push y s in
        let f'' = rotate (l, f', y', s') in
        Lazy.force (Stream.push elm f'')
      end
  end in
  match s with
  | lazy (Stream.Cons(_, s')) -> (l, f, r, s')
  | lazy Stream.Nil -> begin
      let f' = rotate (l, f, r, s) in
      (l, f', [], f')
    end

let length (l, _, _, _) =
  l

let is_empty (l, _, _, _) =
  l = 0

let hd (_, f, _, _) =
  match f with
  | lazy Stream.Nil -> halt "Empty q has no head"
  | lazy (Stream.Cons(elm, _)) -> elm

let tl (l, f, r, s) =
  match f with
  | lazy Stream.Nil -> halt "Empty q has no tail"
  | lazy (Stream.Cons(_, f')) -> exec (pred l, f', r, s)

let push_back elm (l, f, r, s) =
  exec (succ l, f, elm :: r, s)

let pop t =
  hd t, tl t

let pp pp_elm ppf (l, f, r, s) =
  let open Format in
  fprintf ppf "@[<h>";
  fprintf ppf "(len=%a,@ f=%a,@ r=%a,@ s=%a)"
    Uns.pp l (Stream.pp pp_elm) f (List.pp pp_elm) r (Stream.pp pp_elm) s;
  fprintf ppf "@]"

(******************************************************************************)
(* Begin tests. *)

let%expect_test "empty" =
  let open Format in
  let ppt = (pp Uns.pp) in
  printf "@[<h>";
  let t = empty in
  printf "empty = %a\n" ppt t;
  printf "@]";

  [%expect{|
    empty = (len=0, f=Nil, r=[], s=Nil)
    |}]

let%expect_test "length" =
  let open Format in
  let ppt = (pp Uns.pp) in
  printf "@[<h>";
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let l = length t in
        printf "length %a = %a\n" ppt t Uns.pp l;
        fn (succ i) n (push_back i t)
      end
  end in
  fn 0 3 empty;

  [%expect{|
    length (len=0, f=Nil, r=[], s=Nil) = 0
    length (len=1, f=(0 Nil), r=[], s=(0 Nil)) = 1
    length (len=2, f=(0 Nil), r=[1], s=Nil) = 2
    length (len=3, f=(0 (1 (2 Nil))), r=[], s=(0 (1 (2 Nil)))) = 3
    |}]

let%expect_test "is_empty" =
  let open Format in
  let ppt = (pp Uns.pp) in
  printf "@[<h>";
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let e = is_empty t in
        printf "is_empty %a = %b\n" ppt t e;
        fn (succ i) n (push_back i t)
      end
  end in
  fn 0 3 empty;

  [%expect{|
    is_empty (len=0, f=Nil, r=[], s=Nil) = true
    is_empty (len=1, f=(0 Nil), r=[], s=(0 Nil)) = false
    is_empty (len=2, f=(0 Nil), r=[1], s=Nil) = false
    is_empty (len=3, f=(0 (1 (2 Nil))), r=[], s=(0 (1 (2 Nil)))) = false
    |}]

let%expect_test "hd" =
  let open Format in
  let ppt = (pp Uns.pp) in
  printf "@[<h>";
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let elm = hd t in
        printf "hd %a = %a\n" ppt t Uns.pp elm;
        fn (succ i) n (push_back i t)
      end
  end in
  (* halts if we start with empty *)
  fn 1 4 (push_back 0 empty);
  printf "@]";

  [%expect{|
    hd (len=1, f=(0 Nil), r=[], s=(0 Nil)) = 0
    hd (len=2, f=(0 Nil), r=[1], s=Nil) = 0
    hd (len=3, f=(0 (1 (2 Nil))), r=[], s=(0 (1 (2 Nil)))) = 0
    hd (len=4, f=(0 (1 (2 Nil))), r=[3], s=(1 (2 Nil))) = 0
    |}]

let%expect_test "tl" =
  let open Format in
  let ppt = (pp Uns.pp) in
  printf "@[<h>";
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t' = tl t in
        printf "tl %a = %a\n" ppt t ppt t';
        fn (succ i) n (push_back i t)
      end
  end in
  (* halts if we start with empty *)
  fn 1 4 (push_back 0 empty);
  printf "@]";

  [%expect{|
    tl (len=1, f=(0 Nil), r=[], s=(0 Nil)) = (len=0, f=Nil, r=[], s=Nil)
    tl (len=2, f=(0 Nil), r=[1], s=Nil) = (len=1, f=(1 Nil), r=[], s=(1 Nil))
    tl (len=3, f=(0 (1 (2 Nil))), r=[], s=(0 (1 (2 Nil)))) = (len=2, f=(1 (2 Nil)), r=[], s=(1 (2 Nil)))
    tl (len=4, f=(0 (1 (2 Nil))), r=[3], s=(1 (2 Nil))) = (len=3, f=(1 (2 Nil)), r=[3], s=(2 Nil))
    |}]

let%expect_test "push_back" =
  let open Format in
  let ppt = (pp Uns.pp) in
  printf "@[<h>";
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t' = push_back i t in
        printf "push_back %a %a = %a\n" Uns.pp i ppt t ppt t';
        fn (succ i) n t'
      end
  end in
  fn 0 3 empty;
  printf "@]";

  [%expect{|
    push_back 0 (len=0, f=Nil, r=[], s=Nil) = (len=1, f=(0 Nil), r=[], s=(0 Nil))
    push_back 1 (len=1, f=(0 Nil), r=[], s=(0 Nil)) = (len=2, f=(0 Nil), r=[1], s=Nil)
    push_back 2 (len=2, f=(0 Nil), r=[1], s=Nil) = (len=3, f=(0 (1 (2 Nil))), r=[], s=(0 (1 (2 Nil))))
    push_back 3 (len=3, f=(0 (1 (2 Nil))), r=[], s=(0 (1 (2 Nil)))) = (len=4, f=(0 (1 (2 Nil))), r=[3], s=(1 (2 Nil)))
    |}]

let%expect_test "pop" =
  let open Format in
  let ppt = (pp Uns.pp) in
  printf "@[<h>";
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let elm, t' = pop t in
        printf "pop %a = %a %a\n" ppt t Uns.pp elm ppt t';
        fn (succ i) n (push_back i t)
      end
  end in
  (* halts if we start with empty *)
  fn 1 4 (push_back 0 empty);
  printf "@]";

  [%expect{|
    pop (len=1, f=(0 Nil), r=[], s=(0 Nil)) = 0 (len=0, f=Nil, r=[], s=Nil)
    pop (len=2, f=(0 Nil), r=[1], s=Nil) = 0 (len=1, f=(1 Nil), r=[], s=(1 Nil))
    pop (len=3, f=(0 (1 (2 Nil))), r=[], s=(0 (1 (2 Nil)))) = 0 (len=2, f=(1 (2 Nil)), r=[], s=(1 (2 Nil)))
    pop (len=4, f=(0 (1 (2 Nil))), r=[3], s=(1 (2 Nil))) = 0 (len=3, f=(1 (2 Nil)), r=[3], s=(2 Nil))
    |}]
