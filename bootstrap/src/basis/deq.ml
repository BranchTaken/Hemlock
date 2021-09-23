open Rudiments0

type 'a t =
  uns * 'a Stream.t * 'a Stream.t * uns * 'a Stream.t * 'a Stream.t

(* Increment size for rebuilding; must be either 2 or 3. Reference: Double-ended queues section in
 * Purely Functional Data Structures by Chris Okasaki. *)
let c = 2L

let empty = (
  0L, lazy Stream.Nil, lazy Stream.Nil,
  0L, lazy Stream.Nil, lazy Stream.Nil
)

let length (lf, _, _, lr, _, _) =
  lf + lr

let is_empty (lf, _, _, lr, _, _) =
  lf + lr = 0L

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
  if lf > (c * lr) + 1L then
    let i = (lf + lr) / 2L in
    let j = lf + lr - i in
    let f' = Stream.take i f in
    let r' = rotate_drop r i f in
    (i, f', f', j, r', r')
  else if lr > (c * lf) + 1L then
    let j = (lf + lr) / 2L in
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
      let lf' = lf - 1L in
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
  let lf' = lf + 1L in
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
