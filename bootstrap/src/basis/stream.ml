open Rudiments0

type 'a elm =
  | Nil
  | Cons of 'a * 'a t
and 'a t = 'a elm lazy_t

let empty = lazy Nil

let init range ~f =
  let rec fn i past f = lazy begin
    match i < past with
    | false -> Nil
    | true -> begin
        let elm = f i in
        let t = fn (succ i) past f in
        Cons(elm, t)
      end
  end in
  fn (Range.base range) (Range.past range) f

let rec init_indef ~f state =
  lazy begin
    match f state with
    | None -> Nil
    | Some(elm, state') -> begin
        let t = init_indef ~f state' in
        Cons(elm, t)
      end
  end

let length t =
  let rec fn t i = begin
    match t with
    | lazy Nil -> i
    | lazy (Cons(_, t')) -> fn t' (succ i)
  end in
  fn t 0L

let is_empty = function
  | lazy Nil -> true
  | lazy (Cons(_, _)) -> false

let hd = function
  | lazy Nil -> halt "Empty stream has no head"
  | lazy (Cons(elm, _)) -> elm

let tl = function
  | lazy Nil -> halt "Empty stream has no tail"
  | lazy (Cons(_, t)) -> t

(* Non-lazy internal function of push (conventionally named push'). It is exposed outside of push
 * since it is useful within non-lazy sections of other stream functions. *)
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
    | _, 0L -> Nil
    | lazy Nil, _ -> Lazy.force t
    | lazy (Cons(elm, t')), _ -> push' elm (take (pred n) t')
  end

let drop n t =
  let rec drop' n t = begin
    match t, n with
    | _, 0L -> Lazy.force t
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

let xpp xpp_elm xppf t =
  let open Format in
  fprintf xppf "@[<h>";
  let rec fn t = begin
    match t with
    | lazy Nil -> fprintf xppf "Nil"
    | lazy (Cons(elm, t')) -> begin
        fprintf xppf "(%a@ " xpp_elm elm;
        fn t';
        fprintf xppf ")"
      end
  end in
  fn t;
  fprintf xppf "@]"

let fmt fmt_elm t formatter =
  let rec fn t formatter = begin
    match t with
    | lazy Nil -> formatter |> Fmt.fmt "Nil"
    | lazy (Cons (elm, t')) ->
      formatter
      |> Fmt.fmt "("
      |> fmt_elm elm
      |> Fmt.fmt " "
      |> fn t'
      |> Fmt.fmt ")"
  end in
  formatter |> fn t
