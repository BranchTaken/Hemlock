open Rudiments0

type 'a t = uns * 'a Stream.t * 'a list * 'a Stream.t

let empty = (0L, lazy Stream.Nil, [], lazy Stream.Nil)

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
  l = 0L

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

let fmt fmt_elm (l, f, r, s) formatter =
  formatter
  |> Fmt.fmt "(len="
  |> Uns.fmt l
  |> Fmt.fmt ", f="
  |> (Stream.fmt fmt_elm) f
  |> Fmt.fmt ", r="
  |> (List.fmt fmt_elm) r
  |> Fmt.fmt ", s="
  |> (Stream.fmt fmt_elm) s
  |> Fmt.fmt ")"
