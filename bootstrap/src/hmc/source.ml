open Basis
open Basis.Rudiments

type t = {
  text: Text.t;
  path: string option;
  line_bias: sint;
  col_bias: sint; (* Only applies to first biased line. *)
}

let pp {path; line_bias; col_bias; _} formatter =
  formatter
  |> Fmt.fmt "{text=..."
  |> Fmt.fmt "; path=" |> (Option.pp String.pp) path
  |> Fmt.fmt "; line_bias=" |> Sint.pp line_bias
  |> Fmt.fmt "; col_bias=" |> Sint.pp col_bias
  |> Fmt.fmt "}"

let init text =
  {
    text;
    path=Text.path text;
    line_bias=Sint.zero;
    col_bias=Sint.zero;
  }

let bias ~path ~line_bias ~col_bias t =
  {t with path; line_bias; col_bias}

let debias t =
  init t.text

let text t =
  t.text

let path t =
  t.path

let line_bias t =
  t.line_bias

let col_bias t =
  t.col_bias

module Cursor = struct
  module T = struct
    type container = t
    type t = {
      container: container;
      bias_pred: t option;
      text_cursor: Text.Cursor.t;
    }

    let bias container pred t =
      {
        container;
        bias_pred=Some pred;
        text_cursor=t.text_cursor;
      }

    let debias t =
      {t with container=debias t.container; bias_pred=None}

    let container t =
      t.container

    let index t =
      Text.Cursor.index t.text_cursor

    let bias_pred t =
      t.bias_pred

    let text_cursor t =
      t.text_cursor

    let cmp t0 t1 =
      Text.Cursor.cmp t0.text_cursor t1.text_cursor

    let hd container =
      {
        container;
        bias_pred=None;
        text_cursor=Text.Cursor.hd container.text;
      }

    (* Bias cannot be managed here, and the scanner is written to not need tl. *)
    let tl container =
      {
        container;
        bias_pred=None;
        text_cursor=Text.Cursor.tl container.text;
      }

    let succ t =
      {t with text_cursor=Text.Cursor.succ t.text_cursor}

    let pred t =
      let text_cursor' = Text.Cursor.pred t.text_cursor in
      match t.bias_pred with
      | None -> {t with text_cursor=text_cursor'}
      | Some bias_pred -> begin
          match Text.Cursor.(bias_pred.text_cursor < text_cursor') with
          | true -> {t with text_cursor=text_cursor'}
          | false -> bias_pred
        end

    let seek_fwd offset t =
      let rec fn offset t = begin
        match offset with
        | 0L -> t
        | _ -> fn (Uns.pred offset) (succ t)
      end in
      fn offset t

    let seek_rev offset t =
      let rec fn offset t = begin
        match offset with
        | 0L -> t
        | _ -> fn (Uns.pred offset) (pred t)
      end in
      fn offset t

    let seek offset t =
      if Sint.(offset < 0L) then seek_rev (Uns.bits_of_sint (Sint.neg offset)) t
      else seek_fwd (Uns.bits_of_sint offset) t

    let lget t =
      Text.Cursor.lget t.text_cursor

    let rget t =
      Text.Cursor.rget t.text_cursor

    let prev t =
      lget t, pred t

    let next t =
      rget t, succ t

    let next_opt t =
      match Text.Cursor.next_opt t.text_cursor with
      | None -> None
      | Some (cp, text_cursor) -> Some (cp, {t with text_cursor})

    let nextv_opt t =
      match Text.Cursor.nextv_opt t.text_cursor with
      | None -> None
      | Some (cp, valid, text_cursor) -> Some (cp, valid, {t with text_cursor})

    let pos t =
      let line = Uns.bits_of_sint (Sint.((Uns.bits_to_sint (Text.Pos.line (Text.Cursor.pos
          t.text_cursor)) + t.container.line_bias))) in
      let col = match t.bias_pred with
        | None -> Text.Pos.col (Text.Cursor.pos t.text_cursor)
        | Some bias_pred -> begin
            let first_line = Text.Pos.line (Text.Cursor.pos bias_pred.text_cursor) in
            let cur_line = Text.Pos.line (Text.Cursor.pos t.text_cursor) in
            match cur_line = first_line with
            | false -> Text.Pos.col (Text.Cursor.pos t.text_cursor)
            | true ->
              Uns.bits_of_sint (Sint.((Uns.bits_to_sint (Text.Pos.col (Text.Cursor.pos
                  t.text_cursor)) + t.container.col_bias)))
          end
      in
      Text.Pos.init ~line ~col

    let pp t formatter =
      formatter
      |> (fun formatter -> (match path t.container with
        | None -> formatter
        | Some path ->
          formatter
          |> Fmt.fmt path
          |> Fmt.fmt ":"
      ))
      |> Text.Pos.pp (pos t)
  end
  include T
  include Cmpable.Make(T)
end

module Slice = struct
  type cursor = Cursor.t
  type t = {
    base: cursor;
    past: cursor;
  }

  let pp {base; past} formatter =
    formatter
    |> (fun formatter -> (match path (Cursor.container base) with
      | None -> formatter
      | Some path ->
        formatter
        |> Fmt.fmt path
        |> Fmt.fmt ":"
    ))
    |> Fmt.fmt "["
    |> Text.Pos.pp (Cursor.(pos base))
    |> Fmt.fmt ".."
    |> Text.Pos.pp (Cursor.(pos past))
    |> Fmt.fmt ")"

  let cmp t0 t1 =
    let open Cmp in
    match Cursor.cmp t0.base t1.base with
    | Lt -> Lt
    | Eq -> Cursor.cmp t0.past t1.past
    | Gt -> Gt

  let init ?base ?past container =
    let base = match base with
      | None -> Cursor.hd container
      | Some base -> base
    in
    let past = match past with
      | None -> Cursor.tl container
      | Some past -> past
    in
    {base; past}

  let of_cursors ~base ~past =
    {base; past}

  let container t =
    Cursor.container t.base

  let base t =
    t.base

  let past t =
    t.past

  let cursors t =
    t.base, t.past

  let line_context t =
    let rec bol cursor = begin
      match Text.Cursor.index (Cursor.text_cursor cursor) with
      | 0L -> cursor
      | _ -> begin
          match Cursor.prev cursor with
          | cp, _ when Codepoint.(cp = nl) -> cursor
          | _, cursor' -> begin
              match Cursor.bias_pred cursor with
              | None -> bol cursor'
              | Some bp -> begin
                  match Cursor.(bp < cursor') with
                  | true -> bol cursor'
                  | false -> bol (Cursor.debias cursor')
                end
            end
        end
    end in
    let rec eol cursor = begin
      match Cursor.next_opt cursor with
      | None -> cursor
      | Some (cp, _) when Codepoint.(cp = nl) -> cursor
      | Some (_, cursor') -> eol (Cursor.debias cursor')
    end in
    {base=bol t.base; past=eol t.past}

  let to_string t =
    let base = t.base.text_cursor in
    let past = t.past.text_cursor in
    let container = text (container t) in
    Text.Slice.to_string (Text.Slice.init ~base ~past container)
end
