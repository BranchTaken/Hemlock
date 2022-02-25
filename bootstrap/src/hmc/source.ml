open Basis
open Basis.Rudiments

type t = {
  text: Text.t;
  path: Path.t option;
  line_bias: sint;
  col_bias: sint; (* Only applies to first biased line. *)
}

let pp {path; line_bias; col_bias; _} formatter =
  formatter
  |> Fmt.fmt "{text=..."
  |> Fmt.fmt "; path=" |> (Option.pp Path.pp) path
  |> Fmt.fmt "; line_bias=" |> Sint.pp line_bias
  |> Fmt.fmt "; col_bias=" |> Sint.pp col_bias
  |> Fmt.fmt "}"

module O = struct
  let ( = ) t0 t1 =
    (* text is assumed to be equivalent. *)
    match t0.path, t1.path with
    | None, Some _
    | Some _, None -> false
    | Some path0, Some path1
      when String.(<>) (Path.to_string_replace path0) (Path.to_string_replace path1) -> false
    | None, None
    | Some _, Some _ -> t0.line_bias = t1.line_bias && t0.col_bias = t1.col_bias
end

let init text =
  {
    text;
    path=Text.path text;
    line_bias=Sint.zero;
    col_bias=Sint.zero;
  }

let bias ~path ~line_bias ~col_bias t =
  {t with path; line_bias; col_bias}

let unbias t =
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
      bias_prior: t option;
      text_cursor: Text.Cursor.t;
    }

    let bias container t =
      {
        container;
        bias_prior=Some t;
        text_cursor=t.text_cursor;
      }

    let debias t =
      let container, bias_prior = match t.bias_prior with
        | None -> t.container, None
        | Some prior -> prior.container, prior.bias_prior
      in
      {t with container; bias_prior}

    let unbias t =
      {t with container=unbias t.container; bias_prior=None}

    let container t =
      t.container

    let index t =
      Text.Cursor.index t.text_cursor

    let bias_prior t =
      match t.bias_prior with
      | None -> t
      | Some bias_prior -> bias_prior

    let text_cursor t =
      t.text_cursor

    let rec cmp t0 t1 =
      let open Cmp in
      match Text.Cursor.cmp t0.text_cursor t1.text_cursor with
      | Lt -> Lt
      | Eq -> begin
          match t0.bias_prior, t1.bias_prior with
          | None, None -> Eq
          | None, Some _ -> Lt
          | Some bias_prior0, Some bias_prior1 -> cmp bias_prior0 bias_prior1
          | Some _, None -> Gt
        end
      | Gt -> Gt

    let hd container =
      {
        container;
        bias_prior=None;
        text_cursor=Text.Cursor.hd container.text;
      }

    (* Bias cannot be managed here, and the scanner is written to not need tl. *)
    let tl container =
      {
        container;
        bias_prior=None;
        text_cursor=Text.Cursor.tl container.text;
      }

    let succ t =
      {t with text_cursor=Text.Cursor.succ t.text_cursor}

    let rec pred t =
      let text_cursor' = Text.Cursor.pred t.text_cursor in
      match t.bias_prior with
      | None -> {t with text_cursor=text_cursor'}
      | Some bias_prior -> begin
          match Text.Cursor.(bias_prior.text_cursor < t.text_cursor) with
          | true -> {t with text_cursor=text_cursor'}
          | false -> pred bias_prior
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

    let rvalid t =
      Text.Cursor.rvalid t.text_cursor

    let pos t =
      let line = Uns.bits_of_sint (Sint.((Uns.bits_to_sint (Text.Pos.line (Text.Cursor.pos
          t.text_cursor)) + t.container.line_bias))) in
      let col = match t.bias_prior with
        | None -> Text.Pos.col (Text.Cursor.pos t.text_cursor)
        | Some bias_prior -> begin
            let first_line = Text.Pos.line (Text.Cursor.pos bias_prior.text_cursor) in
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
          |> Fmt.fmt (Path.to_string_replace path)
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
    |> Fmt.fmt "["
    |> (fun formatter -> (match path (Cursor.container base) with
      | None -> formatter
      | Some path ->
        formatter
        |> String.pp (Path.to_string_replace path)
        |> Fmt.fmt ":"
    ))
    |> Text.Pos.pp (Cursor.(pos base))
    |> Fmt.fmt ".."
    |> (fun formatter -> (match path (Cursor.container past) with
      | None -> formatter
      | Some path ->
        formatter
        |> String.pp (Path.to_string_replace path)
        |> Fmt.fmt ":"
    ))
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

  let line_context ?lookahead t =
    let line_of_cursor cursor = begin
      Text.Pos.line (Cursor.pos (Cursor.unbias (cursor)))
    end in
    let start_context last_line start = begin
      let rec expand_rightwards last_line base cursor = begin
        let context_of_cursors base past = begin
          match Cursor.(base < past) with
          | false -> []
          | true -> [of_cursors ~base ~past]
        end in
        match Cursor.next_opt cursor with
        | None -> context_of_cursors base cursor
        | Some (_cp, cursor') -> begin
            match last_line < (line_of_cursor cursor') with
            | true -> context_of_cursors base cursor
            | false -> expand_rightwards last_line base cursor'
          end
      end in
      let rec contract_leftwards last_line cursor = begin
        match Cursor.index cursor = 0L with
        | true -> cursor
        | false -> begin
            let cursor' = Cursor.pred cursor in
            match line_of_cursor cursor' > last_line with
            | false -> cursor'
            | true -> contract_leftwards last_line cursor'
          end
      end in
      match line_of_cursor start <= last_line with
      | true -> start, expand_rightwards last_line (Cursor.unbias start) (Cursor.unbias start)
      | false -> contract_leftwards last_line start, []
    end in
    let leftwards first_line right right_context = begin
      let merge_context base_ past_ context = begin
        assert O.(Cursor.container base_ = (Cursor.container past_));
        match Cursor.(base_ = past_) with
        | true -> context
        | false -> begin
            match context with
            | [] -> [of_cursors ~base:base_ ~past:past_]
            | slice :: context' -> begin
                match O.(Cursor.container base_ = (Cursor.container (past slice))) with
                | true -> of_cursors ~base:base_ ~past:(past slice) :: context'
                | false -> of_cursors ~base:base_ ~past:past_ :: context
              end
          end
      end in
      let rec expand_leftwards first_line cursor past context = begin
        assert O.(Cursor.container cursor = (Cursor.container past));
        match Cursor.index cursor = 0L with
        | true -> merge_context cursor past context
        | false -> begin
            let cursor' = Cursor.pred cursor in
            match line_of_cursor cursor' >= first_line with
            | false -> merge_context cursor past context
            | true -> begin
                match O.(Cursor.container cursor' = (Cursor.container past)) with
                | false ->
                  let cursor' = Cursor.bias_prior cursor in
                  expand_leftwards first_line cursor' cursor' (merge_context cursor past context)
                | true -> expand_leftwards first_line cursor' past context
              end
          end
      end in
      expand_leftwards first_line right right right_context
    end in
    let last_line = line_of_cursor (past t) in
    let start = match lookahead with
      | Some lookahead -> lookahead
      | None -> t.past
    in
    let first_line = line_of_cursor (base t) in
    let right, right_context = start_context last_line start in
    leftwards first_line right right_context

  let to_string t =
    let base = t.base.text_cursor in
    let past = t.past.text_cursor in
    let container = text (container t) in
    Text.Slice.to_string (Text.Slice.init ~base ~past container)
end
