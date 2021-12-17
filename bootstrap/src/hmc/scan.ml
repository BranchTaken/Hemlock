open Basis
open Basis.Rudiments

let nat_digit_map = String.foldi ~init:(Map.empty (module Codepoint))
  ~f:(fun i digit_map cp ->
    Map.insert_hlt ~k:cp ~v:(Nat.of_uns i) digit_map
  ) "0123456789abcdef"

let nat_of_cp digit =
  Map.get_hlt digit nat_digit_map

let zint_digit_map = String.foldi ~init:(Map.empty (module Codepoint))
  ~f:(fun i digit_map cp ->
    Map.insert_hlt ~k:cp ~v:(Zint.of_uns i) digit_map
  ) "0123456789abcdef"

let zint_of_cp digit =
  Map.get_hlt digit zint_digit_map

module Radix = struct
  type t =
    | Bin
    | Oct
    | Dec
    | Hex

  let to_nat = function
    | Bin -> Nat.k_2
    | Oct -> Nat.k_8
    | Dec -> Nat.k_a
    | Hex -> Nat.k_g

  let nat_accum digit nat t =
    Nat.(nat * (to_nat t) + digit)

  let to_zint = function
    | Bin -> Zint.k_2
    | Oct -> Zint.k_8
    | Dec -> Zint.k_a
    | Hex -> Zint.k_g

  let zint_accum digit zint t =
    Zint.(zint * (to_zint t) + digit)
end

module Source = struct
  type t = {
    path: string option;
    line_bias: sint;
    col_bias: sint;
    base: Text.Cursor.t;
    past: Text.Cursor.t
  }

  let init ~path ~line_bias ~col_bias ~base ~past =
    {path; line_bias; col_bias; base; past}

  let path t =
    t.path

  let cmp t0 t1 =
    let open Cmp in
    match Text.Cursor.cmp t0.base t1.base with
    | Lt -> Lt
    | Eq -> Text.Cursor.cmp t0.past t1.past
    | Gt -> Gt

  module Cursor = struct
    module T = struct
      type container = t
      type elm = codepoint
      type t = {
        source: container;
        text_cursor: Text.Cursor.t;
      }

      let cmp t0 t1 =
        Text.Cursor.cmp t0.text_cursor t1.text_cursor

      let container t =
        t.source

      let index t =
        Text.Cursor.((index t.text_cursor) - (index t.source.base))

      let hd source =
        {source; text_cursor=source.base}

      let tl source =
        {source; text_cursor=source.past}

      let prev t =
        match Text.Cursor.(t.text_cursor = t.source.base) with
        | true -> halt "Out of bounds"
        | false -> begin
            let cp, text_cursor' = Text.Cursor.prev t.text_cursor in
            cp, {t with text_cursor=text_cursor'}
          end

      let next t =
        match Text.Cursor.(t.text_cursor = t.source.past) with
        | true -> halt "Out of bounds"
        | false -> begin
            let cp, text_cursor' = Text.Cursor.next t.text_cursor in
            cp, {t with text_cursor=text_cursor'}
          end

      let pred t =
        match prev t with _, t' -> t'

      let succ t =
        match next t with _, t' -> t'

      let lget t =
        match prev t with cp, _ -> cp

      let rget t =
        match next t with cp, _ -> cp

      let pos t =
        let text_pos = Text.Cursor.pos t.text_cursor in
        let line = Uns.bits_of_sint
            (Sint.((Uns.bits_to_sint (Text.Pos.line text_pos)) + t.source.line_bias)) in
        let col = Uns.bits_of_sint
            (Sint.((Uns.bits_to_sint (Text.Pos.col text_pos)) + t.source.col_bias)) in
        Text.Pos.init ~line ~col

      let seek_rev offset t =
        match offset > index t with
        | true -> halt "Out of bounds"
        | false ->
          {t with text_cursor=Text.Cursor.seek (Sint.neg (Uns.bits_to_sint offset)) t.text_cursor}

      let seek_fwd offset t =
        match (index t) + offset > (Text.Cursor.index t.source.past) with
        | true -> halt "Out of bounds"
        | false ->
          {t with text_cursor=Text.Cursor.seek (Uns.bits_to_sint offset) t.text_cursor}

      let seek offset t =
        match Sint.(offset < 0L) with
        | true -> seek_rev (Uns.bits_of_sint (Sint.neg offset)) t
        | false -> seek_fwd (Uns.bits_of_sint offset) t
    end
    include T
    include Cmpable.Make(T)
  end

  module Slice = struct
    include Slice.MakeMonoIndex(Cursor)
  end

  let line_context t =
    let rec bol text_cursor = begin
      match Text.Cursor.index text_cursor with
      | 0L -> text_cursor
      | _ -> begin
          match Text.Cursor.prev text_cursor with
          | cp, _ when Codepoint.(cp = nl) -> text_cursor
          | _, text_cursor' -> bol text_cursor'
        end
    end in
    let rec eol text_cursor = begin
      match Text.Cursor.next_opt text_cursor with
      | None -> text_cursor
      | Some (cp, _) when Codepoint.(cp = nl) -> text_cursor
      | Some (_, text_cursor') -> eol text_cursor'
    end in
    let t' = {t with base=bol t.base; past=eol t.past} in
    let hd' = {(Cursor.hd t) with source=t'} in
    let tl' = {(Cursor.tl t) with source=t'} in
    let slice = Slice.of_cursors ~base:hd' ~past:tl' in
    slice, t'

  let pp_loc t formatter =
    formatter
    |> (fun formatter -> (match t.path with
      | None -> formatter
      | Some path ->
        formatter
        |> Fmt.fmt path
        |> Fmt.fmt ":"
    ))
    |> Fmt.fmt "["
    |> Text.Pos.pp (Cursor.(pos (hd t)))
    |> Fmt.fmt ".."
    |> Text.Pos.pp (Cursor.(pos (tl t)))
    |> Fmt.fmt ")"

  let pp t formatter =
    formatter
    |> Fmt.fmt "{path=" |> (Option.pp String.pp) t.path
    |> Fmt.fmt "; line_bias=" |> Sint.pp t.line_bias
    |> Fmt.fmt "; col_bias=" |> Sint.pp t.col_bias
    |> Fmt.fmt "; base=" |> Text.Cursor.pp t.base
    |> Fmt.fmt "; past=" |> Text.Cursor.pp t.past
    |> Fmt.fmt "; [base..past)="
    |> Text.Slice.(pp (init ~base:t.base ~past:t.past (Text.Cursor.container t.base)))
    |> Fmt.fmt "}"
end

module AbstractToken = struct
  module Rendition = struct
    module Malformation = struct
      type t = {
        source: Source.t;
        description: string;
      }

      let cmp t0 t1 =
        Source.cmp t0.source t1.source

      let init ~path ~line_bias ~col_bias ~base ~past ~description =
        let source = Source.init ~path ~line_bias ~col_bias ~base ~past in
        {source; description}

      let source t =
        t.source

      let description t =
        t.description

      let pp t formatter =
        formatter
        |> Fmt.fmt "\""
        |> Source.pp_loc t.source
        |> Fmt.fmt ": "
        |> Fmt.fmt t.description
        |> Fmt.fmt "\""
    end

    type 'a t =
      | Constant of 'a
      | Malformed of Malformation.t list

    let pp pp_a t formatter =
      match t with
      | Constant a ->
        formatter
        |> Fmt.fmt "Constant "
        |> pp_a a
      | Malformed malformations ->
        formatter
        |> Fmt.fmt "Malformed "
        |> (List.pp Malformation.pp) malformations

    let pp_unit tok_name t formatter =
      formatter
      |> Fmt.fmt tok_name
      |> (function formatter -> match t with
        | Constant _ -> formatter
        | Malformed _ -> formatter |> Fmt.fmt "=" |> pp Unit.pp t
      )

    let of_mals mals =
      Malformed (List.sort ~cmp:Malformation.cmp mals)
  end

  type source_directive = {
    path: string option;
    line: uns;
    col: uns;
  }

  let pp_source_directive {path; line; col} formatter =
    formatter
    |> Fmt.fmt "{path=" |> (Option.pp String.pp) path
    |> Fmt.fmt "; line=" |> Uns.pp line
    |> Fmt.fmt "; col=" |> Uns.pp col
    |> Fmt.fmt "}"

  type t =
    (* Keywords. *)
    | Tok_and
    | Tok_also
    | Tok_as
    | Tok_assert
    | Tok_conceal
    | Tok_effect
    | Tok_else
    | Tok_expose
    | Tok_external
    | Tok_false
    | Tok_fun
    | Tok_function
    | Tok_if
    | Tok_import
    | Tok_include
    | Tok_lazy
    | Tok_let
    | Tok_match
    | Tok_mutability
    | Tok_of
    | Tok_open
    | Tok_or
    | Tok_rec
    | Tok_then
    | Tok_true
    | Tok_type
    | Tok_val
    | Tok_when
    | Tok_with

    (* Operators. *)
    | Tok_tilde_op of string
    | Tok_qmark_op of string
    | Tok_star_star_op of string
    | Tok_star_op of string
    | Tok_slash_op of string
    | Tok_pct_op of string
    | Tok_plus_op of string
    | Tok_minus_op of string
    | Tok_at_op of string
    | Tok_caret_op of string
    | Tok_dollar_op of string
    | Tok_lt_op of string
    | Tok_eq_op of string
    | Tok_gt_op of string
    | Tok_bar_op of string
    | Tok_colon_op of string
    | Tok_dot_op of string

    (* Punctuation. *)
    | Tok_tilde
    | Tok_qmark
    | Tok_minus
    | Tok_lt
    | Tok_lt_eq
    | Tok_eq
    | Tok_lt_gt
    | Tok_gt_eq
    | Tok_gt
    | Tok_comma
    | Tok_dot
    | Tok_semi
    | Tok_semi_semi
    | Tok_colon
    | Tok_colon_colon
    | Tok_colon_eq
    | Tok_lparen
    | Tok_rparen
    | Tok_lbrack
    | Tok_rbrack
    | Tok_lcurly
    | Tok_rcurly
    | Tok_bar
    | Tok_larray
    | Tok_rarray
    | Tok_lmodule
    | Tok_rmodule
    | Tok_bslash
    | Tok_tick
    | Tok_caret
    | Tok_amp
    | Tok_xmark
    | Tok_arrow
    | Tok_carrow

    | Tok_source_directive of source_directive Rendition.t
    | Tok_indent of unit Rendition.t
    | Tok_line_delim
    | Tok_dedent of unit Rendition.t
    | Tok_whitespace
    | Tok_hash_comment
    | Tok_paren_comment of unit Rendition.t
    | Tok_uscore
    | Tok_uident of string Rendition.t
    | Tok_cident of string
    | Tok_codepoint of codepoint Rendition.t
    | Tok_istring_lditto
    | Tok_isubstring of string Rendition.t
    | Tok_istring_pct
    | Tok_istring_lparen_caret
    | Tok_istring_caret_rparen
    | Tok_istring_rditto
    | Tok_rstring of string Rendition.t
    | Tok_bstring of string Rendition.t
    | Tok_r32 of real Rendition.t
    | Tok_r64 of real Rendition.t
    | Tok_u8 of u8 Rendition.t
    | Tok_i8 of i8 Rendition.t
    | Tok_u16 of u16 Rendition.t
    | Tok_i16 of i16 Rendition.t
    | Tok_u32 of u32 Rendition.t
    | Tok_i32 of i32 Rendition.t
    | Tok_u64 of u64 Rendition.t
    | Tok_i64 of i64 Rendition.t
    | Tok_u128 of u128 Rendition.t
    | Tok_i128 of i128 Rendition.t
    | Tok_u256 of u256 Rendition.t
    | Tok_i256 of i256 Rendition.t
    | Tok_u512 of u512 Rendition.t
    | Tok_i512 of i512 Rendition.t
    | Tok_end_of_input
    | Tok_misaligned
    | Tok_error

  let pp t formatter =
    formatter
    |> Fmt.fmt "<"
    |> (fun formatter ->
      match t with
      (* Keywords. *)
      | Tok_and -> formatter |> Fmt.fmt "Tok_and"
      | Tok_also -> formatter |> Fmt.fmt "Tok_also"
      | Tok_as -> formatter |> Fmt.fmt "Tok_as"
      | Tok_assert -> formatter |> Fmt.fmt "Tok_assert"
      | Tok_conceal -> formatter |> Fmt.fmt "Tok_conceal"
      | Tok_effect -> formatter |> Fmt.fmt "Tok_effect"
      | Tok_else -> formatter |> Fmt.fmt "Tok_else"
      | Tok_expose -> formatter |> Fmt.fmt "Tok_expose"
      | Tok_external -> formatter |> Fmt.fmt "Tok_external"
      | Tok_false -> formatter |> Fmt.fmt "Tok_false"
      | Tok_fun -> formatter |> Fmt.fmt "Tok_fun"
      | Tok_function -> formatter |> Fmt.fmt "Tok_function"
      | Tok_if -> formatter |> Fmt.fmt "Tok_if"
      | Tok_import -> formatter |> Fmt.fmt "Tok_import"
      | Tok_include -> formatter |> Fmt.fmt "Tok_include"
      | Tok_lazy -> formatter |> Fmt.fmt "Tok_lazy"
      | Tok_let -> formatter |> Fmt.fmt "Tok_let"
      | Tok_match -> formatter |> Fmt.fmt "Tok_match"
      | Tok_mutability -> formatter |> Fmt.fmt "Tok_mutability"
      | Tok_of -> formatter |> Fmt.fmt "Tok_of"
      | Tok_open -> formatter |> Fmt.fmt "Tok_open"
      | Tok_or -> formatter |> Fmt.fmt "Tok_or"
      | Tok_rec -> formatter |> Fmt.fmt "Tok_rec"
      | Tok_then -> formatter |> Fmt.fmt "Tok_then"
      | Tok_true -> formatter |> Fmt.fmt "Tok_true"
      | Tok_type -> formatter |> Fmt.fmt "Tok_type"
      | Tok_val -> formatter |> Fmt.fmt "Tok_val"
      | Tok_when -> formatter |> Fmt.fmt "Tok_when"
      | Tok_with -> formatter |> Fmt.fmt "Tok_with"

      (* Operators. *)
      | Tok_tilde_op op -> formatter |> Fmt.fmt "Tok_tilde_op=" |> String.pp op
      | Tok_qmark_op op -> formatter |> Fmt.fmt "Tok_qmark_op=" |> String.pp op
      | Tok_star_star_op op -> formatter |> Fmt.fmt "Tok_star_star_op=" |> String.pp op
      | Tok_star_op op -> formatter |> Fmt.fmt "Tok_star_op=" |> String.pp op
      | Tok_slash_op op -> formatter |> Fmt.fmt "Tok_slash_op=" |> String.pp op
      | Tok_pct_op op -> formatter |> Fmt.fmt "Tok_pct_op=" |> String.pp op
      | Tok_plus_op op -> formatter |> Fmt.fmt "Tok_plus_op=" |> String.pp op
      | Tok_minus_op op -> formatter |> Fmt.fmt "Tok_minus_op=" |> String.pp op
      | Tok_at_op op -> formatter |> Fmt.fmt "Tok_at_op=" |> String.pp op
      | Tok_caret_op op -> formatter |> Fmt.fmt "Tok_caret_op=" |> String.pp op
      | Tok_dollar_op op -> formatter |> Fmt.fmt "Tok_dollar_op=" |> String.pp op
      | Tok_lt_op op -> formatter |> Fmt.fmt "Tok_lt_op=" |> String.pp op
      | Tok_eq_op op -> formatter |> Fmt.fmt "Tok_eq_op=" |> String.pp op
      | Tok_gt_op op -> formatter |> Fmt.fmt "Tok_gt_op=" |> String.pp op
      | Tok_bar_op op -> formatter |> Fmt.fmt "Tok_bar_op=" |> String.pp op
      | Tok_colon_op op -> formatter |> Fmt.fmt "Tok_colon_op=" |> String.pp op
      | Tok_dot_op op -> formatter |> Fmt.fmt "Tok_dot_op=" |> String.pp op

      (* Punctuation. *)
      | Tok_tilde -> formatter |> Fmt.fmt "Tok_tilde"
      | Tok_qmark -> formatter |> Fmt.fmt "Tok_qmark"
      | Tok_minus -> formatter |> Fmt.fmt "Tok_minus"
      | Tok_lt -> formatter |> Fmt.fmt "Tok_lt"
      | Tok_lt_eq -> formatter |> Fmt.fmt "Tok_lt_eq"
      | Tok_eq -> formatter |> Fmt.fmt "Tok_eq"
      | Tok_lt_gt -> formatter |> Fmt.fmt "Tok_lt_gt"
      | Tok_gt_eq -> formatter |> Fmt.fmt "Tok_gt_eq"
      | Tok_gt -> formatter |> Fmt.fmt "Tok_gt"
      | Tok_comma -> formatter |> Fmt.fmt "Tok_comma"
      | Tok_dot -> formatter |> Fmt.fmt "Tok_dot"
      | Tok_semi -> formatter |> Fmt.fmt "Tok_semi"
      | Tok_semi_semi -> formatter |> Fmt.fmt "Tok_semi_semi"
      | Tok_colon -> formatter |> Fmt.fmt "Tok_colon"
      | Tok_colon_colon -> formatter |> Fmt.fmt "Tok_colon_colon"
      | Tok_colon_eq -> formatter |> Fmt.fmt "Tok_colon_eq"
      | Tok_lparen -> formatter |> Fmt.fmt "Tok_lparen"
      | Tok_rparen -> formatter |> Fmt.fmt "Tok_rparen"
      | Tok_lbrack -> formatter |> Fmt.fmt "Tok_lbrack"
      | Tok_rbrack -> formatter |> Fmt.fmt "Tok_rbrack"
      | Tok_lcurly -> formatter |> Fmt.fmt "Tok_lcurly"
      | Tok_rcurly -> formatter |> Fmt.fmt "Tok_rcurly"
      | Tok_bar -> formatter |> Fmt.fmt "Tok_bar"
      | Tok_larray -> formatter |> Fmt.fmt "Tok_larray"
      | Tok_rarray -> formatter |> Fmt.fmt "Tok_rarray"
      | Tok_lmodule -> formatter |> Fmt.fmt "Tok_lmodule"
      | Tok_rmodule -> formatter |> Fmt.fmt "Tok_rmodule"
      | Tok_bslash -> formatter |> Fmt.fmt "Tok_bslash"
      | Tok_tick -> formatter |> Fmt.fmt "Tok_tick"
      | Tok_caret -> formatter |> Fmt.fmt "Tok_caret"
      | Tok_amp -> formatter |> Fmt.fmt "Tok_amp"
      | Tok_xmark -> formatter |> Fmt.fmt "Tok_xmark"
      | Tok_arrow -> formatter |> Fmt.fmt "Tok_arrow"
      | Tok_carrow -> formatter |> Fmt.fmt "Tok_carrow"
      | Tok_source_directive rendition ->
        formatter |> Fmt.fmt "Tok_source_directive=" |> (Rendition.pp pp_source_directive) rendition
        |> Fmt.fmt ")"
      | Tok_indent rendition -> formatter |> Rendition.pp_unit "Tok_indent" rendition
      | Tok_line_delim -> formatter |> Fmt.fmt "Tok_line_delim"
      | Tok_dedent rendition -> formatter |> Rendition.pp_unit "Tok_dedent" rendition
      | Tok_whitespace -> formatter |> Fmt.fmt "Tok_whitespace"
      | Tok_hash_comment -> formatter |> Fmt.fmt "Tok_hash_comment"
      | Tok_paren_comment rendition -> formatter |> Rendition.pp_unit "Tok_paren_comment" rendition
      | Tok_uscore -> formatter |> Fmt.fmt "Tok_uscore"
      | Tok_uident rendition ->
        formatter |> Fmt.fmt "Tok_uident=" |> (Rendition.pp String.pp) rendition
      | Tok_cident cident -> formatter |> Fmt.fmt "Tok_cident=" |> String.pp cident
      | Tok_codepoint rendition ->
        formatter |> Fmt.fmt "Tok_codepoint=" |> (Rendition.pp Codepoint.pp) rendition
      | Tok_istring_lditto -> formatter |> Fmt.fmt "Tok_istring_lditto"
      | Tok_isubstring rendition ->
        formatter |> Fmt.fmt "Tok_isubstring=" |> (Rendition.pp String.pp) rendition
      | Tok_istring_pct -> formatter |> Fmt.fmt "Tok_istring_pct"
      | Tok_istring_lparen_caret -> formatter |> Fmt.fmt "Tok_istring_lparen_caret"
      | Tok_istring_caret_rparen -> formatter |> Fmt.fmt "Tok_istring_caret_rparen"
      | Tok_istring_rditto -> formatter |> Fmt.fmt "Tok_istring_rditto"
      | Tok_rstring rendition ->
        formatter |> Fmt.fmt "Tok_rstring=" |> (Rendition.pp String.pp) rendition
      | Tok_bstring rendition ->
        formatter |> Fmt.fmt "Tok_bstring=" |> (Rendition.pp String.pp) rendition
      | Tok_r32 rendition ->
        formatter |> Fmt.fmt "Tok_r32="
        |> (Rendition.pp Real.(fmt ~alt:true ~base:Fmt.Hex ~precision:6L ~notation:Fmt.Normalized))
          rendition
      | Tok_r64 rendition ->
        formatter |> Fmt.fmt "Tok_r64="
        |> (Rendition.pp Real.(fmt ~alt:true ~base:Fmt.Hex ~precision:13L ~notation:Fmt.Normalized))
          rendition
      | Tok_u8 rendition ->
        formatter |> Fmt.fmt "Tok_u8=" |> (Rendition.pp U8.pp) rendition
      | Tok_i8 rendition ->
        formatter |> Fmt.fmt "Tok_i8=" |> (Rendition.pp I8.pp) rendition
      | Tok_u16 rendition ->
        formatter |> Fmt.fmt "Tok_u16=" |> (Rendition.pp U16.pp) rendition
      | Tok_i16 rendition ->
        formatter |> Fmt.fmt "Tok_i16=" |> (Rendition.pp I16.pp) rendition
      | Tok_u32 rendition ->
        formatter |> Fmt.fmt "Tok_u32=" |> (Rendition.pp U32.pp) rendition
      | Tok_i32 rendition ->
        formatter |> Fmt.fmt "Tok_i32=" |> (Rendition.pp I32.pp) rendition
      | Tok_u64 rendition ->
        formatter |> Fmt.fmt "Tok_u64=" |> (Rendition.pp U64.pp) rendition
      | Tok_i64 rendition ->
        formatter |> Fmt.fmt "Tok_i64=" |> (Rendition.pp I64.pp) rendition
      | Tok_u128 rendition ->
        formatter |> Fmt.fmt "Tok_u128=" |> (Rendition.pp U128.pp) rendition
      | Tok_i128 rendition ->
        formatter |> Fmt.fmt "Tok_i128=" |> (Rendition.pp I128.pp) rendition
      | Tok_u256 rendition ->
        formatter |> Fmt.fmt "Tok_u256=" |> (Rendition.pp U256.pp) rendition
      | Tok_i256 rendition ->
        formatter |> Fmt.fmt "Tok_i256=" |> (Rendition.pp I256.pp) rendition
      | Tok_u512 rendition ->
        formatter |> Fmt.fmt "Tok_u512=" |> (Rendition.pp U512.pp) rendition
      | Tok_i512 rendition ->
        formatter |> Fmt.fmt "Tok_i512=" |> (Rendition.pp I512.pp) rendition
      | Tok_end_of_input -> formatter |> Fmt.fmt "Tok_end_of_input"
      | Tok_misaligned -> formatter |> Fmt.fmt "Tok_misaligned"
      | Tok_error -> formatter |> Fmt.fmt "Tok_error"
    )
    |> Fmt.fmt ">"

  let keyword_map = Map.of_alist (module String) [
    ("and", Tok_and);
    ("also", Tok_also);
    ("as", Tok_as);
    ("assert", Tok_assert);
    ("conceal", Tok_conceal);
    ("effect", Tok_effect);
    ("else", Tok_else);
    ("expose", Tok_expose);
    ("external", Tok_external);
    ("false", Tok_false);
    ("fun", Tok_fun);
    ("function", Tok_function);
    ("if", Tok_if);
    ("import", Tok_import);
    ("include", Tok_include);
    ("lazy", Tok_lazy);
    ("let", Tok_let);
    ("match", Tok_match);
    ("mutability", Tok_mutability);
    ("of", Tok_of);
    ("open", Tok_open);
    ("or", Tok_or);
    ("rec", Tok_rec);
    ("then", Tok_then);
    ("true", Tok_true);
    ("type", Tok_type);
    ("val", Tok_val);
    ("when", Tok_when);
    ("with", Tok_with);
  ]

  let of_uident_str uident_str =
    match Map.get uident_str keyword_map with
    | Some t -> t
    | None -> Tok_uident (Constant uident_str)
end

module ConcreteToken = struct
  type t = {
    atoken: AbstractToken.t;
    source: Source.t;
  }

  let init atoken source =
    {atoken; source}

  let atoken t =
    t.atoken

  let source t =
    t.source

  let pp t formatter =
    formatter
    |> Fmt.fmt "{atoken=" |> AbstractToken.pp t.atoken
    |> Fmt.fmt "; source=" |> Source.pp_loc t.source
    |> Fmt.fmt "}"
end

type line_state =
  | Line_dentation
  | Line_delim
  | Line_body

let pp_line_state line_state formatter =
  formatter
  |> Fmt.fmt (match line_state with
    | Line_dentation -> "Line_dentation"
    | Line_delim -> "Line_delim"
    | Line_body -> "Line_body"
  )

(* Istring_state determines what starting state to feed to Dag.start. States may be skipped, e.g. if
 * justification is not specified, but ordering through the spec/expr states is strict. Upon
 * completing specifier scanning the state returns to interp. *)
type istring_state =
  | Istring_interp
  | Istring_spec_pct
  (* XXX | Istring_spec_pad_seen *)
  (* XXX | Istring_spec_just_seen *)
  (* XXX | Istring_spec_sign_seen *)
  (* XXX | Istring_spec_alt_seen *)
  (* XXX | Istring_spec_zpad_seen *)
  | Istring_expr_width
  (* XXX | Istring_spec_width_seen *)
  (* XXX | Istring_expr_precision *)
  (* XXX | Istring_spec_precision_seen *)
  (* XXX | Istring_spec_base_seen *)
  (* XXX | Istring_spec_notation_seen *)
  (* XXX | Istring_spec_pretty_seen *)
  (* XXX | Istring_expr_fmt *)
  (* XXX | Istring_spec_fmt_seen *)
  (* XXX | Istring_spec_sep_seen *)
  | Istring_expr_value of Text.Cursor.t (* Cursor is start of value expression to be stringified. *)
  | Istring_rditto

let pp_istring_state istring_state formatter =
  match istring_state with
  | Istring_interp -> formatter |> Fmt.fmt "Istring_interp"
  | Istring_spec_pct -> formatter |> Fmt.fmt "Istring_spec_pct"
  | Istring_expr_width -> formatter |> Fmt.fmt "Istring_expr_width"
  | Istring_expr_value cursor -> formatter |> Fmt.fmt "Istring_exp_value " |> Text.Cursor.pp cursor
  | Istring_rditto -> formatter |> Fmt.fmt "Istring_rditto"

type t = {
  path: string option;
  line_bias: sint;
  col_bias: sint;
  tok_base: Text.Cursor.t;
  line_state: line_state;
  istring_state: istring_state list;
  level: uns;
}

let init text =
  {
    path=Text.path text;
    line_bias=Sint.zero;
    col_bias=Sint.zero;
    tok_base=Text.Cursor.hd text;
    line_state=Line_dentation;
    istring_state=[];
    level=0L;
  }

let pp t formatter =
  formatter
  |> Fmt.fmt "{path=" |> (Option.pp String.pp) t.path
  |> Fmt.fmt "; line_bias=" |> Sint.pp t.line_bias
  |> Fmt.fmt "; col_bias=" |> Sint.pp t.col_bias
  |> Fmt.fmt "; tok_base=" |> Text.Pos.pp (Text.Cursor.pos t.tok_base)
  |> Fmt.fmt "; line_state=" |> pp_line_state t.line_state
  |> Fmt.fmt "; istring_state=" |> (List.pp pp_istring_state) t.istring_state
  |> Fmt.fmt "; level=" |> Uns.pp t.level
  |> Fmt.fmt "}"

let source_at cursor t =
  Source.init ~path:t.path ~line_bias:t.line_bias ~col_bias:t.col_bias ~base:t.tok_base ~past:cursor

let text t =
  Text.Cursor.container t.tok_base

let set_of_cps cps =
  String.fold ~init:(Set.empty (module Codepoint)) ~f:(fun set cp ->
    Set.insert cp set
  ) cps

let map_of_cps_alist alist =
  List.fold ~init:(Map.empty (module Codepoint)) ~f:(fun edges (cps, v) ->
    String.fold ~init:edges ~f:(fun edges cp ->
      Map.insert_hlt ~k:cp ~v edges
    ) cps
  ) alist

let str_of_cursor cursor t =
  let slice = Text.Slice.init ~base:t.tok_base ~past:cursor (Text.Cursor.container cursor) in
  Text.Slice.to_string slice

let accept atoken cursor t =
  let source = source_at cursor t in
  {t with tok_base=cursor}, (ConcreteToken.init atoken source)

let accept_istring_push push tok _ppcursor _pcursor cursor t =
  let source = source_at cursor t in
  {t with tok_base=cursor; istring_state=push :: t.istring_state}, (ConcreteToken.init tok source)

let accept_istring_trans trans tok _ppcursor _pcursor cursor t =
  let source = source_at cursor t in
  {t with tok_base=cursor; istring_state=trans :: (List.tl t.istring_state)},
  (ConcreteToken.init tok source)

(* XXX Remove.
   let accept_istring_pop tok _ppcursor _pcursor cursor t =
   let source = source_at cursor t in
   {t with cursor; istring_state=List.tl t.istring_state},
   (ConcreteToken.init tok source)
*)

let accept_line_delim atoken cursor t =
  let source = source_at cursor t in
  {t with tok_base=cursor; line_state=Line_delim}, (ConcreteToken.init atoken source)

(***************************************************************************************************
 * Convenience routines for reporting malformations. *)

let malformation ~base ~past description t =
  AbstractToken.Rendition.Malformation.init ~path:t.path ~line_bias:t.line_bias ~col_bias:t.col_bias
    ~base ~past ~description

let malformed malformation =
  AbstractToken.Rendition.of_mals [malformation]

let unexpected_codepoint_source_directive base past t =
  malformation ~base ~past "Unexpected codepoint in source directive" t

let unterminated_source_directive base past t =
  malformation ~base ~past "Unterminated source directive" t

let invalid_unicode base past t =
  malformation ~base ~past "Invalid Unicode value" t

let illegal_backslash base past t =
  malformation ~base ~past "Illegal backslash escape" t

let missing_backslash base past t =
  malformation ~base ~past "Missing backslash escape" t

let partial_unicode base past t =
  malformation ~base ~past "Partial \\u{...}" t

let empty_codepoint base past t =
  malformation ~base ~past "Empty codepoint literal" t

let excess_codepoint base past t =
  malformation ~base ~past "Excess codepoint before terminator" t

let unterminated_comment base past t =
  malformation ~base ~past "Unterminated comment" t

let unterminated_codepoint base past t =
  malformation ~base ~past "Unterminated codepoint literal" t

let unterminated_string base past t =
  malformation ~base ~past "Unterminated string literal" t

let invalid_utf8 base past t =
  malformation ~base ~past "Invalid UTF-8 encoding" t

let invalid_bar_indent base past t =
  malformation ~base ~past "Invalid bar string indentation" t

let invalid_dec base past t =
  malformation ~base ~past "Invalid decimal digit" t

let invalid_hex base past t =
  malformation ~base ~past "Invalid hexadecimal digit" t

let invalid_numerical base past t =
  malformation ~base ~past "Invalid codepoint in numerical constant" t

let invalid_type_suffix_leading_zero base past t =
  malformation ~base ~past "Leading zero in numerical constant type suffix" t

let unsupported_bitwidth base past t =
  malformation ~base ~past "Unsupported bitwidth in numerical constant" t

let out_of_range_int radix limit base past t =
  let description =
    String.Fmt.empty
    |> Fmt.fmt "Numerical constant exceeds "
    |> (
      let base = match radix with
        | Radix.Bin -> Fmt.Bin
        | Radix.Oct -> Fmt.Oct
        | Radix.Dec -> Fmt.Dec
        | Radix.Hex -> Fmt.Hex
      in
      Nat.fmt ~alt:true ~base
    ) limit
    |> Fmt.to_string
  in
  malformation ~base ~past description t

let out_of_range_real base past t =
  malformation ~base ~past "Numerical constant cannot be precisely represented" t

(**************************************************************************************************)

let accept_dentation atoken cursor t =
  accept atoken cursor {t with line_state=Line_body}

let end_of_input cursor t =
  match t.line_state, t.level with
  | Line_delim, 0L -> accept_dentation Tok_line_delim cursor t
  | _, 0L -> accept Tok_end_of_input cursor t
  | _ -> accept_dentation (Tok_dedent (Constant ())) cursor {t with level=Uns.pred t.level}

let hash_comment _ppcursor _pcursor cursor t =
  let accept_hash_comment cursor t = begin
    accept_line_delim Tok_hash_comment cursor t
  end in
  let rec fn cursor t = begin
    match Text.Cursor.next_opt cursor with
    | None -> accept_hash_comment cursor t
    | Some (cp, cursor') -> begin
        match cp with
        | cp when Codepoint.(cp = nl) -> accept_hash_comment cursor' t
        | _ -> fn cursor' t
      end
  end in
  fn cursor t

let paren_comment _ppcursor _pcursor cursor t =
  let accept_paren_comment cursor t = begin
    let open AbstractToken.Rendition in
    accept (Tok_paren_comment (Constant ())) cursor t
  end in

  let rec fn_wrapper ~f nesting cursor t = begin
    match Text.Cursor.next_opt cursor with
    | None ->
      accept (Tok_paren_comment (malformed (unterminated_comment t.tok_base cursor t))) cursor t
    | Some (cp, cursor') -> f cp nesting cursor' t
  end
  and fn nesting cursor t = begin
    fn_wrapper ~f:(fun cp nesting cursor t ->
      match cp with
      | cp when Codepoint.(cp = of_char '*') -> fn_star nesting cursor t
      | cp when Codepoint.(cp = of_char '(') -> fn_lparen nesting cursor t
      | _ -> fn nesting cursor t
    ) nesting cursor t
  end
  and fn_star nesting cursor t = begin
    fn_wrapper ~f:(fun cp nesting cursor t ->
      match cp with
      | cp when Codepoint.(cp = of_char '*') -> fn_star nesting cursor t
      | cp when Codepoint.(cp = of_char '(') -> fn_lparen nesting cursor t
      | cp when Codepoint.(cp = of_char ')') -> begin
          match nesting with
          | 1L -> accept_paren_comment cursor t
          | _ -> fn (Uns.pred nesting) cursor t
        end
      | _ -> fn nesting cursor t
    ) nesting cursor t
  end
  and fn_lparen nesting cursor t = begin
    fn_wrapper ~f:(fun cp nesting cursor t ->
      match cp with
      | cp when Codepoint.(cp = of_char '*') -> fn (Uns.succ nesting) cursor t
      | cp when Codepoint.(cp = of_char '(') -> fn_lparen nesting cursor t
      | _ -> fn nesting cursor t
    ) nesting cursor t
  end in
  fn 1L cursor t

let operator_cps = "-+*/%@^$<=>|:.~?"
let operator_set = set_of_cps operator_cps

let operator_map = (
  let open AbstractToken in
  Map.of_alist (module String) [
    ("|", Tok_bar);
    (":", Tok_colon);
    ("::", Tok_colon_colon);
    (":=", Tok_colon_eq);
    (".", Tok_dot);
    ("-", Tok_minus);
    ("^", Tok_caret);
    ("<", Tok_lt);
    ("<=", Tok_lt_eq);
    ("=", Tok_eq);
    ("<>", Tok_lt_gt);
    (">=", Tok_gt_eq);
    (">", Tok_gt);
    ("->", Tok_arrow);
    ("~->", Tok_carrow);
  ])

let operator fop _ppcursor _pcursor cursor t =
  let accept_operator fop cursor t = begin
    let op = str_of_cursor cursor t in
    match Map.get op operator_map with
    | Some tok -> accept tok cursor t
    | None -> accept (fop op) cursor t
  end in
  let rec fn fop cursor t = begin
    match Text.Cursor.next_opt cursor with
    | None -> accept_operator fop cursor t
    | Some (cp, cursor') -> begin
        match Set.mem cp operator_set with
        | true -> fn fop cursor' t
        | false -> accept_operator fop cursor t
      end
  end in
  fn fop cursor t

let ident_set = set_of_cps
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'"

let ident ~f_accept cursor t =
  let rec fn cursor t = begin
    match Text.Cursor.next_opt cursor with
    | None -> f_accept cursor t
    | Some (cp, cursor') -> begin
        match Set.mem cp ident_set with
        | true -> fn cursor' t
        | false -> f_accept cursor t
      end
  end in
  fn cursor t

let accept_uident cursor t =
  let uident_str = str_of_cursor cursor t in
  accept (AbstractToken.of_uident_str uident_str) cursor t

let uident _ppcursor _pcursor cursor t =
  ident ~f_accept:accept_uident cursor t

let malformed_uident _ppcursor _pcursor cursor t =
  ident ~f_accept:(fun cursor t ->
    let ident_str = str_of_cursor cursor t in
    let mal = (malformed (malformation ~base:t.tok_base ~past:cursor
        (Format.asprintf "@[<h>Identifier %s lacks _*[A-Za-z] prefix" ident_str) t)) in
    accept (Tok_uident mal) cursor t
  ) cursor t

let cident _ppcursor _pcursor cursor t =
  ident ~f_accept:(fun cursor t ->
    let cident_str = str_of_cursor cursor t in
    accept (Tok_cident cident_str) cursor t
  ) cursor t

let uscore_ident_map = map_of_cps_alist [
  ("ABCDEFGHIJKLMNOPQRSTUVWXYZ", cident);
  ("abcdefghijklmnopqrstuvwxyz", uident);
  ("0123456789'", malformed_uident);
]

let uscore_ident _ppcursor pcursor cursor t =
  let rec fn cursor t = begin
    match Text.Cursor.next_opt cursor with
    | None -> accept_uident cursor t
    | Some (cp, cursor') -> begin
        match Map.get cp uscore_ident_map with
        | Some ident -> ident pcursor cursor cursor' t
        | None -> begin
            match cp with
            | cp when Codepoint.(cp = of_char '_') -> fn cursor' t
            | _ -> accept_uident cursor t
          end
      end
  end in
  fn cursor t

let accum_cp_of_nat ~accum_cp ~accum_mal nat accum base past t =
  Option.value_map (Nat.to_uns_opt nat)
    ~f:(fun u -> Codepoint.narrow_of_uns_opt u)
    ~default:None
  |> Option.value_map
    ~f:(fun cp -> accum_cp cp accum)
    ~default:(accum_mal (invalid_unicode base past t) accum)

module Codepoint_ : sig
  val codepoint: Text.Cursor.t -> Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
end = struct
  type umap =
    | UMapUscore
    | UMapDigit
    | UMapRcurly
    | UMapDitto
    | UMapTick

  let u_map = map_of_cps_alist [
    ("_", UMapUscore);
    ("0123456789abcdef", UMapDigit);
    ("}", UMapRcurly);
    ("\"", UMapDitto);
    ("'", UMapTick);
  ]

  type bmap =
    | BMapU
    | BMapT
    | BMapN
    | BMapR
    | BMapTick
    | BMapBslash
    | BMapNewline

  let bslash_map = map_of_cps_alist [
    ("u", BMapU);
    ("t", BMapT);
    ("n", BMapN);
    ("r", BMapR);
    ("'", BMapTick);
    ("\\", BMapBslash);
    ("\n", BMapNewline);
  ]

  type lmap =
    | LMapLookahead
    | LMapTick
    | LMapBslash

  let lookahead_map = map_of_cps_alist [
    ("abcdefghijklmnopqrstuvwxyz_ \n", LMapLookahead);
    ("'", LMapTick);
    ("\\", LMapBslash);
  ]

  type accum =
    | Empty
    | Cp of codepoint
    | Malformations of AbstractToken.Rendition.Malformation.t list

  let accum_cp cp = function
    | Empty -> Cp cp
    | Cp _ -> not_reached ()
    | Malformations _ as accum -> accum

  let accum_mal mal = function
    | Empty
    | Cp _ -> Malformations [mal]
    | Malformations mals -> Malformations (mal :: mals)

  (* Codepoint: '...' *)
  let codepoint _ppcursor _pcursor cursor t =
    let accept_codepoint accum cursor t = begin
      match accum with
      | Empty -> not_reached ()
      | Cp cp -> accept (Tok_codepoint (Constant cp)) cursor t
      | Malformations mals -> accept (Tok_codepoint (AbstractToken.Rendition.of_mals mals)) cursor t
    end in

    (* The callers of fn_wrapper have varying scanner state they're carrying as call parameters, so
     * in most cases they have to allocate a closure. This isn't ideal performance-wise, but it
     * reduces boilerplate. *)
    let fn_wrapper ~f accum cursor t = begin
      match Text.Cursor.next_opt cursor with
      | None -> begin
          let mal = unterminated_codepoint t.tok_base cursor t in
          accept_codepoint (accum_mal mal accum) cursor t
        end
      | Some (cp, cursor') -> f cp cursor' accum cursor t
    end in
    let rec fn_cp accum cursor t = begin
      fn_wrapper ~f:(fun cp cursor' accum cursor t ->
        match cp with
        | cp when Codepoint.(cp = of_char '\'') ->
          accept_codepoint accum cursor' t
        | _ -> begin
            let mal = excess_codepoint cursor cursor' t in
            fn_cp (accum_mal mal accum) cursor' t
          end
      ) accum cursor t
    end in
    let fn_lookahead accum pcursor cursor t = begin
      match Text.Cursor.next_opt cursor with
      | Some (cp, cursor') when Codepoint.(cp = of_char '\'') -> accept_codepoint accum cursor' t
      | Some (_, _)
      | None -> accept Tok_tick pcursor t
    end in
    let rec fn_bslash_u_lcurly nat bslash_cursor accum cursor t = begin
      fn_wrapper ~f:(fun cp cursor' accum cursor t ->
        match Map.get cp u_map with
        | Some UMapUscore -> fn_bslash_u_lcurly nat bslash_cursor accum cursor' t
        | Some UMapDigit -> begin
            fn_bslash_u_lcurly Radix.(nat_accum (nat_of_cp cp) nat Hex) bslash_cursor accum cursor'
              t
          end
        | Some UMapRcurly -> begin
            let accum' = accum_cp_of_nat ~accum_cp ~accum_mal nat accum bslash_cursor cursor' t in
            fn_cp accum' cursor' t
          end
        | Some UMapTick -> begin
            let mal = malformed (partial_unicode bslash_cursor cursor t) in
            accept (Tok_codepoint mal) cursor' t
          end
        | Some UMapDitto
        | None -> begin
            let mal = invalid_hex cursor cursor' t in
            let accum' = accum_mal mal accum in
            fn_bslash_u_lcurly nat bslash_cursor accum' cursor' t
          end
      ) accum cursor t
    end in
    let fn_bslash_u bslash_cursor accum cursor t = begin
      fn_wrapper ~f:(fun cp cursor' accum cursor t ->
        match cp with
        | cp when Codepoint.(cp = of_char '{') ->
          fn_bslash_u_lcurly Nat.zero bslash_cursor accum cursor' t
        | cp when Codepoint.(cp = of_char '\'') -> begin
            let mal = malformed (illegal_backslash bslash_cursor cursor t) in
            accept (Tok_codepoint mal) cursor' t
          end
        | _ -> begin
            let mal = illegal_backslash bslash_cursor cursor t in
            let accum = Malformations [mal] in
            fn_cp accum cursor t
          end
      ) accum cursor t
    end in
    let fn_bslash bslash_cursor accum cursor t = begin
      fn_wrapper ~f:(fun cp cursor' accum _cursor t ->
        match Map.get cp bslash_map with
        | Some BMapU -> fn_bslash_u bslash_cursor accum cursor' t
        | Some BMapT -> fn_cp (Cp Codepoint.ht) cursor' t
        | Some BMapN -> fn_cp (Cp Codepoint.nl) cursor' t
        | Some BMapR -> fn_cp (Cp Codepoint.cr) cursor' t
        | Some BMapTick
        | Some BMapBslash -> fn_cp (Cp cp) cursor' t
        | Some BMapNewline -> accept Tok_tick cursor t
        | None -> begin
            let mal = illegal_backslash bslash_cursor cursor' t in
            fn_cp (accum_mal mal accum) cursor' t
          end
      ) accum cursor t
    end in
    let accum = Empty in
    match Text.Cursor.nextv_opt cursor with
    | None -> accept Tok_tick cursor t
    | Some (_, false, cursor') -> begin
        let mal = invalid_utf8 cursor cursor' t in
        fn_cp (accum_mal mal accum) cursor' t
      end
    | Some (cp, true, cursor') -> begin
        match Map.get cp lookahead_map with
        | Some LMapLookahead -> fn_lookahead (Cp cp) cursor cursor' t
        | Some LMapTick -> begin
            let mal = empty_codepoint t.tok_base cursor' t in
            accept_codepoint (accum_mal mal accum) cursor' t
          end
        | Some LMapBslash -> fn_bslash cursor accum cursor' t
        | None -> fn_cp (Cp cp) cursor' t
      end
end

module String_ : sig
  val bstring: Text.Cursor.t -> Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
  val rstring: Text.Cursor.t -> Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
  val accept_unterminated_rstring: Text.Cursor.t -> t -> t * ConcreteToken.t
end = struct
  (* Interpolated substring: "..." *)
  type accum =
    | Codepoints of codepoint list
    | Malformations of AbstractToken.Rendition.Malformation.t list

  let accum_cp cp = function
    | Codepoints cps -> Codepoints (cp :: cps)
    | (Malformations _) as mals -> mals

  let accum_mal mal = function
    | Codepoints _ -> Malformations [mal]
    | Malformations mals -> Malformations (mal :: mals)

  type tag_accum =
    {
      cps: codepoint list;
      mals: AbstractToken.Rendition.Malformation.t list;
    }

  let tag_accum_empty = {cps=[]; mals=[]}

  let tag_accum_cp cp accum =
    {accum with cps=(cp :: accum.cps)}

  let tag_accum_mal mal accum =
    {cps=Codepoint.replacement :: accum.cps; mals=mal :: accum.mals}

  type tag =
    {
      tag: string;
      mals: AbstractToken.Rendition.Malformation.t list;
    }

  let tag_of_accum accum =
    {
      tag=String.of_list_rev accum.cps;
      mals=List.rev accum.mals;
    }

  let accept_unterminated_rstring cursor t =
    accept (Tok_rstring (malformed (unterminated_string t.tok_base cursor t))) cursor t

  (* Raw string: ``...`` *)
  let rstring _pcursor pcursor _cursor t =
    let accept_rstring rtag body_accum ltag cursor t = begin
      match ltag.mals, body_accum, rtag.mals with
      | [], Codepoints cps, [] -> begin
          (* Trim trailing '\n' if present before reversing cps. *)
          let cps_tl_trimmed = match cps with
            | cp :: cps' when Codepoint.(cp = nl) -> cps'
            | cps' -> cps'
          in
          (* Reverse cps and trim leading '\n' if present. *)
          let cps_trimmed = match List.rev cps_tl_trimmed with
            | cp :: cps' when Codepoint.(cp = nl) -> cps'
            | cps' -> cps'
          in
          accept (Tok_rstring (Constant (String.of_list cps_trimmed))) cursor t
        end
      | _, Codepoints _, _ -> begin
          let mals = List.(rev_concat ltag.mals (rev rtag.mals)) in
          accept (Tok_rstring (AbstractToken.Rendition.of_mals mals)) cursor t
        end
      | _, Malformations body_mals, _ -> begin
          let mals = List.(rev_concat ltag.mals (rev_concat body_mals (rev rtag.mals))) in
          accept (Tok_rstring (AbstractToken.Rendition.of_mals mals)) cursor t
        end
    end in

    let rec fn_rtag rtag_accum ltag_cursor body_accum saved_body_accum ltag cursor t = begin
      match Text.Cursor.nextv_opt cursor with
      | None -> begin
          let mal = unterminated_string t.tok_base cursor t in
          let rtag = tag_of_accum (tag_accum_mal mal rtag_accum) in
          accept_rstring rtag saved_body_accum ltag cursor t
        end
      | Some (_, false, cursor') -> begin
          let mal = invalid_utf8 cursor cursor' t in
          match String.C.Cursor.(ltag_cursor = (tl (string ltag_cursor))) with
          | true -> fn (accum_mal mal body_accum) ltag cursor' t
          | false -> begin
              let ltag_cp, ltag_cursor' = String.C.Cursor.next ltag_cursor in
              match Codepoint.(replacement = ltag_cp) with
              | true ->
                fn_rtag (tag_accum_cp Codepoint.replacement rtag_accum) ltag_cursor' (accum_mal mal
                    body_accum) saved_body_accum ltag cursor' t
              | false -> fn (accum_mal mal body_accum) ltag cursor' t
            end
        end
      | Some (cp, true, cursor') -> begin
          match cp, String.C.Cursor.(ltag_cursor = (tl (string ltag_cursor))) with
          | cp, true when Codepoint.(cp = of_char '`') ->
            accept_rstring (tag_of_accum rtag_accum) saved_body_accum ltag cursor' t
          | cp, false when Codepoint.(cp = of_char '`') ->
            fn_rtag tag_accum_empty (String.C.Cursor.hd ltag.tag) (accum_cp cp body_accum) body_accum
              ltag cursor' t
          | _, true -> fn (accum_cp cp body_accum) ltag cursor' t
          | _, false -> begin
              let ltag_cp, ltag_cursor' = String.C.Cursor.next ltag_cursor in
              match Codepoint.(cp = ltag_cp) with
              | true ->
                fn_rtag (tag_accum_cp cp rtag_accum) ltag_cursor' (accum_cp cp body_accum)
                  saved_body_accum ltag cursor' t
              | false -> fn (accum_cp cp body_accum) ltag cursor' t
            end
        end
    end
    and fn body_accum ltag cursor t = begin
      match Text.Cursor.nextv_opt cursor with
      | None -> begin
          let mal = unterminated_string t.tok_base cursor t in
          let body_accum' = accum_mal mal body_accum in
          let rtag = tag_of_accum tag_accum_empty in
          accept_rstring rtag body_accum' ltag cursor t
        end
      | Some (_, false, cursor') -> begin
          let mal = invalid_utf8 cursor cursor' t in
          fn (accum_mal mal body_accum) ltag cursor' t
        end
      | Some (cp, true, cursor') -> begin
          match cp with
          | cp when Codepoint.(cp = of_char '`') ->
            fn_rtag tag_accum_empty (String.C.Cursor.hd ltag.tag) (accum_cp cp body_accum)
              body_accum ltag cursor' t
          | _ -> fn (accum_cp cp body_accum) ltag cursor' t
        end
    end
    and fn_ltag ltag_accum cursor t = begin
      match Text.Cursor.nextv_opt cursor with
      | None -> begin
          let mal = unterminated_string t.tok_base cursor t in
          let ltag_accum' = tag_accum_mal mal ltag_accum in
          let ltag = tag_of_accum ltag_accum' in
          let body_accum = Codepoints [] in
          let rtag = tag_of_accum tag_accum_empty in
          accept_rstring rtag body_accum ltag cursor t
        end
      | Some (_, false, cursor') -> begin
          let mal = invalid_utf8 cursor cursor' t in
          fn_ltag (tag_accum_mal mal ltag_accum) cursor' t
        end
      | Some (cp, true, cursor') -> begin
          match cp with
          | cp when Codepoint.(cp = of_char '`') ->
            fn (Codepoints []) (tag_of_accum ltag_accum) cursor' t
          | _ -> fn_ltag (tag_accum_cp cp ltag_accum) cursor' t
        end
    end in
    fn_ltag tag_accum_empty pcursor t

  (* Raw bar string:
   *   `|...
   *   ` *)
  let bstring _ppcursor _pcursor cursor t =
    let accept_bstring accum cursor t = begin
      match accum with
      | Codepoints (_ :: cps) ->
        accept (Tok_bstring (Constant (String.of_list_rev cps))) cursor t
      | Codepoints [] -> not_reached () (* There's always a '\n'. *)
      | Malformations mals -> accept (Tok_bstring (AbstractToken.Rendition.of_mals mals)) cursor t
    end in

    let rec fn_lpad c0_cursor accum lmargin cursor t = begin
      match Text.Cursor.nextv_opt cursor with
      | None -> begin
          let mal = unterminated_string t.tok_base cursor t in
          accept_bstring (accum_mal mal accum) cursor t
        end
      | Some (_, false, cursor') -> begin
          let mal = invalid_utf8 cursor cursor' t in
          fn (accum_mal mal accum) lmargin cursor' t
        end
      | Some (cp, true, cursor') -> begin
          match cp with
          | cp when Codepoint.(cp = of_char ' ') -> fn_lpad c0_cursor accum lmargin cursor' t
          | cp when Codepoint.(cp = of_char '|') -> begin
              match Text.(Pos.col (Cursor.pos cursor')) = lmargin with
              | true -> fn accum lmargin cursor' t
              | false -> begin
                  let mal = invalid_bar_indent c0_cursor cursor' t in
                  fn (accum_mal mal accum) lmargin cursor' t
                end
            end
          | cp when Codepoint.(cp = of_char '`') -> accept_bstring accum cursor' t
          | cp when Codepoint.(cp = nl) -> begin
              let mal = invalid_bar_indent c0_cursor cursor t in
              fn_lpad cursor' (accum_mal mal accum) lmargin cursor' t
            end
          | _ -> begin
              let mal = invalid_bar_indent c0_cursor cursor t in
              fn (accum_mal mal accum) lmargin cursor' t
            end
        end
    end
    and fn accum lmargin cursor t = begin
      match Text.Cursor.nextv_opt cursor with
      | None -> begin
          let mal = unterminated_string t.tok_base cursor t in
          accept_bstring (accum_mal mal accum) cursor t
        end
      | Some (_, false, cursor') -> begin
          let mal = invalid_utf8 cursor cursor' t in
          fn (accum_mal mal accum) lmargin cursor' t
        end
      | Some (cp, true, cursor') -> begin
          let accum' = accum_cp cp accum in
          match cp with
          | cp when Codepoint.(cp = nl) -> fn_lpad cursor' accum' lmargin cursor' t
          | _ -> fn accum' lmargin cursor' t
        end
    end in
    let lmargin = Text.(Pos.col (Cursor.pos cursor)) in
    fn (Codepoints []) lmargin cursor t
end

type nsmap =
  | NSMapDigit
  | NSMapIdent

let num_suffix_map = map_of_cps_alist [
  ("0123456789", NSMapDigit);
  ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_'", NSMapIdent);
]

module Real : sig
  type outer = t
  type t
  val zero: AbstractToken.t
  val of_whole: Nat.t -> Radix.t -> t
  val of_mals: AbstractToken.Rendition.Malformation.t list -> t
  val r_suffix: t -> Text.Cursor.t -> Radix.t -> Text.Cursor.t -> outer -> outer * ConcreteToken.t
  val exp: t -> Text.Cursor.t -> Radix.t -> Text.Cursor.t -> outer -> outer * ConcreteToken.t
  val dot: t -> Text.Cursor.t -> Radix.t -> Text.Cursor.t -> outer -> outer * ConcreteToken.t
  val zero_r_suffix: Text.Cursor.t -> Text.Cursor.t -> Text.Cursor.t -> outer
    -> outer * ConcreteToken.t
  val zero_frac: Text.Cursor.t -> Text.Cursor.t -> Text.Cursor.t -> outer -> outer * ConcreteToken.t
  val zero_exp: Text.Cursor.t -> Text.Cursor.t -> Text.Cursor.t -> outer -> outer * ConcreteToken.t
end = struct
  type outer = t
  type exp_sign =
    | ExpNeg
    | ExpPos
  type subtype =
    | Subtype_r32
    | Subtype_r64
  type t =
    (* exp is always non-negative; exp_sign tracks the exponent's sign because the sign is scanned
     * before there is a non-zero magnitude in which to incorporate the sign. *)
    (* frac, point_shift, exp_sign, exp *)
    | R of Realer.t * sint * exp_sign * Zint.t
    | R_dec
    | Malformations of AbstractToken.Rendition.Malformation.t list

  let accum_exp_digit cp = function
    | R (frac, point_shift, exp_sign, exp) -> begin
        let exp' = Radix.zint_accum (zint_of_cp cp) exp Dec in
        R (frac, point_shift, exp_sign, exp')
      end
    | R_dec -> R_dec
    | Malformations _ as mals -> mals

  let accum_exp_sign exp_sign = function
    | R (frac, point_shift, _exp_sign, exp) -> R (frac, point_shift, exp_sign, exp)
    | R_dec -> R_dec
    | Malformations _ as mals -> mals

  let realer_of_nat nat =
    let sig_bits = (Nat.bit_length nat) - (Nat.bit_clz nat) in
    let exponent = match sig_bits with
      | 0L -> Zint.zero
      | _ -> Zint.of_uns (pred sig_bits)
    in
    Realer.create ~sign:Realer.Pos ~exponent ~mantissa:nat

  let accum_frac_digit cp radix = function
    | R (frac, point_shift, exp_sign, exp) -> begin
        let cp_nat = nat_of_cp cp in
        let open Radix in
        let bits_per_digit = match radix with
          | Bin -> Sint.kv 1L
          | Oct -> Sint.kv 3L
          | Dec -> not_reached ()
          | Hex -> Sint.kv 4L
        in
        let sig_bits = (Nat.bit_length cp_nat) - (Nat.bit_clz cp_nat) in
        let nonfrac_shift = Sint.(bits_per_digit - (pred (Uns.bits_to_sint sig_bits))) in
        let exponent = Zint.((of_sint point_shift) - (of_sint nonfrac_shift)) in
        let digit = Realer.create ~sign:Pos ~exponent ~mantissa:cp_nat in
        let frac' = Realer.(frac + digit) in
        let point_shift' = Sint.(point_shift - bits_per_digit) in
        R (frac', point_shift', exp_sign, exp)
      end
    | R_dec
    | Malformations _ as mals -> mals

  let accum_mal mal = function
    | R _ -> Malformations [mal]
    | R_dec -> Malformations [mal]
    | Malformations mals -> Malformations (mal :: mals)

  let accept subtype_opt suffix_cursor accum radix cursor t =
    let open AbstractToken in
    let accept_mals subtype mals cursor t = begin
      let malformed = Rendition.of_mals mals in
      let tok = match subtype with
        | Subtype_r32 -> Tok_r32 malformed
        | Subtype_r64 -> Tok_r64 malformed
      in
      accept tok cursor t
    end in
    let subtype = match subtype_opt with
      | Some subtype -> subtype
      | None -> Subtype_r64
    in
    let open Radix in
    match accum, radix with
    | R (frac, _point_shift, exp_sign, exp), Bin
    | R (frac, _point_shift, exp_sign, exp), Oct
    | R (frac, _point_shift, exp_sign, exp), Hex -> begin
        let exponent = Zint.(match exp_sign with
          | ExpNeg -> neg exp
          | ExpPos -> exp
        ) in
        let e = Realer.create ~sign:Realer.Pos ~exponent ~mantissa:Nat.one in
        let realer = Realer.(frac * e) in
        let tok = match subtype with
          | Subtype_r32 -> begin
              Tok_r32 (
                match Realer.to_r32_opt realer with
                | Some r -> (Constant r)
                | None -> malformed (out_of_range_real t.tok_base cursor t)
              )
            end
          | Subtype_r64 -> begin
              Tok_r64 (
                match Realer.to_r64_opt realer with
                | Some r -> (Constant r)
                | None -> malformed (out_of_range_real t.tok_base cursor t)
              )
            end
        in
        accept tok cursor t
      end
    | R _, Dec -> not_reached ()
    | R_dec, Dec -> begin
        let r = Real.of_string Text.Slice.(to_string (init ~base:t.tok_base
            ~past:suffix_cursor (Text.Cursor.container t.tok_base))) in
        let tok = match subtype with
          | Subtype_r32 -> Tok_r32 (Constant r)
          | Subtype_r64 -> Tok_r64 (Constant r)
        in
        accept tok cursor t
      end
    | R_dec, Bin
    | R_dec, Oct
    | R_dec, Hex -> not_reached ()
    | Malformations mals, _ -> accept_mals subtype mals cursor t

  let rec next_suffix bitwidth suffix_cursor accum radix cursor t =
    let accept_subtype bitwidth suffix_cursor accum radix cursor t = begin
      let subtype_opt = match Nat.to_uns_opt bitwidth with
        | Some bit_length -> begin
            match bit_length with
            | 32L -> Some Subtype_r32
            | 64L -> Some Subtype_r64
            | 0L -> begin
                let _cp, digits_cursor = Text.Cursor.next suffix_cursor in
                match Text.Cursor.(digits_cursor = cursor) with
                | true -> Some Subtype_r64 (* "r" suffix. *)
                | false -> None
              end
            | _ -> None
          end
        | None -> None
      in
      let accum' = match subtype_opt with
        | None -> begin
            let mal = unsupported_bitwidth suffix_cursor cursor t in
            accum_mal mal accum
          end
        | Some _ -> accum
      in
      accept subtype_opt suffix_cursor accum' radix cursor t
    end in
    match Text.Cursor.next_opt cursor with
    | None -> accept_subtype bitwidth suffix_cursor accum radix cursor t
    | Some (cp, cursor') -> begin
        match Map.get cp num_suffix_map with
        | Some NSMapDigit -> begin
            let digit = nat_of_cp cp in
            match Nat.(bitwidth = zero && digit = zero) with
            | true -> begin
                let mal = invalid_type_suffix_leading_zero cursor cursor' t in
                let accum' = accum_mal mal accum in
                next_suffix bitwidth suffix_cursor accum' radix cursor' t
              end
            | false -> begin
                let bitwidth' = Radix.(nat_accum digit bitwidth Dec) in
                next_suffix bitwidth' suffix_cursor accum radix cursor' t
              end
          end
        | Some NSMapIdent -> begin
            let mal = invalid_numerical cursor cursor' t in
            let accum' = accum_mal mal accum in
            next_suffix bitwidth suffix_cursor accum' radix cursor' t
          end
        | None -> accept_subtype bitwidth suffix_cursor accum radix cursor t
      end

  type exp_map =
    | EMapUscore
    | EMapDigit
    | EMapRealSuffix
    | EMapNeg
    | EMapPos
    | EMapMalIdent

  let dec_exp_map = map_of_cps_alist [
    ("_", EMapUscore);
    ("0123456789", EMapDigit);
    ("r", EMapRealSuffix);
    ("-", EMapNeg);
    ("+", EMapPos);
    ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqstuvwxyz'", EMapMalIdent);
  ]

  let hex_exp_map = map_of_cps_alist [
    ("_", EMapUscore);
    ("0123456789abcdef", EMapDigit);
    ("r", EMapRealSuffix);
    ("-", EMapNeg);
    ("+", EMapPos);
    ("ABCDEFGHIJKLMNOPQRSTUVWXYZghijklmnopqstuvwxyz'", EMapMalIdent);
  ]

  let rec next_exp exp_map accum exp_cursor radix cursor t =
    match Text.Cursor.next_opt cursor with
    | None -> accept None cursor accum radix cursor t
    | Some (cp, cursor') -> begin
        match Map.get cp exp_map with
        | Some EMapUscore -> next_exp exp_map accum exp_cursor radix cursor' t
        | Some EMapDigit -> begin
            let accum' = accum_exp_digit cp accum in
            next_exp exp_map accum' exp_cursor radix cursor' t
          end
        | Some EMapRealSuffix ->
          next_suffix Nat.k_0 cursor accum radix cursor' t
        | Some EMapNeg
        | Some EMapPos
        | Some EMapMalIdent -> begin
            let mal = invalid_numerical cursor cursor' t in
            let accum' = accum_mal mal accum in
            next_exp exp_map accum' exp_cursor radix cursor' t
          end
        | None -> accept None cursor accum radix cursor t
      end

  let rec first_exp accum exp_cursor radix cursor t =
    let open Radix in
    let exp_map = match radix with
      | Bin -> hex_exp_map
      | Oct -> hex_exp_map
      | Dec -> dec_exp_map
      | Hex -> hex_exp_map
    in
    match Text.Cursor.next_opt cursor with
    | None -> accept None cursor accum radix cursor t
    | Some (cp, cursor') -> begin
        match Map.get cp exp_map with
        | Some EMapUscore -> first_exp accum exp_cursor radix cursor' t
        | Some EMapDigit -> begin
            let accum' = accum_exp_digit cp accum in
            next_exp exp_map accum' exp_cursor radix cursor' t
          end
        | Some EMapRealSuffix ->
          next_suffix Nat.k_0 cursor accum radix cursor' t
        | Some EMapMalIdent -> begin
            let mal = invalid_numerical cursor cursor' t in
            let accum' = accum_mal mal accum in
            next_exp exp_map accum' exp_cursor radix cursor' t
          end
        | Some EMapNeg -> begin
            let accum' = accum_exp_sign ExpNeg accum in
            next_exp exp_map accum' exp_cursor radix cursor' t
          end
        | Some EMapPos -> begin
            let accum' = accum_exp_sign ExpPos accum in
            next_exp exp_map accum' exp_cursor radix cursor' t
          end
        | None -> accept None cursor accum radix cursor t
      end

  type frac_map =
    | FMapUscore
    | FMapDigit
    | FMapExp
    | FMapRealSuffix
    | FMapMalIdent

  let bin_frac_map = map_of_cps_alist [
    ("_", FMapUscore);
    ("01", FMapDigit);
    ("p", FMapExp);
    ("r", FMapRealSuffix);
    ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnoqstuvwxyz23456789'", FMapMalIdent);
  ]

  let oct_frac_map = map_of_cps_alist [
    ("_", FMapUscore);
    ("01234567", FMapDigit);
    ("p", FMapExp);
    ("r", FMapRealSuffix);
    ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnoqstuvwxyz89'", FMapMalIdent);
  ]

  let dec_frac_map = map_of_cps_alist [
    ("_", FMapUscore);
    ("0123456789", FMapDigit);
    ("e", FMapExp);
    ("r", FMapRealSuffix);
    ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdfghijklmnopqstuvwxyz'", FMapMalIdent);
  ]

  let hex_frac_map = map_of_cps_alist [
    ("_", FMapUscore);
    ("0123456789abcdef", FMapDigit);
    ("p", FMapExp);
    ("r", FMapRealSuffix);
    ("ABCDEFGHIJKLMNOPQRSTUVWXYZghijklmnoqstuvwxyz'", FMapMalIdent);
  ]

  let rec next_frac accum mantissa_cursor frac_map radix cursor t =
    match Text.Cursor.next_opt cursor with
    | None -> accept None cursor accum radix cursor t
    | Some (cp, cursor') -> begin
        match Map.get cp frac_map with
        | Some FMapUscore -> next_frac accum mantissa_cursor frac_map radix cursor' t
        | Some FMapDigit -> begin
            let accum' = accum_frac_digit cp radix accum in
            next_frac accum' mantissa_cursor frac_map radix cursor' t
          end
        | Some FMapExp -> first_exp accum cursor radix cursor' t
        | Some FMapRealSuffix -> next_suffix Nat.k_0 cursor accum radix cursor' t
        | Some FMapMalIdent -> begin
            let mal = invalid_numerical cursor cursor' t in
            let accum' = accum_mal mal accum in
            next_frac accum' mantissa_cursor frac_map radix cursor' t
          end
        | None -> accept None cursor accum radix cursor t
      end

  (* Entry point functions follow. *)

  let zero = AbstractToken.Tok_r64 (Constant 0.)

  let of_whole whole radix =
    let open Radix in
    match radix with
    | Bin | Oct | Hex -> R (realer_of_nat whole, Sint.kv 0L, ExpPos, Zint.zero)
    | Dec -> R_dec

  let of_mals mals =
    Malformations mals

  let r_suffix accum suffix_cursor radix cursor t =
    next_suffix Nat.k_0 suffix_cursor accum radix cursor t

  let exp accum exp_cursor radix cursor t =
    first_exp accum exp_cursor radix cursor t

  let dot accum mantissa_cursor radix cursor t =
    let open Radix in
    let frac_map = match radix with
      | Bin -> bin_frac_map
      | Oct -> oct_frac_map
      | Dec -> dec_frac_map
      | Hex -> hex_frac_map
    in
    next_frac accum mantissa_cursor frac_map radix cursor t

  let zero_r_suffix _ppcursor pcursor cursor t =
    r_suffix R_dec pcursor Dec cursor t

  let zero_frac _ppcursor pcursor _cursor t =
    next_frac R_dec t.tok_base dec_frac_map Dec pcursor t

  let zero_exp _ppcursor pcursor cursor t =
    first_exp R_dec pcursor Dec cursor t
end

module Integer : sig
  val zero: AbstractToken.t
  val bin: Nat.t -> Text.Cursor.t -> Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
  val oct: Nat.t -> Text.Cursor.t -> Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
  val dec: Nat.t -> Text.Cursor.t -> Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
  val hex: Nat.t -> Text.Cursor.t -> Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
  val mal_ident: Text.Cursor.t -> Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
  val zero_u_suffix: Text.Cursor.t -> Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
  val zero_i_suffix: Text.Cursor.t -> Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
end = struct
  type signedness =
    | Unsigned
    | Signed

  (* Full enumeration of integer types is a bit unwieldy, but it's more robust and doesn't actually
   * cause much code bloat. *)
  type subtype =
    | Subtype_u8
    | Subtype_i8
    | Subtype_u16
    | Subtype_i16
    | Subtype_u32
    | Subtype_i32
    | Subtype_u64
    | Subtype_i64
    | Subtype_u128
    | Subtype_i128
    | Subtype_u256
    | Subtype_i256
    | Subtype_u512
    | Subtype_i512

  type accum =
    | N of Nat.t
    | Malformations of AbstractToken.Rendition.Malformation.t list

  let accum_whole_digit cp radix = function
    | N n -> N (Radix.nat_accum (nat_of_cp cp) n radix)
    | Malformations _ as mals -> mals

  let accum_mal mal = function
    | N _ -> Malformations [mal]
    | Malformations mals -> Malformations (mal :: mals)

  (* Prefix signs are scanned as separate tokens, and there is no way to distinguish them from infix
   * operators until parsing is complete. Therefore the scanner accepts min_value (e.g. 0x80i8)
   * regardless of whether it's negative. Thanks to 2s complement representation, both 0x80i8 and
   * -0x80i8 encode min_value, and no correctness issues arise from allowing 0x80i8.
   *
   * It would be possible to disallow this edge case during an AST optimization pass which combines
   * constants and their prefix signs; if the compiler does so, it should take care to emit error
   * messages formatted the same as those emitted by the scanner. Although it would be possible to
   * push the limit checking into the post-parsing optimization, doing so would make the
   * optimization mandatory, as well as making it more likely for the programmer to not see such
   * errors unless there are no parse errors which prevent code generation. *)
  let limit_of_subtype subtype =
    let open Nat in
    match subtype with
    | Subtype_u8 -> max_u8
    | Subtype_i8 -> max_abs_i8
    | Subtype_u16 -> max_u16
    | Subtype_i16 -> max_abs_i16
    | Subtype_u32 -> max_u32
    | Subtype_i32 -> max_abs_i32
    | Subtype_u64 -> max_u64
    | Subtype_i64 -> max_abs_i64
    | Subtype_u128 -> max_u128
    | Subtype_i128 -> max_abs_i128
    | Subtype_u256 -> max_u256
    | Subtype_i256 -> max_abs_i256
    | Subtype_u512 -> max_u512
    | Subtype_i512 -> max_abs_i512

  let accept subtype_opt accum radix cursor t =
    let open AbstractToken in
    let accept_mals subtype mals cursor t = begin
      let malformed = Rendition.of_mals mals in
      let tok = match subtype with
        | Subtype_u8 -> Tok_u8 malformed
        | Subtype_i8 -> Tok_i8 malformed
        | Subtype_u16 -> Tok_u16 malformed
        | Subtype_i16 -> Tok_i16 malformed
        | Subtype_u32 -> Tok_u32 malformed
        | Subtype_i32 -> Tok_i32 malformed
        | Subtype_u64 -> Tok_u64 malformed
        | Subtype_i64 -> Tok_i64 malformed
        | Subtype_u128 -> Tok_u128 malformed
        | Subtype_i128 -> Tok_i128 malformed
        | Subtype_u256 -> Tok_u256 malformed
        | Subtype_i256 -> Tok_i256 malformed
        | Subtype_u512 -> Tok_u512 malformed
        | Subtype_i512 -> Tok_i512 malformed
      in
      accept tok cursor t
    end in
    let subtype = match subtype_opt with
      | Some subtype -> subtype
      | None -> Subtype_u64
    in
    match accum with
    | N n -> begin
        let limit = limit_of_subtype subtype in
        match Nat.(n <= limit) with
        | false -> begin
            let mal = out_of_range_int radix limit t.tok_base cursor t in
            match (accum_mal mal accum) with
            | Malformations mals -> accept_mals subtype mals cursor t
            | N _ -> not_reached ()
          end
        | true -> begin
            let tok = match subtype with
              | Subtype_u8 -> Tok_u8 (Constant (Nat.to_u8_hlt n))
              | Subtype_i8 -> Tok_i8 (Constant (Nat.to_i8_hlt n))
              | Subtype_u16 -> Tok_u16 (Constant (Nat.to_u16_hlt n))
              | Subtype_i16 -> Tok_i16 (Constant (Nat.to_i16_hlt n))
              | Subtype_u32 -> Tok_u32 (Constant (Nat.to_u32_hlt n))
              | Subtype_i32 -> Tok_i32 (Constant (Nat.to_i32_hlt n))
              | Subtype_u64 -> Tok_u64 (Constant (Nat.to_u64_hlt n))
              | Subtype_i64 -> Tok_i64 (Constant (Nat.to_i64_hlt n))
              | Subtype_u128 -> Tok_u128 (Constant (Nat.to_u128_hlt n))
              | Subtype_i128 -> Tok_i128 (Constant (Nat.to_i128_hlt n))
              | Subtype_u256 -> Tok_u256 (Constant (Nat.to_u256_hlt n))
              | Subtype_i256 -> Tok_i256 (Constant (Nat.to_i256_hlt n))
              | Subtype_u512 -> Tok_u512 (Constant (Nat.to_u512_hlt n))
              | Subtype_i512 -> Tok_i512 (Constant (Nat.to_i512_hlt n))
            in
            accept tok cursor t
          end
      end
    | Malformations mals -> accept_mals subtype mals cursor t

  let rec next_suffix bitwidth signedness suffix_cursor accum radix cursor t =
    let accept_subtype bitwidth signedness suffix_cursor accum radix cursor t = begin
      let subtype_opt = match Nat.to_uns_opt bitwidth with
        | Some bit_length -> begin
            match signedness, bit_length with
            | Unsigned, 8L -> Some Subtype_u8
            | Signed, 8L -> Some Subtype_i8
            | Unsigned, 16L -> Some Subtype_u16
            | Signed, 16L -> Some Subtype_i16
            | Unsigned, 32L -> Some Subtype_u32
            | Signed, 32L -> Some Subtype_i32
            | Unsigned, 64L -> Some Subtype_u64
            | Signed, 64L -> Some Subtype_i64
            | Unsigned, 128L -> Some Subtype_u128
            | Signed, 128L -> Some Subtype_i128
            | Unsigned, 256L -> Some Subtype_u256
            | Signed, 256L -> Some Subtype_i256
            | Unsigned, 512L -> Some Subtype_u512
            | Signed, 512L -> Some Subtype_i512
            | Unsigned, 0L -> begin
                let _cp, digits_cursor = Text.Cursor.next suffix_cursor in
                match Text.Cursor.(digits_cursor = cursor) with
                | true -> Some Subtype_u64 (* "u" suffix. *)
                | false -> None
              end
            | Signed, 0L -> begin
                let _cp, digits_cursor = Text.Cursor.next suffix_cursor in
                match Text.Cursor.(digits_cursor = cursor) with
                | true -> Some Subtype_i64 (* "i" suffix. *)
                | false -> None
              end
            | _ -> None
          end
        | None -> None
      in
      let accum' = match subtype_opt with
        | None -> begin
            let mal = unsupported_bitwidth suffix_cursor cursor t in
            accum_mal mal accum
          end
        | Some _ -> accum
      in
      accept subtype_opt accum' radix cursor t
    end in
    match Text.Cursor.next_opt cursor with
    | None -> accept_subtype bitwidth signedness suffix_cursor accum radix cursor t
    | Some (cp, cursor') -> begin
        match Map.get cp num_suffix_map with
        | Some NSMapDigit -> begin
            let digit = nat_of_cp cp in
            match Nat.(bitwidth = zero && digit = zero) with
            | true -> begin
                let mal = invalid_type_suffix_leading_zero cursor cursor' t in
                let accum' = accum_mal mal accum in
                next_suffix bitwidth signedness suffix_cursor accum' radix cursor' t
              end
            | false -> begin
                let bitwidth' = Radix.(nat_accum digit bitwidth Dec) in
                next_suffix bitwidth' signedness suffix_cursor accum radix cursor' t
              end
          end
        | Some NSMapIdent -> begin
            let mal = invalid_numerical cursor cursor' t in
            let accum' = accum_mal mal accum in
            next_suffix bitwidth signedness suffix_cursor accum' radix cursor' t
          end
        | None ->
          accept_subtype bitwidth signedness suffix_cursor accum radix cursor t
      end

  type whole_map =
    | WMapUscore
    | WMapDigit
    | WMapDot
    | WMapExp
    | WMapUnsSuffix
    | WMapIntSuffix
    | WMapRealSuffix
    | WMapMalIdent

  let bin_whole_map = map_of_cps_alist [
    ("_", WMapUscore);
    ("01", WMapDigit);
    (".", WMapDot);
    ("p", WMapExp);
    ("u", WMapUnsSuffix);
    ("i", WMapIntSuffix);
    ("r", WMapRealSuffix);
    ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghjklmnoqstvwxyz23456789'",
      WMapMalIdent);
  ]

  let oct_whole_map = map_of_cps_alist [
    ("_", WMapUscore);
    ("01234567", WMapDigit);
    (".", WMapDot);
    ("p", WMapExp);
    ("u", WMapUnsSuffix);
    ("i", WMapIntSuffix);
    ("r", WMapRealSuffix);
    ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghjklmnoqstvwxyz89'", WMapMalIdent);
  ]

  let dec_whole_map = map_of_cps_alist [
    ("_", WMapUscore);
    ("0123456789", WMapDigit);
    (".", WMapDot);
    ("e", WMapExp);
    ("u", WMapUnsSuffix);
    ("i", WMapIntSuffix);
    ("r", WMapRealSuffix);
    ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdfghjklmnopqstvwxyz'", WMapMalIdent);
  ]

  let hex_whole_map = map_of_cps_alist [
    ("_", WMapUscore);
    ("0123456789abcdef", WMapDigit);
    (".", WMapDot);
    ("p", WMapExp);
    ("u", WMapUnsSuffix);
    ("i", WMapIntSuffix);
    ("r", WMapRealSuffix);
    ("ABCDEFGHIJKLMNOPQRSTUVWXYZghjklmnoqstvwxyz'", WMapMalIdent);
  ]

  let real_accum_of_accum radix = function
    | N whole -> Real.of_whole whole radix
    | Malformations mals -> Real.of_mals mals

  let next_dot accum whole_cursor radix pcursor cursor t =
    match Text.Cursor.next_opt cursor with
    | None -> Real.dot (real_accum_of_accum radix accum) whole_cursor radix cursor t
    | Some (cp, _cursor') -> begin
        match Set.mem cp operator_set with
        | true -> accept None accum radix pcursor t
        | false -> Real.dot (real_accum_of_accum radix accum) whole_cursor radix cursor t
      end

  let rec next_whole accum whole_cursor whole_map radix cursor t =
    match Text.Cursor.next_opt cursor with
    | None -> accept None accum radix cursor t
    | Some (cp, cursor') -> begin
        match Map.get cp whole_map with
        | Some WMapUscore -> next_whole accum whole_cursor whole_map radix cursor' t
        | Some WMapDigit -> begin
            let accum' = accum_whole_digit cp radix accum in
            next_whole accum' whole_cursor whole_map radix cursor' t
          end
        | Some WMapDot -> next_dot accum whole_cursor radix cursor cursor' t
        | Some WMapExp -> Real.exp (real_accum_of_accum radix accum) cursor radix cursor' t
        | Some WMapUnsSuffix -> next_suffix Nat.k_0 Unsigned cursor accum radix cursor' t
        | Some WMapIntSuffix -> next_suffix Nat.k_0 Signed cursor accum radix cursor' t
        | Some WMapRealSuffix ->
          Real.r_suffix (real_accum_of_accum radix accum) cursor radix cursor' t
        | Some WMapMalIdent -> begin
            let mal = invalid_numerical cursor cursor' t in
            let accum' = accum_mal mal accum in
            next_whole accum' whole_cursor whole_map radix cursor' t
          end
        | None -> accept None accum radix cursor t
      end

  (* Entry point functions follow. *)

  let zero = AbstractToken.Tok_u64 (Constant U64.zero)

  let bin n _ppcursor _pcursor cursor t =
    let accum = N n in
    next_whole accum cursor bin_whole_map Bin cursor t

  let oct n _ppcursor _pcursor cursor t =
    let accum = N n in
    next_whole accum cursor oct_whole_map Oct cursor t

  let dec n _ppcursor _pcursor cursor t =
    let accum = N n in
    next_whole accum t.tok_base dec_whole_map Dec cursor t

  let hex n _ppcursor _pcursor cursor t =
    let accum = N n in
    next_whole accum cursor hex_whole_map Hex cursor t

  let mal_ident _ppcursor pcursor cursor t =
    let mal = invalid_numerical pcursor cursor t in
    let accum = Malformations [mal] in
    next_whole accum t.tok_base dec_whole_map Dec cursor t

  let zero_u_suffix _ppcursor pcursor cursor t =
    let accum = N Nat.k_0 in
    next_suffix Nat.k_0 Unsigned pcursor accum Dec cursor t

  let zero_i_suffix _ppcursor pcursor cursor t =
    let accum = N Nat.k_0 in
    next_suffix Nat.k_0 Signed pcursor accum Dec cursor t
end

module View = struct
  type t = {
    ppcursor: Text.Cursor.t;
    pcursor: Text.Cursor.t;
    cursor: Text.Cursor.t;
  }

  let pp {ppcursor; pcursor; cursor} formatter =
    formatter
    |> Fmt.fmt "{ppcursor=" |> Text.Pos.pp (Text.Cursor.pos ppcursor)
    |> Fmt.fmt "; pcursor=" |> Text.Pos.pp (Text.Cursor.pos pcursor)
    |> Fmt.fmt "; cursor=" |> Text.Pos.pp (Text.Cursor.pos cursor)
    |> Fmt.fmt "}"

  let of_t t =
    {ppcursor=t.tok_base; pcursor=t.tok_base; cursor=t.tok_base}

  let next {pcursor; cursor; _} =
    match Text.Cursor.next_opt cursor with
    | None -> None
    | Some (cp, cursor') -> Some (cp, {ppcursor=pcursor; pcursor=cursor; cursor=cursor'})
end

module State = struct
  module Src_path = struct
    type t = {
      mals: AbstractToken.Rendition.Malformation.t list;
      path: codepoint list option;
    }

    let pp {mals; path} formatter =
      formatter
      |> Fmt.fmt "{mals=" |> (List.pp AbstractToken.Rendition.Malformation.pp) mals
      |> Fmt.fmt "; path=" |> (Option.pp (List.pp Codepoint.pp)) path
      |> Fmt.fmt "}"

    let empty = {mals=[]; path=None}

    let mals_accum mal ({mals; _} as t) =
      {t with mals=mal :: mals}

    let path_accum cp ({path; _} as t) =
      match path with
      | None -> {t with path=Some [cp]}
      | Some cps -> {t with path=Some (cp :: cps)}
  end

  module Src_path_bslash = struct
    type t = {
      mals: AbstractToken.Rendition.Malformation.t list;
      path: codepoint list option;
      bslash_cursor: Text.Cursor.t;
    }

    let pp {mals; path; bslash_cursor} formatter =
      formatter
      |> Fmt.fmt "{mals=" |> (List.pp AbstractToken.Rendition.Malformation.pp) mals
      |> Fmt.fmt "; path=" |> (Option.pp (List.pp Codepoint.pp)) path
      |> Fmt.fmt "; bslash_cursor=" |> Text.Pos.pp (Text.Cursor.pos bslash_cursor)
      |> Fmt.fmt "}"

    let mals_accum mal {mals; path; _} =
      Src_path.{mals=mal :: mals; path}

    let path_accum cp {mals; path; _} =
      match path with
      | None -> Src_path.{mals; path=Some [cp]}
      | Some cps -> Src_path.{mals; path=Some (cp :: cps)}
  end

  module Src_path_bslash_u = Src_path_bslash

  module Src_path_bslash_u_lcurly = struct
    type t = {
      mals: AbstractToken.Rendition.Malformation.t list;
      path: codepoint list option;
      bslash_cursor: Text.Cursor.t;
      u: Nat.t;
    }

    let pp {mals; path; bslash_cursor; u} formatter =
      formatter
      |> Fmt.fmt "{mals=" |> (List.pp AbstractToken.Rendition.Malformation.pp) mals
      |> Fmt.fmt "; path=" |> (Option.pp (List.pp Codepoint.pp)) path
      |> Fmt.fmt "; bslash_cursor=" |> Text.Pos.pp (Text.Cursor.pos bslash_cursor)
      |> Fmt.fmt "; u=" |> Nat.fmt ~alt:true ~base:Fmt.Hex ~pretty:true u
      |> Fmt.fmt "}"

    let init ~mals ~path ~bslash_cursor =
      {mals; path; bslash_cursor; u=Nat.k_0}

    let mals_accum mal {mals; path; _} =
      Src_path.{mals=mal :: mals; path}

    let path_accum cp {mals; path; _} =
      match path with
      | None -> Src_path.{mals; path=Some [cp]}
      | Some cps -> Src_path.{mals; path=Some (cp :: cps)}

    let u_accum digit ({u; _} as t) =
      {t with u=Nat.(u * k_g + digit)}
  end

  module Src_rditto = Src_path

  module Src_path_colon = Src_path

  module Src_line = struct
    type t = {
      mals: AbstractToken.Rendition.Malformation.t list;
      path: codepoint list option;
      line_cursor: Text.Cursor.t;
      line: Nat.t;
    }

    let pp {mals; path; line_cursor; line} formatter =
      formatter
      |> Fmt.fmt "{mals=" |> (List.pp AbstractToken.Rendition.Malformation.pp) mals
      |> Fmt.fmt "; path=" |> (Option.pp (List.pp Codepoint.pp)) path
      |> Fmt.fmt "; line_cursor=" |> Text.Pos.pp (Text.Cursor.pos line_cursor)
      |> Fmt.fmt "; line=" |> Nat.pp line
      |> Fmt.fmt "}"

    let init ?(mals=[]) ?(path=None) ~line_cursor line =
      {mals; path; line_cursor; line}

    let line_accum digit ({line; _} as t) =
      {t with line=Nat.(line * k_a + digit)}
  end

  module Src_line_colon = struct
    type t = {
      mals: AbstractToken.Rendition.Malformation.t list;
      path: codepoint list option;
      line: Nat.t;
    }

    let pp {mals; path; line} formatter =
      formatter
      |> Fmt.fmt "{mals=" |> (List.pp AbstractToken.Rendition.Malformation.pp) mals
      |> Fmt.fmt "; path=" |> (Option.pp (List.pp Codepoint.pp)) path
      |> Fmt.fmt "; line=" |> Nat.pp line
      |> Fmt.fmt "}"

    let init ?(mals=[]) ?(path=None) line =
      {mals; path; line}

    let mals_accum mal ({mals; _} as t) =
      {t with mals=mal :: mals}
  end

  module Src_col = struct
    type t = {
      mals: AbstractToken.Rendition.Malformation.t list;
      path: codepoint list option;
      line: Nat.t;
      col_cursor: Text.Cursor.t;
      col: Nat.t;
    }

    let pp {mals; path; line; col_cursor; col} formatter =
      formatter
      |> Fmt.fmt "{mals=" |> (List.pp AbstractToken.Rendition.Malformation.pp) mals
      |> Fmt.fmt "; path=" |> (Option.pp (List.pp Codepoint.pp)) path
      |> Fmt.fmt "; line=" |> Nat.pp line
      |> Fmt.fmt "; col_cursor=" |> Text.Pos.pp (Text.Cursor.pos col_cursor)
      |> Fmt.fmt "; col=" |> Nat.pp col
      |> Fmt.fmt "}"

    let init ~mals ~path ~line ~col_cursor ~col =
      {mals; path; line; col_cursor; col}

    let mals_accum mal ({mals; _} as t) =
      {t with mals=mal :: mals}

    let col_accum digit ({col; _} as t) =
      {t with col=Nat.(col * k_a + digit)}
  end

  module Src_rcolon = struct
    type t = {
      mals: AbstractToken.Rendition.Malformation.t list;
      path: codepoint list option;
      line: Nat.t;
      col: Nat.t;
    }

    let pp {mals; path; line; col} formatter =
      formatter
      |> Fmt.fmt "{mals=" |> (List.pp AbstractToken.Rendition.Malformation.pp) mals
      |> Fmt.fmt "; path=" |> (Option.pp (List.pp Codepoint.pp)) path
      |> Fmt.fmt "; line=" |> Nat.pp line
      |> Fmt.fmt "; col=" |> Nat.pp col
      |> Fmt.fmt "}"

    let init ~mals ~path ~line ~col =
      {mals; path; line; col}

    let mals_accum mal ({mals; _} as t) =
      {t with mals=mal :: mals}

    let render {mals; path; line; col} =
      match mals with
      | [] -> begin
          let path = match path with
            | None -> None
            | Some cps -> Some (String.of_list_rev cps)
          in
          let line = Nat.to_u64_hlt line in
          let col = Nat.to_u64_hlt col in
          AbstractToken.Tok_source_directive (Constant {path; line; col})
        end
      | _ :: _ -> AbstractToken.Tok_source_directive (AbstractToken.Rendition.of_mals mals)
  end

  module CodepointAccum = struct
    type t =
      | Codepoints of codepoint list
      | Malformations of AbstractToken.Rendition.Malformation.t list

    let pp t formatter =
      match t with
      | Codepoints cps ->
        formatter |> Fmt.fmt "(Codepoints " |> (List.pp Codepoint.pp) cps |> Fmt.fmt ")"
      | Malformations mals ->
        formatter |> Fmt.fmt "(Malformations "
        |> (List.pp AbstractToken.Rendition.Malformation.pp) mals |> Fmt.fmt ")"

    let accum_cp cp = function
      | Codepoints cps -> Codepoints (cp :: cps)
      | (Malformations _) as mals -> mals

    let accum_mal mal = function
      | Codepoints _ -> Malformations [mal]
      | Malformations mals -> Malformations (mal :: mals)

    let empty = Codepoints []
  end

  module Isubstring_start = struct
    type t = {
      accum: CodepointAccum.t;
    }

    let pp t formatter =
      formatter |> Fmt.fmt "{accum=" |> CodepointAccum.pp t.accum |> Fmt.fmt "}"
  end

  module Isubstring_bslash = struct
    type t = {
      accum: CodepointAccum.t;
      bslash_cursor: Text.Cursor.t;
    }

    let pp t formatter =
      formatter
      |> Fmt.fmt "{accum=" |> CodepointAccum.pp t.accum
      |> Fmt.fmt "; bslash_cursor=" |> Text.Pos.pp (Text.Cursor.pos t.bslash_cursor)
      |> Fmt.fmt "}"
  end

  module Isubstring_bslash_u = Isubstring_bslash

  module Isubstring_bslash_u_lcurly = struct
    type t = {
      accum: CodepointAccum.t;
      bslash_cursor: Text.Cursor.t;
      u: Nat.t;
    }

    let pp t formatter =
      formatter
      |> Fmt.fmt "{accum=" |> CodepointAccum.pp t.accum
      |> Fmt.fmt "; bslash_cursor=" |> Text.Pos.pp (Text.Cursor.pos t.bslash_cursor)
      |> Fmt.fmt "; u=" |> Nat.fmt ~alt:true ~base:Fmt.Hex ~pretty:true t.u
      |> Fmt.fmt "}"
  end

  type t =
    | State_start
    | State_semi
    | State_lparen
    | State_lbrack
    | State_lcurly
    | State_bslash
    | State_tilde
    | State_qmark
    | State_star
    | State_caret
    | State_bar
    | State_uscore
    | State_btick
    | State_0
    | State_0_dot
    | State_src_lcolon
    | State_src_path of Src_path.t
    | State_src_path_bslash of Src_path_bslash.t
    | State_src_path_bslash_u of Src_path_bslash_u.t
    | State_src_path_bslash_u_lcurly of Src_path_bslash_u_lcurly.t
    | State_src_rditto of Src_rditto.t
    | State_src_path_colon of Src_path_colon.t
    | State_src_line of Src_line.t
    | State_src_line_colon of Src_line_colon.t
    | State_src_col of Src_col.t
    | State_src_rcolon of Src_rcolon.t
    | State_whitespace
    | State_whitespace_bslash
    | State_hash_comment
    | State_isubstring_start of Isubstring_start.t
    | State_isubstring_bslash of Isubstring_bslash.t
    | State_isubstring_bslash_u of Isubstring_bslash_u.t
    | State_isubstring_bslash_u_lcurly of Isubstring_bslash_u_lcurly.t
    | State_spec_start
    | State_spec_lparen
    | State_rditto_start

  let pp t formatter =
    match t with
    | State_start -> formatter |> Fmt.fmt "State_start"
    | State_semi -> formatter |> Fmt.fmt "State_semi"
    | State_lparen -> formatter |> Fmt.fmt "State_lparen"
    | State_lbrack -> formatter |> Fmt.fmt "State_lbrack"
    | State_lcurly -> formatter |> Fmt.fmt "State_lcurly"
    | State_bslash -> formatter |> Fmt.fmt "State_bslash"
    | State_tilde -> formatter |> Fmt.fmt "State_tilde"
    | State_qmark -> formatter |> Fmt.fmt "State_qmark"
    | State_star -> formatter |> Fmt.fmt "State_star"
    | State_caret -> formatter |> Fmt.fmt "State_caret"
    | State_bar -> formatter |> Fmt.fmt "State_bar"
    | State_uscore -> formatter |> Fmt.fmt "State_uscore"
    | State_btick -> formatter |> Fmt.fmt "State_btick"
    | State_0 -> formatter |> Fmt.fmt "State_0"
    | State_0_dot -> formatter |> Fmt.fmt "State_0_dot"
    | State_src_lcolon -> formatter |> Fmt.fmt "State_src_lcolon"
    | State_src_path v -> formatter |> Fmt.fmt "State_path " |> Src_path.pp v
    | State_src_path_bslash v -> formatter |> Fmt.fmt "State_path_bslash " |> Src_path_bslash.pp v
    | State_src_path_bslash_u v ->
      formatter |> Fmt.fmt "State_path_bslash_u " |> Src_path_bslash_u.pp v
    | State_src_path_bslash_u_lcurly v ->
      formatter |> Fmt.fmt "State_path_bslash_u_lcurly " |> Src_path_bslash_u_lcurly.pp v
    | State_src_rditto v -> formatter |> Fmt.fmt "State_rditto " |> Src_rditto.pp v
    | State_src_path_colon v -> formatter |> Fmt.fmt "State_path_colon " |> Src_path_colon.pp v
    | State_src_line v -> formatter |> Fmt.fmt "State_line " |> Src_line.pp v
    | State_src_line_colon v -> formatter |> Fmt.fmt "State_line_colon " |> Src_line_colon.pp v
    | State_src_col v -> formatter |> Fmt.fmt "State_col " |> Src_col.pp v
    | State_src_rcolon v -> formatter |> Fmt.fmt "State_rcolon " |> Src_rcolon.pp v
    | State_whitespace -> formatter |> Fmt.fmt "State_whitespace"
    | State_whitespace_bslash -> formatter |> Fmt.fmt "State_whitespace_bslash"
    | State_hash_comment -> formatter |> Fmt.fmt "State_hash_comment"
    | State_isubstring_start v ->
      formatter |> Fmt.fmt "State_isubstring_start " |> Isubstring_start.pp v
    | State_isubstring_bslash v ->
      formatter |> Fmt.fmt "State_isubstring_bslash " |> Isubstring_bslash.pp v
    | State_isubstring_bslash_u v ->
      formatter |> Fmt.fmt "State_isubstring_bslash_u " |> Isubstring_bslash_u.pp v
    | State_isubstring_bslash_u_lcurly v ->
      formatter |> Fmt.fmt "State_isubstring_bslash_lcurly " |> Isubstring_bslash_u_lcurly.pp v
    | State_spec_start -> formatter |> Fmt.fmt "State_spec_start"
    | State_spec_lparen -> formatter |> Fmt.fmt "State_spec_lparen"
    | State_rditto_start -> formatter |> Fmt.fmt "State_rditto_start"

  let start = State_start
  let isubstring_start = State_isubstring_start {accum=CodepointAccum.empty}
  let spec_start = State_spec_start
  let rditto_start = State_rditto_start
end

module Dfa = struct
  type transition =
    | Advance of View.t * State.t
    | Retry of State.t
    | Accept of ConcreteToken.t

  type action0 = View.t -> t -> t * transition
  and node0 = {
    edges0: (codepoint, action0, Codepoint.cmper_witness) Map.t;
    default0: action0;
    eoi0: action0;
  }

  type 'a action1 = 'a -> View.t -> t -> t * transition
  and 'a node1 = {
    edges1: (codepoint, 'a action1, Codepoint.cmper_witness) Map.t;
    default1: 'a action1;
    eoi1: 'a action1;
  }

  let act0 trace node view t =
    let action0, view' = match View.next view with
      | Some (cp, view') -> begin
          match Map.get cp node.edges0 with
          | Some action0 -> begin
              if trace then
                File.Fmt.stdout |> Fmt.fmt "Scan: " |> Codepoint.pp cp |> Fmt.fmt " -> action0"
                |> ignore;
              action0, view'
            end
          | None -> begin
              if trace then
                File.Fmt.stdout |> Fmt.fmt "Scan: " |> Codepoint.pp cp |> Fmt.fmt " -> default0"
                |> ignore;
              node.default0, view'
            end
        end
      | None -> begin
          if trace then File.Fmt.stdout |> Fmt.fmt "Scan: eoi0" |> ignore;
          node.eoi0, view
        end
    in
    let t', transition = action0 view' t in
    t', transition

  let act1 trace node state_payload view t =
    let action1, view' = match View.next view with
      | Some (cp, view') -> begin
          match Map.get cp node.edges1 with
          | Some action1 -> begin
              if trace then
                File.Fmt.stdout |> Fmt.fmt "Scan: " |> Codepoint.pp cp |> Fmt.fmt " -> action1"
                |> ignore;
              action1, view'
            end
          | None -> begin
              if trace then
                File.Fmt.stdout |> Fmt.fmt "Scan: " |> Codepoint.pp cp |> Fmt.fmt " -> default1"
                |> ignore;
              node.default1, view'
            end
        end
      | None -> begin
          if trace then File.Fmt.stdout |> Fmt.fmt "Scan: eoi1" |> ignore;
          node.eoi1, view
        end
    in
    let t', transition = action1 state_payload view' t in
    t', transition

  let accept atoken cursor t =
    let source = source_at cursor t in
    {t with tok_base=cursor}, Accept (ConcreteToken.init atoken source)

  let accept_incl atoken View.{cursor; _} t =
    accept atoken cursor t

  let accept_excl atoken View.{pcursor; _} t =
    accept atoken pcursor t

  let accept_pexcl atoken View.{ppcursor; _} t =
    accept atoken ppcursor t

  let accept_source_directive atoken cursor t =
    let t' = match atoken with
      | AbstractToken.Tok_source_directive Constant {path; line; col} -> begin
          let path = match path with
            | None -> t.path
            | Some path -> Some path
          in
          let line_bias = Sint.((Uns.bits_to_sint line) - (Uns.bits_to_sint (Text.(Pos.line
              (Cursor.pos cursor))))) in
          let col_bias = Sint.((Uns.bits_to_sint col) - (Uns.bits_to_sint (Text.(Pos.col
              (Cursor.pos cursor))))) in
          {t with path; line_bias; col_bias}
        end
      | _ -> t
    in
    (* Treat the directive as having come from the actual file being read. *)
    let source = Source.init ~path:Text.(path (Cursor.container t.tok_base)) ~line_bias:Sint.zero
      ~col_bias:Sint.zero ~base:t.tok_base ~past:cursor in
    {t' with tok_base=cursor}, Accept (ConcreteToken.init atoken source)

  let accept_line_delim atoken cursor t =
    let source = source_at cursor t in
    {t with tok_base=cursor; line_state=Line_delim}, Accept (ConcreteToken.init atoken source)

  let accept_line_delim_incl atoken View.{cursor; _} t =
    accept_line_delim atoken cursor t

  let accept_dentation atoken View.{cursor; _} t =
    accept atoken cursor {t with line_state=Line_body}

  (* XXX Refactor out. *)
  let wrap_legacy f View.{ppcursor; pcursor; cursor} t =
    let t', tok = f ppcursor pcursor cursor t in
    t', Accept tok

  let advance state' view t =
    t, Advance (view, state')

  let retry state' t =
    t, Retry state'

  let node0_start =
    {
      edges0=map_of_cps_alist [
        (",", accept_incl Tok_comma);
        (";", advance State_semi);
        ("(", advance State_lparen);
        (")", accept_incl Tok_rparen);
        ("[", advance State_lbrack);
        ("]", accept_incl Tok_rbrack);
        ("{", advance State_lcurly);
        ("}", accept_incl Tok_rcurly);
        ("\\", advance State_bslash);
        ("&", accept_incl Tok_amp);
        ("!", accept_incl Tok_xmark);
        ("\n", accept_line_delim_incl Tok_whitespace);
        ("~", advance State_tilde);
        ("?", advance State_qmark);
        ("*", advance State_star);
        ("/", wrap_legacy (operator (fun s -> Tok_slash_op s)));
        ("%", wrap_legacy (operator (fun s -> Tok_pct_op s)));
        ("+", wrap_legacy (operator (fun s -> Tok_plus_op s)));
        ("-", wrap_legacy (operator (fun s -> Tok_minus_op s)));
        ("@", wrap_legacy (operator (fun s -> Tok_at_op s)));
        ("^", advance State_caret);
        ("$", wrap_legacy (operator (fun s -> Tok_dollar_op s)));
        ("<", wrap_legacy (operator (fun s -> Tok_lt_op s)));
        ("=", wrap_legacy (operator (fun s -> Tok_eq_op s)));
        (">", wrap_legacy (operator (fun s -> Tok_gt_op s)));
        ("|", advance State_bar);
        (":", wrap_legacy (operator (fun s -> Tok_colon_op s)));
        (".", wrap_legacy (operator (fun s -> Tok_dot_op s)));
        (" ", advance State_whitespace);
        ("#", advance State_hash_comment);
        ("_", advance State_uscore);
        ("abcdefghijklmnopqrstuvwxyz", wrap_legacy uident);
        ("ABCDEFGHIJKLMNOPQRSTUVWXYZ", wrap_legacy cident);
        ("'", wrap_legacy Codepoint_.codepoint);
        ("\"", wrap_legacy (accept_istring_push Istring_interp Tok_istring_lditto));
        ("`", advance State_btick);
        ("0", advance State_0);
        ("123456789", (fun (View.{pcursor; _} as view) t ->
            let digit = nat_of_cp (Text.Cursor.rget pcursor) in
            wrap_legacy Integer.(dec digit) view t
          )
        );
      ];
      default0=accept_incl Tok_error;
      eoi0=(fun view t ->
        match t.line_state, t.level with
        | Line_delim, 0L -> accept_dentation Tok_line_delim view t
        | _, 0L -> accept_incl Tok_end_of_input view t
        | _ -> accept_dentation (Tok_dedent (Constant ())) view {t with level=Uns.pred t.level}
      );
    }

  let node0_semi = {
    edges0=map_of_cps_alist [
      (";", accept_incl Tok_semi_semi);
    ];
    default0=accept_excl Tok_semi;
    eoi0=accept_incl Tok_semi;
  }

  let node0_lparen = {
    edges0=map_of_cps_alist [
      ("*", wrap_legacy paren_comment);
    ];
    default0=accept_excl Tok_lparen;
    eoi0=accept_incl Tok_lparen;
  }

  let node0_lbrack = {
    edges0=map_of_cps_alist [
      ("|", accept_incl Tok_larray);
      (":", advance State_src_lcolon);
    ];
    default0=accept_excl Tok_lbrack;
    eoi0=accept_incl Tok_lbrack;
  }

  let node0_lcurly = {
    edges0=map_of_cps_alist [
      ("|", accept_incl Tok_lmodule);
    ];
    default0=accept_excl Tok_lcurly;
    eoi0=accept_incl Tok_lcurly;
  }

  let node0_bslash = {
    edges0=map_of_cps_alist [
      ("\n", advance State_whitespace);
    ];
    default0=accept_excl Tok_bslash;
    eoi0=accept_incl Tok_bslash;
  }

  let node0_tilde = {
    edges0=map_of_cps_alist [
      (operator_cps, wrap_legacy (operator (fun s -> Tok_tilde_op s)));
    ];
    default0=accept_excl Tok_tilde;
    eoi0=accept_incl Tok_tilde;
  }

  let node0_qmark = {
    edges0=map_of_cps_alist [
      (operator_cps, wrap_legacy (operator (fun s -> Tok_qmark_op s)));
    ];
    default0=accept_excl Tok_qmark;
    eoi0=accept_incl Tok_qmark;
  }

  let node0_star = {
    edges0=map_of_cps_alist [
      ("*", wrap_legacy (operator (fun s -> Tok_star_star_op s)));
      ("-+/%@^$<=>|:.~?", wrap_legacy(operator (fun s -> Tok_star_op s)));
    ];
    default0=accept_excl (Tok_star_op "*");
    eoi0=accept_incl (Tok_star_op "*");
  }

  let node0_caret = {
    edges0=map_of_cps_alist [
      (")", (fun view t ->
          (* XXX We need to know whether this is the {width,precision,fmt} vs value expression,
           * so that we can transition to Istring_spec vs Istring_interp. *)
          (* XXX This may be better implemented as a `String_.state_next` function based on
           * current state and token being accepted. *)
          match t.istring_state with
          | Istring_expr_value _XXX :: _ ->
            wrap_legacy (accept_istring_trans Istring_interp Tok_istring_caret_rparen) view t
          | _ :: _ -> not_implemented "XXX"
          | [] -> accept_excl Tok_caret view t
        )
      );
      (operator_cps, wrap_legacy (operator (fun s -> Tok_caret_op s)));
    ];
    default0=accept_excl Tok_caret;
    eoi0=accept_incl Tok_caret;
  }

  let node0_bar = {
    edges0=map_of_cps_alist [
      ("]", accept_incl Tok_rarray);
      ("}", accept_incl Tok_rmodule);
      (operator_cps, wrap_legacy (operator (fun s -> Tok_bar_op s)));
    ];
    default0=accept_excl Tok_bar;
    eoi0=accept_incl Tok_bar;
  }

  let node0_uscore = {
    edges0=map_of_cps_alist [
      ("abcdefghijklmnopqrstuvwxyz0123456789'", wrap_legacy uident);
      ("ABCDEFGHIJKLMNOPQRSTUVWXYZ", wrap_legacy cident);
      ("_", wrap_legacy uscore_ident);
    ];
    default0=accept_excl Tok_uscore;
    eoi0=accept_incl Tok_uscore;
  }

  let node0_btick = {
    edges0=map_of_cps_alist [
      ("|", wrap_legacy String_.bstring);
    ];
    default0=wrap_legacy String_.rstring;
    eoi0=(fun View.{cursor; _} t ->
      let t', tok = String_.accept_unterminated_rstring cursor t in
      t', Accept tok
    );
  }

  let node0_0 = {
    edges0=map_of_cps_alist [
      ("_", wrap_legacy Integer.(dec Nat.k_0));
      ("0123456789", (fun (View.{pcursor; _} as view) t ->
          let digit = nat_of_cp (Text.Cursor.rget pcursor) in
          wrap_legacy Integer.(dec digit) view t
        )
      );
      ("b", wrap_legacy Integer.(bin Nat.k_0));
      ("o", wrap_legacy Integer.(oct Nat.k_0));
      ("x", wrap_legacy Integer.(hex Nat.k_0));
      ("u", wrap_legacy Integer.zero_u_suffix);
      ("i", wrap_legacy Integer.zero_i_suffix);
      ("r", wrap_legacy Real.zero_r_suffix);
      (".", advance State_0_dot);
      ("e", wrap_legacy Real.zero_exp);
      ("ABCDEFGHIJKLMNOPQRSTUVWXYZacdfghjklmnpqstvwyz'", wrap_legacy Integer.mal_ident);
    ];
    default0=accept_excl Integer.zero;
    eoi0=accept_incl Integer.zero;
  }

  let node0_0_dot = {
    edges0=map_of_cps_alist [
      (operator_cps, accept_pexcl Integer.zero);
    ];
    default0=wrap_legacy Real.zero_frac;
    eoi0=accept_incl Real.zero;
  }

  let node0_src_lcolon = {
    edges0=map_of_cps_alist [
      ("\"", advance (State_src_path State.Src_path.empty));
      ("123456789", (fun ({pcursor; _} as view) t ->
          let digit = nat_of_cp (Text.Cursor.rget pcursor) in
          advance (State_src_line (State.Src_line.init ~line_cursor:pcursor digit)) view t));
      (":", (fun view t ->
          advance (State_src_rcolon (State.Src_rcolon.init ~mals:[] ~path:None ~line:Nat.k_1
            ~col:Nat.k_1)) view t
        )
      );
      ("]", (fun {pcursor; cursor; _} t ->
          let mal = unexpected_codepoint_source_directive pcursor cursor t in
          accept (Tok_source_directive (AbstractToken.Rendition.of_mals [mal])) cursor t
        )
      );
    ];
    default0=(fun ({pcursor; cursor; _} as view) t ->
      let mal = unexpected_codepoint_source_directive pcursor cursor t in
      advance (State_src_line (State.Src_line.init ~mals:[mal] ~line_cursor:cursor Nat.k_0)) view t
    );
    eoi0=(fun {cursor; _} t ->
      let mal = unterminated_source_directive t.tok_base cursor t in
      accept (Tok_source_directive (AbstractToken.Rendition.of_mals [mal])) cursor t
    );
  }

  let node1_src_path =
    let open State.Src_path in
    let open View in
    {
      edges1=map_of_cps_alist [
        ("%", (fun state ({pcursor; cursor; _} as view) t ->
            let mal = missing_backslash pcursor cursor t in
            advance (State_src_path (state |> mals_accum mal)) view t
          )
        );
        ("\"", (fun state view t -> advance (State_src_rditto state) view t));
        ("\\", (fun {mals; path} ({pcursor; _} as view) t ->
            advance (State_src_path_bslash {mals; path; bslash_cursor=pcursor}) view t));
      ];
      default1=(fun state ({pcursor; _} as view) t ->
        advance (State_src_path (state |> path_accum (Text.Cursor.rget pcursor))) view t
      );
      eoi1=(fun {mals; _} {cursor; _} t ->
        let mal = unterminated_source_directive t.tok_base cursor t in
        accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
      );
    }

  let node1_src_path_bslash =
    let open State.Src_path_bslash in
    {
      edges1=map_of_cps_alist [
        ("u", (fun state view t -> advance (State_src_path_bslash_u state) view t));
        ("t", (fun state view t ->
            advance (State_src_path (state |> path_accum Codepoint.ht)) view t));
        ("n", (fun state view t ->
            advance (State_src_path (state |> path_accum Codepoint.nl)) view t));
        ("r", (fun state view t ->
            advance (State_src_path (state |> path_accum Codepoint.cr)) view t));
        ("\"\\%", (fun state ({pcursor; _} as view) t ->
            advance (State_src_path (state |> path_accum (Text.Cursor.rget pcursor))) view t
          )
        );
      ];
      default1=(fun ({bslash_cursor; _} as state) {cursor; _} t ->
        let mal = illegal_backslash bslash_cursor cursor t in
        retry (State_src_path (state |> mals_accum mal)) t
      );
      eoi1=(fun {mals; _} {cursor; _} t ->
        let mal = unterminated_source_directive t.tok_base cursor t in
        accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
      );
    }

  let node1_src_path_bslash_u =
    let open State.Src_path_bslash_u in
    {
      edges1=map_of_cps_alist [
        ("{", (fun {mals; path; bslash_cursor} view t ->
            advance (State_src_path_bslash_u_lcurly (State.Src_path_bslash_u_lcurly.init ~mals ~path
                ~bslash_cursor)) view t
          )
        );
        ("\"", (fun ({bslash_cursor; _} as state) ({cursor; _} as view) t ->
            let mal = illegal_backslash bslash_cursor cursor t in
            advance (State_src_rditto (state |> mals_accum mal)) view t
          )
        );
        ("]", (fun {mals; _} {pcursor; cursor; _} t ->
            let mal = unexpected_codepoint_source_directive pcursor cursor t in
            accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
          )
        );
      ];
      default1=(fun ({bslash_cursor; _} as state) ({cursor; _} as view) t ->
        let mal = illegal_backslash bslash_cursor cursor t in
        advance (State_src_path (state |> mals_accum mal)) view t
      );
      eoi1=(fun {mals; _} {cursor; _} t ->
        let mal = unterminated_source_directive t.tok_base cursor t in
        accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
      );
    }

  let node1_src_path_bslash_u_lcurly =
    let open State.Src_path_bslash_u_lcurly in
    {
      edges1=map_of_cps_alist [
        ("_", (fun state view t -> advance (State_src_path_bslash_u_lcurly state) view t));
        ("0123456789abcdef", (fun state ({pcursor; _} as view) t ->
            let digit = nat_of_cp (Text.Cursor.rget pcursor) in
            advance (State_src_path_bslash_u_lcurly (state |> u_accum digit)) view t
          )
        );
        ("}", (fun ({bslash_cursor; u; _} as state) ({cursor; _} as view) t ->
            advance (State_src_path (
              Option.value_map (Nat.to_uns_opt u)
                ~f:(fun u -> Codepoint.narrow_of_uns_opt u)
                ~default:None
              |> Option.value_map
                ~f:(fun cp -> (state |> path_accum cp))
                ~default:(state |> mals_accum (invalid_unicode bslash_cursor cursor t))
            )) view t
          )
        );
        ("\"", (fun ({bslash_cursor; _} as state) ({cursor; _} as view) t ->
            let mal = illegal_backslash bslash_cursor cursor t in
            advance (State_src_rditto (state |> mals_accum mal)) view t
          )
        );
        ("]", (fun {mals; _} {pcursor; cursor; _} t ->
            let mal = unexpected_codepoint_source_directive pcursor cursor t in
            accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
          )
        );
      ];
      default1=(fun ({bslash_cursor; _} as state) ({cursor; _} as view) t ->
        let mal = partial_unicode bslash_cursor cursor t in
        advance (State_src_path (state |> mals_accum mal)) view t
      );
      eoi1=(fun {mals; _} {cursor; _} t ->
        let mal = unterminated_source_directive t.tok_base cursor t in
        accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
      );
    }

  let node1_src_rditto =
    let open State.Src_rditto in
    {
      edges1=map_of_cps_alist [
        (":", (fun state view t -> advance (State_src_path_colon state) view t));
        ("]", (fun {mals; _} {pcursor; cursor; _} t ->
            let mal = unexpected_codepoint_source_directive pcursor cursor t in
            accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
          )
        );
      ];
      default1=(fun state ({pcursor; cursor; _} as view) t ->
        let mal = unexpected_codepoint_source_directive pcursor cursor t in
        advance (State_src_rditto (state |> mals_accum mal)) view t
      );
      eoi1=(fun {mals; _} {cursor; _} t ->
        let mal = unterminated_source_directive t.tok_base cursor t in
        accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
      );
    }

  let node1_src_path_colon =
    let open State.Src_path_colon in
    let open View in
    {
      edges1=map_of_cps_alist [
        ("123456789", (fun {mals; path} ({pcursor; _} as view) t ->
            let digit = nat_of_cp (Text.Cursor.rget pcursor) in
            advance (State_src_line (State.Src_line.init ~mals ~path ~line_cursor:pcursor digit))
              view t));
        ("]", (fun {mals; path} {cursor; _} t ->
            let tok = State.Src_rcolon.(render (init ~mals ~path ~line:Nat.k_1 ~col:Nat.k_1)) in
            accept_source_directive tok cursor t
          )
        );
      ];
      default1=(fun state ({pcursor; cursor; _} as view) t ->
        let mal = unexpected_codepoint_source_directive pcursor cursor t in
        advance (State_src_rditto (state |> mals_accum mal)) view t
      );
      eoi1=(fun {mals; _} {cursor; _} t ->
        let mal = unterminated_source_directive t.tok_base cursor t in
        accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
      );
    }

  let node1_src_line =
    let open State.Src_line in
    {
      edges1=map_of_cps_alist [
        ("0123456789", (fun state (View.{pcursor; _} as view) t ->
            let digit = nat_of_cp (Text.Cursor.rget pcursor) in
            advance (State_src_line (state |> line_accum digit)) view t
          )
        );
        (":", (fun {mals; path; line_cursor; line} ({pcursor; _} as view) t ->
            let mals = match Nat.(line > max_abs_i64) with
              | false -> mals
              | true -> begin
                  let description =
                    String.Fmt.empty
                    |> Fmt.fmt "Line exceeds "
                    |> Nat.fmt ~alt:true Nat.max_abs_i64
                    |> Fmt.to_string
                  in
                  let mal = malformation ~base:line_cursor ~past:pcursor description t in
                  mal :: mals
                end
            in
            advance (State_src_line_colon (State.Src_line_colon.init ~mals ~path line)) view t
          )
        );
        ("]", (fun {mals; _} {pcursor; cursor; _} t ->
            let mal = unexpected_codepoint_source_directive pcursor cursor t in
            accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
          )
        );
      ];
      default1=(fun {mals; path; _} ({pcursor; cursor; _} as view) t ->
        let mal = invalid_dec pcursor cursor t in
        advance (State_src_line (init ~mals:(mal :: mals) ~path ~line_cursor:cursor Nat.k_0)) view t
      );
      eoi1=(fun {mals; _} {cursor; _} t ->
        let mal = unterminated_source_directive t.tok_base cursor t in
        accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
      );
    }

  let node1_src_line_colon =
    let open State.Src_line_colon in
    {
      edges1=map_of_cps_alist [
        ("123456789", (fun {mals; path; line} (View.{pcursor; _} as view) t ->
            let digit = nat_of_cp (Text.Cursor.rget pcursor) in
            advance (State_src_col (State.Src_col.init ~mals ~path ~line ~col_cursor:pcursor
                ~col:digit)) view t));
        ("]", (fun {mals; path; line} {cursor; _} t ->
            let tok = State.Src_rcolon.(render (init ~mals ~path ~line ~col:Nat.k_1)) in
            accept_source_directive tok cursor t
          )
        );
      ];
      default1=(fun state ({pcursor; cursor; _} as view) t ->
        let mal = unexpected_codepoint_source_directive pcursor cursor t in
        advance (State_src_line_colon (state |> mals_accum mal)) view t
      );
      eoi1=(fun {mals; _} {cursor; _} t ->
        let mal = unterminated_source_directive t.tok_base cursor t in
        accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
      );
    }

  let node1_src_col =
    let open State.Src_col in
    {
      edges1=map_of_cps_alist [
        ("0123456789", (fun state (View.{pcursor; _} as view) t ->
            let digit = nat_of_cp (Text.Cursor.rget pcursor) in
            advance (State_src_col (state |> col_accum digit)) view t
          )
        );
        (":", (fun {mals; path; col_cursor; line; col} ({pcursor; _} as view) t ->
            let mals = match Nat.(col > max_abs_i64) with
              | false -> mals
              | true -> begin
                  let description =
                    String.Fmt.empty
                    |> Fmt.fmt "Column exceeds "
                    |> Nat.fmt ~alt:true Nat.max_abs_i64
                    |> Fmt.to_string
                  in
                  let mal = malformation ~base:col_cursor ~past:pcursor description t in
                  mal :: mals
                end
            in
            advance (State_src_rcolon (State.Src_rcolon.init ~mals ~path ~line ~col)) view t
          )
        );
        ("]", (fun {mals; _} {pcursor; cursor; _} t ->
            let mal = unexpected_codepoint_source_directive pcursor cursor t in
            accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
          )
        );
      ];
      default1=(fun state ({pcursor; cursor; _} as view) t ->
        let mal = unexpected_codepoint_source_directive pcursor cursor t in
        advance (State_src_col (state |> mals_accum mal)) view t
      );
      eoi1=(fun {mals; _} {cursor; _} t ->
        let mal = unterminated_source_directive t.tok_base cursor t in
        accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
      );
    }

  let node1_src_rcolon =
    let open State.Src_rcolon in
    let open View in
    {
      edges1=map_of_cps_alist [
        ("]", (fun state {cursor; _} t ->
            let tok = render state in
            accept_source_directive tok cursor t
          )
        );
      ];
      default1=(fun state ({pcursor; cursor; _} as view) t ->
        let mal = unexpected_codepoint_source_directive pcursor cursor t in
        advance (State_src_rcolon (state |> mals_accum mal)) view t
      );
      eoi1=(fun {mals; _} {cursor; _} t ->
        let mal = unterminated_source_directive t.tok_base cursor t in
        accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
      );
    }

  let node0_whitespace = {
    edges0=map_of_cps_alist [
      (" ", advance State_whitespace);
      ("\\", advance State_whitespace_bslash);
      ("\n", accept_line_delim_incl Tok_whitespace);
    ];
    default0=accept_excl Tok_whitespace;
    eoi0=accept_incl Tok_whitespace;
  }

  let node0_whitespace_bslash = {
    edges0=map_of_cps_alist [
      ("\n", advance State_whitespace);
    ];
    default0=accept_pexcl Tok_whitespace;
    eoi0=accept_excl Tok_whitespace;
  }

  let node0_hash_comment = {
    edges0=map_of_cps_alist [
      ("\n", accept_line_delim_incl Tok_hash_comment);
    ];
    default0=advance State_hash_comment;
    eoi0=accept_line_delim_incl Tok_hash_comment;
  }

  let node1_isubstring_start =
    let open State.Isubstring_start in
    let open View in
    let accept_isubstring_impl trans {accum} cursor t = begin
      let source = source_at cursor t in
      let open State.CodepointAccum in
      let tok = match accum with
        | Codepoints cps -> AbstractToken.Tok_isubstring (Constant (String.of_list_rev cps))
        | Malformations mals -> AbstractToken.Tok_isubstring (AbstractToken.Rendition.of_mals mals)
      in
      {t with tok_base=cursor; istring_state=trans :: (List.tl t.istring_state)},
      Accept (ConcreteToken.init tok source)
    end in
    let accept_isubstring trans accum {cursor; _} t = begin
      accept_isubstring_impl trans accum cursor t
    end in
    let accept_isubstring_excl trans accum {pcursor; _} t = begin
      accept_isubstring_impl trans accum pcursor t
    end in
    let accum_cp cp accum view t = begin
      advance (State_isubstring_start {accum=State.CodepointAccum.accum_cp cp accum}) view t
    end in
    let accum_raw {accum} ({pcursor; _} as view) t = begin
      let cp = Text.Cursor.rget pcursor in
      accum_cp cp accum view t
    end in
    {
      edges1=map_of_cps_alist [
        ("%", (accept_isubstring_excl Istring_spec_pct));
        ("\"", (accept_isubstring_excl Istring_rditto));
        ("\\", (fun {accum} ({pcursor; _} as view) t ->
            advance (State_isubstring_bslash {accum; bslash_cursor=pcursor}) view t
          )
        );
      ];
      default1=accum_raw;
      eoi1=(accept_isubstring Istring_rditto);
    }

  (* XXX Reduce helper duplication among nodes. *)
  let node1_isubstring_bslash =
    let open State.Isubstring_bslash in
    let open View in
    let accum_illegal_backslash accum bslash_cursor cursor t = begin
      let mal = illegal_backslash bslash_cursor cursor t in
      t, Retry (State_isubstring_start {accum=State.CodepointAccum.accum_mal mal accum})
    end in
    let default1 {accum; bslash_cursor} {cursor; _} t = begin
      accum_illegal_backslash accum bslash_cursor cursor t
    end in
    let accum_cp cp {accum; _} view t = begin
      advance (State_isubstring_start {accum=State.CodepointAccum.accum_cp cp accum}) view t
    end in
    let accum_raw state ({pcursor; _} as view) t = begin
      let cp = Text.Cursor.rget pcursor in
      accum_cp cp state view t
    end in
    {
      edges1=map_of_cps_alist [
        ("u", (fun {accum; bslash_cursor} view t ->
            advance (State_isubstring_bslash_u {accum; bslash_cursor}) view t
          )
        );
        ("t", accum_cp Codepoint.ht);
        ("n", accum_cp Codepoint.nl);
        ("r", accum_cp Codepoint.cr);
        ("\"\\%", accum_raw);
      ];
      default1;
      eoi1=default1;
    }

  let node1_isubstring_bslash_u =
    let open State.Isubstring_bslash_u in
    let open View in
    let accum_illegal_backslash accum bslash_cursor cursor t = begin
      let mal = illegal_backslash bslash_cursor cursor t in
      t, Retry (State_isubstring_start {accum=State.CodepointAccum.accum_mal mal accum})
    end in
    let default1 {accum; bslash_cursor} {pcursor; _} t = begin
      accum_illegal_backslash accum bslash_cursor pcursor t
    end in
    let accept_isubstring_impl trans accum cursor t = begin
      let source = source_at cursor t in
      let open State.CodepointAccum in
      let tok = match accum with
        | Codepoints cps -> AbstractToken.Tok_isubstring (Constant (String.of_list_rev cps))
        | Malformations mals -> AbstractToken.Tok_isubstring (AbstractToken.Rendition.of_mals mals)
      in
      {t with tok_base=cursor; istring_state=trans :: (List.tl t.istring_state)},
      Accept (ConcreteToken.init tok source)
    end in
    let accept_isubstring_excl trans accum {pcursor; _} t = begin
      accept_isubstring_impl trans accum pcursor t
    end in
    {
      edges1=map_of_cps_alist [
        ("{", (fun {accum; bslash_cursor} view t ->
            advance (State_isubstring_bslash_u_lcurly {accum; bslash_cursor; u=Nat.k_0}) view t
          ));
        ("\"", (fun {accum; bslash_cursor} ({cursor; _} as view) t ->
            let mal = illegal_backslash bslash_cursor cursor t in
            accept_isubstring_excl Istring_rditto (State.CodepointAccum.accum_mal mal accum) view t
          ));
      ];
      default1;
      eoi1=default1;
    }

  let node1_isubstring_bslash_u_lcurly =
    let open State.Isubstring_bslash_u_lcurly in
    let open View in
    let accum_invalid_unicode accum bslash_cursor cursor t = begin
      let mal = invalid_unicode bslash_cursor cursor t in
      t, Retry (State_isubstring_start {accum=State.CodepointAccum.accum_mal mal accum})
    end in
    let accum_partial_unicode accum bslash_cursor cursor t = begin
      let mal = partial_unicode bslash_cursor cursor t in
      t, Retry (State_isubstring_start {accum=State.CodepointAccum.accum_mal mal accum})
    end in
    let default1 {accum; bslash_cursor; _} {pcursor; _} t = begin
      accum_partial_unicode accum bslash_cursor pcursor t
    end in
    let accum_cp cp {accum; _} view t = begin
      advance (State_isubstring_start {accum=State.CodepointAccum.accum_cp cp accum}) view t
    end in
    let accept_isubstring_impl trans accum cursor t = begin
      let source = source_at cursor t in
      let open State.CodepointAccum in
      let tok = match accum with
        | Codepoints cps -> AbstractToken.Tok_isubstring (Constant (String.of_list_rev cps))
        | Malformations mals -> AbstractToken.Tok_isubstring (AbstractToken.Rendition.of_mals mals)
      in
      {t with tok_base=cursor; istring_state=trans :: (List.tl t.istring_state)},
      Accept (ConcreteToken.init tok source)
    end in
    let accept_isubstring_excl trans accum {pcursor; _} t = begin
      accept_isubstring_impl trans accum pcursor t
    end in
    {
      edges1=map_of_cps_alist [
        ("_", (fun state view t -> advance (State_isubstring_bslash_u_lcurly state) view t));
        ("0123456789abcdef", (fun {accum; bslash_cursor; u} ({pcursor; _} as view) t ->
            let cp = Text.Cursor.rget pcursor in
            let u' = Radix.(nat_accum (nat_of_cp cp) u Hex) in
            advance (State_isubstring_bslash_u_lcurly {accum; bslash_cursor; u=u'}) view t
          ));
        ("}", (fun ({accum; bslash_cursor; u} as state) ({cursor; _} as view) t ->
            Option.value_map (Nat.to_uns_opt u)
              ~f:(fun u -> Codepoint.narrow_of_uns_opt u)
              ~default:None
            |> Option.value_map
              ~f:(fun cp -> accum_cp cp state view t)
              ~default:(accum_invalid_unicode accum bslash_cursor cursor t)
          ));
        ("\"", (fun {accum; bslash_cursor; _} ({cursor; _} as view) t ->
            let mal = partial_unicode bslash_cursor cursor t in
            accept_isubstring_excl Istring_rditto (State.CodepointAccum.accum_mal mal accum) view t
          ));
      ];
      default1;
      eoi1=default1;
    }

  let node0_rditto_start =
    let open View in
    let accept_rditto tok {cursor; _} t = begin
      let source = source_at cursor t in
      {t with tok_base=cursor; istring_state=List.tl t.istring_state},
      Accept (ConcreteToken.init tok source)
    end in
    {
      edges0=map_of_cps_alist [
        ("\"", accept_rditto Tok_istring_rditto);
      ];
      default0=(fun _ _ -> not_reached ());
      eoi0=(accept_rditto Tok_end_of_input);
    }

  let error view t =
    accept_incl Tok_error view t

  let default0 = error

  let eoi0 = error

  let accept ~f View.{ppcursor; pcursor; cursor} t =
    let t', tok = f ppcursor pcursor cursor t in
    t', Accept tok

  let node0_spec_start = {
    edges0=map_of_cps_alist [
      ("'", accept ~f:Codepoint_.codepoint);
      ("(", advance State_spec_lparen)
    ]; default0; eoi0;
  }

  let node0_spec_lparen = {
    edges0=map_of_cps_alist [
      ("^", accept ~f:(accept_istring_trans Istring_expr_width Tok_istring_lparen_caret));
    ]; default0; eoi0;
  }

  let transition_of_state trace state view t =
    let open State in
    match state with
    | State_start -> act0 trace node0_start view t
    | State_semi -> act0 trace node0_semi view t
    | State_lparen -> act0 trace node0_lparen view t
    | State_lbrack -> act0 trace node0_lbrack view t
    | State_lcurly -> act0 trace node0_lcurly view t
    | State_bslash -> act0 trace node0_bslash view t
    | State_tilde -> act0 trace node0_tilde view t
    | State_qmark -> act0 trace node0_qmark view t
    | State_star -> act0 trace node0_star view t
    | State_caret -> act0 trace node0_caret view t
    | State_bar -> act0 trace node0_bar view t
    | State_uscore -> act0 trace node0_uscore view t
    | State_btick -> act0 trace node0_btick view t
    | State_0 -> act0 trace node0_0 view t
    | State_0_dot -> act0 trace node0_0_dot view t
    | State_src_lcolon -> act0 trace node0_src_lcolon view t
    | State_src_path v -> act1 trace node1_src_path v view t
    | State_src_path_bslash v -> act1 trace node1_src_path_bslash v view t
    | State_src_path_bslash_u v -> act1 trace node1_src_path_bslash_u v view t
    | State_src_path_bslash_u_lcurly v -> act1 trace node1_src_path_bslash_u_lcurly v view t
    | State_src_rditto v -> act1 trace node1_src_rditto v view t
    | State_src_path_colon v -> act1 trace node1_src_path_colon v view t
    | State_src_line v -> act1 trace node1_src_line v view t
    | State_src_line_colon v -> act1 trace node1_src_line_colon v view t
    | State_src_col v -> act1 trace node1_src_col v view t
    | State_src_rcolon v -> act1 trace node1_src_rcolon v view t
    | State_whitespace -> act0 trace node0_whitespace view t
    | State_whitespace_bslash -> act0 trace node0_whitespace_bslash view t
    | State_hash_comment -> act0 trace node0_hash_comment view t
    | State_isubstring_start v -> act1 trace node1_isubstring_start v view t
    | State_isubstring_bslash v -> act1 trace node1_isubstring_bslash v view t
    | State_isubstring_bslash_u v -> act1 trace node1_isubstring_bslash_u v view t
    | State_isubstring_bslash_u_lcurly v -> act1 trace node1_isubstring_bslash_u_lcurly v view t
    | State_rditto_start -> act0 trace node0_rditto_start view t
    | State_spec_start -> act0 trace node0_spec_start view t
    | State_spec_lparen -> act0 trace node0_spec_lparen view t

  let rec transition trace state view t =
    match transition_of_state trace state view t with
    | t', Advance (view', state') -> begin
        if trace then
          File.Fmt.stdout |> Fmt.fmt " -> Advance (" |> View.pp view' |> Fmt.fmt ", "
          |> State.pp state' |> Fmt.fmt "), " |> pp t' |> Fmt.fmt "\n" |> ignore;
        transition trace state' view' t'
      end
    | t', Retry state' -> begin
        if trace then
          File.Fmt.stdout |> Fmt.fmt " -> Retry (" |> State.pp state' |> Fmt.fmt "), " |> pp t'
          |> Fmt.fmt "\n" |> ignore;
        transition trace state' view t'
      end
    | t', Accept token -> begin
        if trace then
          File.Fmt.stdout |> Fmt.fmt " -> Accept (" |> ConcreteToken.pp token |> Fmt.fmt "), "
          |> pp t' |> Fmt.fmt "\n" |> ignore;
        t', Accept token
      end

  let next ?(trace=false) state t =
    if trace then
      File.Fmt.stdout |> Fmt.fmt "Scan: start " |> State.pp state |> Fmt.fmt ", " |> pp t
      |> Fmt.fmt "\n" |> ignore;
    match transition trace state (View.of_t t) t with
    | _, Advance _ -> not_reached ()
    | _, Retry _ -> not_reached ()
    | t', Accept token -> t', token
end

module Dentation : sig
  val start: Text.Cursor.t -> t -> t * ConcreteToken.t
end = struct
  type paren_comment_lookahead_result =
    | LineExpr
    | LineNoop of t * ConcreteToken.t

  open AbstractToken
  let tok_indent = Tok_indent (Constant ())
  let tok_dedent = Tok_dedent (Constant ())
  let tok_indent_absent t =
    Tok_indent (malformed (malformation ~base:t.tok_base ~past:t.tok_base "Indent absent" t))
  let tok_dedent_absent t =
    Tok_dedent (malformed (
      malformation ~base:t.tok_base ~past:t.tok_base "Dedent absent" t))

  let rec next cursor t =
    match Text.Cursor.next_opt cursor with
    | None -> begin
        match t.line_state with
        | Line_dentation -> accept Tok_whitespace cursor t
        | Line_delim -> accept_dentation Tok_line_delim cursor t
        | Line_body -> not_reached ()
      end
    | Some (cp, cursor') -> begin
        match cp with
        | cp when Codepoint.(cp = of_char ' ') -> next cursor' t
        | cp when Codepoint.(cp = nl) -> accept_line_delim Tok_whitespace cursor' t
        | _ -> begin
            let col = Text.(Pos.col (Cursor.pos cursor)) in
            let level = col / 4L in
            let rem = col % 4L in
            (* The following patterns incrementally handle all dentation/alignment cases. Malformed
             * tokens are synthesized in error cases such that the cursor does not advance, but the
             * level is incrementally adjusted to converge. The overall result is that
             * Tok_indent/Tok_dedent nesting is always well formed. *)
            match rem, t.level, level with
            (* New expression at same level. *)
            | 0L, t_level, level when t_level = level -> begin
                match t.line_state with
                | Line_dentation -> Dfa.next State.start {t with line_state=Line_body}
                | Line_delim -> accept_dentation Tok_line_delim cursor t
                | Line_body -> not_reached ()
              end

            (* Continuation of expression at current level. *)
            | 2L, t_level, level when t_level = level ->
              accept_dentation Tok_whitespace cursor t

            (* New expression at higher level. *)
            | 0L, t_level, level when succ t_level = level ->
              accept_dentation tok_indent cursor {t with level}

            (* Continuation of expression at lower level. *)
            | 2L, t_level, level when t_level > succ level ->
              accept tok_dedent t.tok_base {t with level=pred t_level}
            | 2L, t_level, level when t_level = succ level ->
              accept_dentation tok_dedent cursor {t with level}

            (* New expression at lower level. *)
            | 0L, t_level, level when t_level > succ level ->
              accept tok_dedent t.tok_base {t with level=pred t_level}
            | 0L, t_level, level when t_level = succ level ->
              accept_dentation tok_dedent cursor {t with level}

            (* Off by one column at lower level. *)
            | 3L, t_level, level when t_level > succ level ->
              accept (tok_dedent_absent t) t.tok_base {t with level=pred t_level}
            | 1L, t_level, level when t_level > level ->
              accept (tok_dedent_absent t) t.tok_base {t with level=pred t_level}

            (* Off by one column at current level. *)
            | 3L, t_level, level when t_level = succ level ->
              accept_dentation Tok_misaligned cursor t
            | 1L, t_level, level when t_level = level ->
              accept_dentation Tok_misaligned cursor t

            (* Excess aligned indentation. *)
            | 0L, t_level, level when succ t_level < level ->
              accept (tok_indent_absent t) t.tok_base {t with level=succ t_level}
            (* Off by one column at higher level. *)
            | 3L, t_level, level when t_level < succ level ->
              accept (tok_indent_absent t) t.tok_base {t with level=succ t_level}
            | 1L, t_level, level when t_level < level ->
              accept (tok_indent_absent t) t.tok_base {t with level=succ t_level}

            (* Continuation of expression at higher level. *)
            | 2L, t_level, level when t_level < level ->
              accept (tok_indent_absent t) t.tok_base {t with level=succ t_level}

            | _ -> not_reached ()
          end
      end

  (* Lines comprising only whitespace and/or comments are ignored with regard to indentation.
   * Leading paren comments are problematic in that we must look ahead far enough to determine
   * whether the line contains an expression. While this could require looking ahead an arbitrary
   * number of tokens, in the overwhelmingly common case the leading paren comment is immediately
   * followed by line-delimiting whitespace. In the LineNoop case the leading paren comment only
   * gets scanned once, and therefore the line-delimiting whitespace token is the only token to be
   * scanned twice in the common case. *)
  let paren_comment_lookahead ppcursor pcursor cursor t =
    let rec fn t = begin
      let t', ctoken = Dfa.next State.start t in
      match ctoken.atoken, t'.line_state with
      | Tok_end_of_input, _
      | Tok_hash_comment, _
      | Tok_whitespace, Line_delim -> true
      | Tok_paren_comment _, _
      | Tok_whitespace, Line_dentation -> fn t'
      | Tok_whitespace, Line_body -> not_reached ()
      | _ -> false
    end in
    match paren_comment ppcursor pcursor cursor t with
    | t', ctoken -> begin
        match fn {t' with line_state=Line_dentation} with
        | false -> LineExpr
        | true -> LineNoop ({t' with line_state=Line_body}, ctoken)
      end

  let other cursor t =
    match t.level with
    | 0L -> begin
        match t.line_state with
        | Line_dentation -> Dfa.next State.start {t with line_state=Line_body}
        | Line_delim -> accept_dentation Tok_line_delim cursor t
        | Line_body -> not_reached ()
      end
    | 1L -> accept_dentation (Tok_dedent (Constant ())) cursor {t with level=0L}
    | _ -> accept (Tok_dedent (Constant ())) cursor {t with level=pred t.level}

  let start cursor t =
    match Text.Cursor.next_opt cursor with
    | None -> end_of_input cursor t
    | Some (cp, cursor') -> begin
        match cp with
        | cp when Codepoint.(cp = of_char ' ') -> next cursor' t
        | cp when Codepoint.(cp = nl) -> accept_line_delim Tok_whitespace cursor' t
        | cp when Codepoint.(cp = of_char '#') -> hash_comment cursor cursor cursor' t
        | cp when Codepoint.(cp = of_char '(') -> begin
            match Text.Cursor.next_opt cursor' with
            | None -> other cursor t
            | Some (cp, cursor'') -> begin
                match cp with
                | cp when Codepoint.(cp = of_char '*') -> begin
                    match paren_comment_lookahead cursor cursor' cursor'' t with
                    | LineExpr -> other cursor t
                    | LineNoop (t', ctoken) -> t', ctoken
                  end
                | _ -> other cursor t
              end
          end
        | _ -> other cursor t
      end
end

let next t =
  assert (Istring_expr_value t.tok_base |> (fun _ -> true)); (* XXX Remove. *)
  let trace = None in
  let _trace = Some true in
  match t.line_state, t.istring_state with
  | Line_dentation, _
  | Line_delim, _ -> Dentation.start t.tok_base t
  | Line_body, [] -> Dfa.next ?trace State.start t
  | Line_body, istring_state :: _ -> begin
      match istring_state with
      | Istring_interp -> Dfa.next ?trace State.isubstring_start t
      | Istring_spec_pct -> Dfa.next ?trace State.spec_start t
      | Istring_rditto -> Dfa.next ?trace State.rditto_start t
      | Istring_expr_width
      | _ -> not_implemented "XXX"
    end
