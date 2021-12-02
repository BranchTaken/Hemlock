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
    bias: sint;
    base: Text.Cursor.t;
    past: Text.Cursor.t
  }

  let init path bias base past =
    {path; bias; base; past}

  let path t =
    t.path

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
            (Sint.((Uns.bits_to_sint (Text.Pos.line text_pos)) + t.source.bias)) in
        Text.Pos.init ~line ~col:(Text.Pos.col text_pos)

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
    |> Fmt.fmt "; bias=" |> Sint.pp t.bias
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

      let init path bias ~base ~past description =
        let source = Source.init path bias base past in
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
  end
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

type istring_state =
  | Istring_interp (* Scanning interpolated string data. *)
  | Istring_spec (* Scanning format specifier. *)
  | Istring_expr (* Inside (^...^)-delimited expression. *)

type t = {
  path: string option;
  bias: sint;
  cursor: Text.Cursor.t;
  line_state: line_state;
  istring_state: istring_state list;
  level: uns;
}

let init text =
  {
    path=Text.path text;
    bias=Sint.zero;
    cursor=Text.Cursor.hd text;
    line_state=Line_dentation;
    istring_state=[];
    level=0L;
  }

let text t =
  Text.Cursor.container t.cursor

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
  let slice = Text.Slice.init ~base:t.cursor ~past:cursor (Text.Cursor.container cursor) in
  Text.Slice.to_string slice

let accept atoken cursor t =
  let source = Source.init t.path t.bias t.cursor cursor in
  {t with cursor}, (ConcreteToken.init atoken source)

let accept_incl atoken _ppcursor _pcursor cursor t =
  accept atoken cursor t

let accept_excl atoken _ppcursor pcursor _cursor t =
  accept atoken pcursor t

let accept_pexcl atoken ppcursor _pcursor _cursor t =
  accept atoken ppcursor t

let accept_istring_lditto _ppcursor _pcursor cursor t =
  let source = Source.init t.path t.bias t.cursor cursor in
  {t with cursor; istring_state=(Istring_interp :: t.istring_state)},
  (ConcreteToken.init Tok_istring_lditto source)

let accept_istring_pct cursor t =
  let source = Source.init t.path t.bias t.cursor cursor in
  {t with cursor; istring_state=Istring_spec :: (List.tl t.istring_state)},
  (ConcreteToken.init Tok_istring_pct source)

let accept_istring_rditto cursor t =
  let source = Source.init t.path t.bias t.cursor cursor in
  {t with cursor; istring_state=List.tl t.istring_state},
  (ConcreteToken.init Tok_istring_rditto source)

let accept_line_delim atoken cursor t =
  let source = Source.init t.path t.bias t.cursor cursor in
  {t with cursor; line_state=Line_delim}, (ConcreteToken.init atoken source)

let accept_line_delim_incl atoken _ppcursor _pcursor cursor t =
  accept_line_delim atoken cursor t

(* The scanner's directed acyclic subgraphs are expressed as DAGs of states with a unified state
 * transition driver. *)
module Dag : sig
  type action = Text.Cursor.t -> Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
  type eoi_action = Text.Cursor.t -> t -> t * ConcreteToken.t
  type state = {
    edges: (codepoint, action, Codepoint.cmper_witness) Map.t;
    eoi: eoi_action;
    default: action;
  }
  val act: state -> Text.Cursor.t -> Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
  val start: state -> t -> t * ConcreteToken.t
end = struct
  type action = Text.Cursor.t -> Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
  type eoi_action = Text.Cursor.t -> t -> t * ConcreteToken.t
  type state = {
    edges: (codepoint, action, Codepoint.cmper_witness) Map.t;
    eoi: eoi_action;
    default: action;
  }

  let act state _ppcursor pcursor cursor t =
    match Text.Cursor.next_opt cursor with
    | None -> state.eoi cursor t
    | Some (cp, cursor') -> begin
        match Map.get cp state.edges with
        | Some action' -> action' pcursor cursor cursor' t
        | None -> state.default pcursor cursor cursor' t
      end

  let start state t =
    act state t.cursor t.cursor t.cursor t
end

(***************************************************************************************************
 * Convenience routines for reporting malformations. *)

let malformation ~base ~past description t =
  AbstractToken.Rendition.Malformation.init t.path t.bias ~base ~past description

let malformed malformation =
  AbstractToken.Rendition.Malformed [malformation]

let invalid_unicode base past t =
  malformation ~base ~past "Invalid Unicode value" t

let illegal_backslash base past t =
  malformation ~base ~past "Illegal backslash escape" t

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

let whitespace _ppcursor _pcursor cursor t =
  let rec fn cursor t = begin
    match Text.Cursor.next_opt cursor with
    | None -> accept Tok_whitespace cursor t
    | Some (cp, cursor') -> begin
        match cp with
        | cp when Codepoint.(cp = of_char ' ') -> fn cursor' t
        | cp when Codepoint.(cp = of_char '\\') -> fn_bslash cursor cursor' t
        | cp when Codepoint.(cp = nl) -> accept_line_delim Tok_whitespace cursor' t
        | _ -> accept Tok_whitespace cursor t
      end
  end
  and fn_bslash pcursor cursor t = begin
    match Text.Cursor.next_opt cursor with
    | None -> accept Tok_whitespace pcursor t
    | Some (cp, cursor') -> begin
        match cp with
        | cp when Codepoint.(cp = nl) -> fn cursor' t
        | _ -> accept Tok_whitespace pcursor t
      end
  end in
  fn cursor t

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
      accept (Tok_paren_comment (malformed (unterminated_comment t.cursor cursor t))) cursor t
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
    let mal = (malformed (malformation ~base:t.cursor ~past:cursor
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
  let nat_to_cp_opt nat = begin
    match Nat.to_uns_opt nat with
    | None -> None
    | Some u -> Codepoint.narrow_of_uns_opt u
  end in
  match nat_to_cp_opt nat with
  | None -> accum_mal (invalid_unicode base past t) accum
  | Some cp -> accum_cp cp accum

module Codepoint_ : sig
  type umap =
    | UMapUscore
    | UMapDigit
    | UMapRcurly
    | UMapDitto
    | UMapTick

  val u_map: (Codepoint.t, umap, Codepoint.cmper_witness) Map.t
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
      let open AbstractToken.Rendition in
      match accum with
      | Empty -> not_reached ()
      | Cp cp -> accept (Tok_codepoint (Constant cp)) cursor t
      | Malformations mals -> accept (Tok_codepoint (Malformed (List.rev mals))) cursor t
    end in

    (* The callers of fn_wrapper have varying scanner state they're carrying as call parameters, so
     * in most cases they have to allocate a closure. This isn't ideal performance-wise, but it
     * reduces boilerplate. *)
    let fn_wrapper ~f accum cursor t = begin
      match Text.Cursor.next_opt cursor with
      | None -> begin
          let mal = unterminated_codepoint t.cursor cursor t in
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
            let mal = empty_codepoint t.cursor cursor' t in
            accept_codepoint (accum_mal mal accum) cursor' t
          end
        | Some LMapBslash -> fn_bslash cursor accum cursor' t
        | None -> fn_cp (Cp cp) cursor' t
      end
end

module String_ : sig
  val start_istring: t -> t * ConcreteToken.t
  val bstring: Text.Cursor.t -> Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
  val rstring: Text.Cursor.t -> Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
  val accept_unterminated_rstring: Text.Cursor.t -> t -> t * ConcreteToken.t
end = struct
  type accum =
    | Codepoints of codepoint list
    | Malformations of AbstractToken.Rendition.Malformation.t list

  let accum_cp cp = function
    | Codepoints cps -> Codepoints (cp :: cps)
    | (Malformations _) as mals -> mals

  let accum_mal mal = function
    | Codepoints _ -> Malformations [mal]
    | Malformations mals -> Malformations (mal :: mals)

  (* Interpolated substring: "..." *)
  let istring_interp _ppcursor _pcursor cursor t =
    let accept_isubstring accum cursor t = begin
      match accum with
      | Codepoints cps -> accept (Tok_isubstring (Constant (String.of_list_rev cps))) cursor t
      | Malformations mals -> accept (Tok_isubstring (Malformed (List.rev mals))) cursor t
    end in

    let rec fn_wrapper ~f accum cursor t = begin
      match Text.Cursor.nextv_opt cursor with
      | None -> begin
          let mal = unterminated_string t.cursor cursor t in
          let t' = match t.istring_state with
            | [] -> t
            | _ :: istring_state -> {t with istring_state}
          in
          accept_isubstring (accum_mal mal accum) cursor t'
        end
      | Some (_, false, cursor') -> begin
          let mal = invalid_utf8 cursor cursor' t in
          fn (accum_mal mal accum) cursor' t
        end
      | Some (cp, true, cursor') -> f accum cursor cp cursor' t
    end
    and fn_bslash_u_lcurly nat bslash_cursor accum cursor t = begin
      fn_wrapper accum cursor t ~f:(fun accum cursor cp cursor' t ->
        match Map.get cp Codepoint_.u_map with
        | Some UMapUscore -> fn_bslash_u_lcurly nat bslash_cursor accum cursor' t
        | Some UMapDigit ->
          fn_bslash_u_lcurly Radix.(nat_accum (nat_of_cp cp) nat Hex) bslash_cursor accum cursor' t
        | Some UMapRcurly ->
          fn (accum_cp_of_nat ~accum_cp ~accum_mal nat accum bslash_cursor cursor t) cursor' t
        | Some UMapDitto -> begin
            let mal = partial_unicode bslash_cursor cursor t in
            accept_isubstring (accum_mal mal accum) cursor t
          end
        | Some UMapTick
        | None -> begin
            let mal = partial_unicode bslash_cursor cursor t in
            fn (accum_mal mal accum) cursor t
          end
      )
    end
    and fn_bslash_u bslash_cursor accum cursor t = begin
      fn_wrapper accum cursor t ~f:(fun accum cursor cp cursor' t ->
        match cp with
        | cp when Codepoint.(cp = of_char '{') ->
          fn_bslash_u_lcurly Nat.zero bslash_cursor accum cursor' t
        | cp when Codepoint.(cp = of_char '"') -> begin
            let mal = illegal_backslash bslash_cursor cursor t in
            accept_isubstring (accum_mal mal accum) cursor t
          end
        | _ -> begin
            let mal = illegal_backslash bslash_cursor cursor t in
            fn (accum_mal mal accum) cursor t
          end
      )
    end
    and fn_bslash bslash_cursor accum cursor t = begin
      fn_wrapper accum cursor t ~f:(fun accum _cursor cp cursor' t ->
        match cp with
        | cp when Codepoint.(cp = of_char 'u') -> fn_bslash_u bslash_cursor accum cursor' t
        | cp when Codepoint.(cp = of_char 't') -> fn (accum_cp Codepoint.ht accum) cursor' t
        | cp when Codepoint.(cp = of_char 'n') -> fn (accum_cp Codepoint.nl accum) cursor' t
        | cp when Codepoint.(cp = of_char 'r') -> fn (accum_cp Codepoint.cr accum) cursor' t
        | cp when Codepoint.(cp = of_char '"') -> fn (accum_cp cp accum) cursor' t
        | cp when Codepoint.(cp = of_char '\\') -> fn (accum_cp cp accum) cursor' t
        | cp when Codepoint.(cp = of_char '%') -> fn (accum_cp cp accum) cursor' t
        | _ -> begin
            let mal = illegal_backslash bslash_cursor cursor' t in
            fn (accum_mal mal accum) cursor' t
          end
      )
    end
    and fn accum cursor t = begin
      fn_wrapper accum cursor t ~f:(fun accum cursor cp cursor' t ->
        match cp with
        | cp when Codepoint.(cp = of_char '\\') -> fn_bslash cursor accum cursor' t
        | cp when Codepoint.(cp = of_char '%') -> begin
            match Text.Cursor.(t.cursor = cursor) with
            | false -> accept_isubstring accum cursor t
            | true -> accept_istring_pct cursor' t
          end
        | cp when Codepoint.(cp = of_char '"') -> begin
            match Text.Cursor.(t.cursor = cursor) with
            | false -> accept_isubstring accum cursor t
            | true -> accept_istring_rditto cursor' t
          end
        | _ -> fn (accum_cp cp accum) cursor' t
      )
    end in
    fn (Codepoints []) cursor t

  let start_istring t =
    match t.istring_state with
    | Istring_interp :: _ -> istring_interp t.cursor t.cursor t.cursor t
    | Istring_spec :: _ -> not_implemented "XXX"
    | Istring_expr :: _
    | [] -> not_reached ()

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
    accept (Tok_rstring (malformed (unterminated_string t.cursor cursor t))) cursor t

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
          accept (Tok_rstring (Malformed mals)) cursor t
        end
      | _, Malformations body_mals, _ -> begin
          let mals = List.(rev_concat ltag.mals (rev_concat body_mals (rev rtag.mals))) in
          accept (Tok_rstring (Malformed mals)) cursor t
        end
    end in

    let rec fn_rtag rtag_accum ltag_cursor body_accum saved_body_accum ltag cursor t = begin
      match Text.Cursor.nextv_opt cursor with
      | None -> begin
          let mal = unterminated_string t.cursor cursor t in
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
          let mal = unterminated_string t.cursor cursor t in
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
          let mal = unterminated_string t.cursor cursor t in
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
      | Malformations mals -> accept (Tok_bstring (Malformed (List.rev mals))) cursor t
    end in

    let rec fn_lpad c0_cursor accum lmargin cursor t = begin
      match Text.Cursor.nextv_opt cursor with
      | None -> begin
          let mal = unterminated_string t.cursor cursor t in
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
          let mal = unterminated_string t.cursor cursor t in
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
      let malformed = Rendition.Malformed (List.rev mals) in
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
                | None -> malformed (out_of_range_real t.cursor cursor t)
              )
            end
          | Subtype_r64 -> begin
              Tok_r64 (
                match Realer.to_r64_opt realer with
                | Some r -> (Constant r)
                | None -> malformed (out_of_range_real t.cursor cursor t)
              )
            end
        in
        accept tok cursor t
      end
    | R _, Dec -> not_reached ()
    | R_dec, Dec -> begin
        let r = Real.of_string Text.Slice.(to_string (init ~base:t.cursor
            ~past:suffix_cursor (Text.Cursor.container t.cursor))) in
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
    next_frac R_dec t.cursor dec_frac_map Dec pcursor t

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
      let malformed = Rendition.Malformed (List.rev mals) in
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
            let mal = out_of_range_int radix limit t.cursor cursor t in
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
    next_whole accum t.cursor dec_whole_map Dec cursor t

  let hex n _ppcursor _pcursor cursor t =
    let accum = N n in
    next_whole accum cursor hex_whole_map Hex cursor t

  let mal_ident _ppcursor pcursor cursor t =
    let mal = invalid_numerical pcursor cursor t in
    let accum = Malformations [mal] in
    next_whole accum t.cursor dec_whole_map Dec cursor t

  let zero_u_suffix _ppcursor pcursor cursor t =
    let accum = N Nat.k_0 in
    next_suffix Nat.k_0 Unsigned pcursor accum Dec cursor t

  let zero_i_suffix _ppcursor pcursor cursor t =
    let accum = N Nat.k_0 in
    next_suffix Nat.k_0 Signed pcursor accum Dec cursor t
end

let end_of_input cursor t =
  match t.line_state, t.level with
  | Line_delim, 0L -> accept_dentation Tok_line_delim cursor t
  | _, 0L -> accept Tok_end_of_input cursor t
  | _ -> accept_dentation (Tok_dedent (Constant ())) cursor {t with level=Uns.pred t.level}

let start_default = Dag.{
  edges=(map_of_cps_alist [
    (",", (accept_incl Tok_comma));
    (";", (act {
        edges=(map_of_cps_alist [
          (";", (accept_incl Tok_semi_semi));
        ]);
        eoi=(accept Tok_semi);
        default=(accept_excl Tok_semi);
      }));
    ("(", (act {
        edges=Map.singleton (module Codepoint) ~k:(Codepoint.of_char '*') ~v:paren_comment;
        eoi=(accept Tok_lparen);
        default=(accept_excl Tok_lparen);
      }));
    (")", (accept_incl Tok_rparen));
    ("[", (act {
        edges=Map.singleton (module Codepoint) ~k:(Codepoint.of_char '|') ~v:(accept_incl
            Tok_larray);
        eoi=(accept Tok_lbrack);
        default=(accept_excl Tok_lbrack);
      }));
    ("]", (accept_incl Tok_rbrack));
    ("{", (act {
        edges=Map.singleton (module Codepoint) ~k:(Codepoint.of_char '|') ~v:(accept_incl
            Tok_lmodule);
        eoi=(accept Tok_lcurly);
        default=(accept_excl Tok_lcurly);
      }));
    ("}", (accept_incl Tok_rcurly));
    ("\\", (act {
        edges=(map_of_cps_alist [
          ("\n", whitespace);
        ]);
        eoi=(accept Tok_bslash);
        default=(accept_excl Tok_bslash);
      }));
    ("&", (accept_incl Tok_amp));
    ("!", (accept_incl Tok_xmark));
    ("\n", (accept_line_delim_incl Tok_whitespace));
    ("~", (act {
        edges=(map_of_cps_alist [
          (operator_cps, (operator (fun s -> Tok_tilde_op s)));
        ]);
        eoi=(accept Tok_tilde);
        default=(accept_excl Tok_tilde);
      }));
    ("?", (act {
        edges=(map_of_cps_alist [
          (operator_cps, (operator (fun s -> Tok_qmark_op s)));
        ]);
        eoi=(accept Tok_qmark);
        default=(accept_excl Tok_qmark);
      }));
    ("*", (act {
        edges=(map_of_cps_alist [
          ("*", (operator (fun s -> Tok_star_star_op s)));
          ("-+/%@^$<=>|:.~?", (operator (fun s -> Tok_star_op s)));
        ]);
        eoi=(accept (Tok_star_op "*"));
        default=(accept_excl (Tok_star_op "*"));
      }));
    ("/", (operator (fun s -> Tok_slash_op s)));
    ("%", (operator (fun s -> Tok_pct_op s)));
    ("+", (operator (fun s -> Tok_plus_op s)));
    ("-", (operator (fun s -> Tok_minus_op s)));
    ("@", (operator (fun s -> Tok_at_op s)));
    ("^", (operator (fun s -> Tok_caret_op s)));
    ("$", (operator (fun s -> Tok_dollar_op s)));
    ("<", (operator (fun s -> Tok_lt_op s)));
    ("=", (operator (fun s -> Tok_eq_op s)));
    (">", (operator (fun s -> Tok_gt_op s)));
    ("|", (act {
        edges=(map_of_cps_alist [
          ("]", (accept_incl Tok_rarray));
          ("}", (accept_incl Tok_rmodule));
          (operator_cps, (operator (fun s -> Tok_bar_op s)));
        ]);
        eoi=(accept Tok_bar);
        default=(accept_excl Tok_bar);
      }));
    (":", (operator (fun s -> Tok_colon_op s)));
    (".", (operator (fun s -> Tok_dot_op s)));
    (" ", whitespace);
    ("#", hash_comment);
    ("_", (act {
        edges=(map_of_cps_alist [
          ("abcdefghijklmnopqrstuvwxyz0123456789'", uident);
          ("ABCDEFGHIJKLMNOPQRSTUVWXYZ", cident);
          ("_", uscore_ident);
        ]);
        eoi=(accept Tok_uscore);
        default=(accept_excl Tok_uscore);
      }));
    ("abcdefghijklmnopqrstuvwxyz", uident);
    ("ABCDEFGHIJKLMNOPQRSTUVWXYZ", cident);
    ("'", Codepoint_.codepoint);
    ("\"", accept_istring_lditto);
    ("`", (act {
        edges=Map.singleton (module Codepoint) ~k:(Codepoint.of_char '|') ~v:String_.bstring;
        eoi=String_.accept_unterminated_rstring;
        default=String_.rstring;
      }));
    ("0", (act {
        edges=(map_of_cps_alist [
          ("0_", Integer.(dec Nat.k_0));
          ("1", Integer.(dec Nat.k_1));
          ("2", Integer.(dec Nat.k_2));
          ("3", Integer.(dec Nat.k_3));
          ("4", Integer.(dec Nat.k_4));
          ("5", Integer.(dec Nat.k_5));
          ("6", Integer.(dec Nat.k_6));
          ("7", Integer.(dec Nat.k_7));
          ("8", Integer.(dec Nat.k_8));
          ("9", Integer.(dec Nat.k_9));
          ("b", Integer.(bin Nat.k_0));
          ("o", Integer.(oct Nat.k_0));
          ("x", Integer.(hex Nat.k_0));
          ("u", Integer.zero_u_suffix);
          ("i", Integer.zero_i_suffix);
          ("r", Real.zero_r_suffix);
          (".", (act {
              edges=(map_of_cps_alist [
                (operator_cps, (accept_pexcl Integer.zero));
              ]);
              eoi=(accept Real.zero);
              default=(Real.zero_frac);
            }));
          ("e", Real.zero_exp);
          ("ABCDEFGHIJKLMNOPQRSTUVWXYZacdfghjklmnpqstvwyz'", Integer.mal_ident);
        ]);
        eoi=(accept Integer.zero);
        default=(accept_excl Integer.zero);
      }));
    ("1", Integer.(dec Nat.k_1));
    ("2", Integer.(dec Nat.k_2));
    ("3", Integer.(dec Nat.k_3));
    ("4", Integer.(dec Nat.k_4));
    ("5", Integer.(dec Nat.k_5));
    ("6", Integer.(dec Nat.k_6));
    ("7", Integer.(dec Nat.k_7));
    ("8", Integer.(dec Nat.k_8));
    ("9", Integer.(dec Nat.k_9));
  ]);
  eoi=end_of_input;
  default=(accept_incl Tok_error);
}

let start t =
  assert Stdlib.(match t.istring_state with [] -> true | hd :: _ -> hd <> Istring_spec); (* XXX Remove. *)
  assert Stdlib.(match t.istring_state with [] -> true | hd :: _ -> hd <> Istring_expr); (* XXX Remove. *)
  match t.istring_state with
  | []
  | Istring_expr :: _ -> Dag.start start_default t
  | (Istring_interp|Istring_spec) :: _ -> String_.start_istring t

module LineDirective : sig
  val start: Text.Cursor.t -> t -> t * ConcreteToken.t option
end = struct
  let digits_1_9 = set_of_cps "123456789"

  type trailing_digit_map =
    | LDMapDigit
    | LDMapSpace
    | LDMapNewline

  let trailing_digit_map = map_of_cps_alist [
    ("0123456789", LDMapDigit);
    (" ", LDMapSpace);
    ("\n", LDMapNewline);
  ]

  let accept_error cursor t =
    let t', tok = accept Tok_error cursor t in
    t', Some tok

  let rec error cursor t =
    match Text.Cursor.next_opt cursor with
    | None -> accept_error cursor t
    | Some (cp, cursor') when Codepoint.(cp = nl) -> accept_error cursor' t
    | Some (_, cursor') -> error cursor' t

  let accept_directive ?path n cursor t =
    match Nat.to_int_opt n with
    | Some ni -> begin
        let path = match path with
          | None -> t.path
          | Some _ -> path
        in
        let bias = Sint.(ni - (Uns.bits_to_sint (Text.(Pos.line (Cursor.pos cursor))))) in
        {t with path; bias; cursor}, None
      end
    | None -> accept_error cursor t

  let path_finish path n cursor t =
    match Text.Cursor.next_opt cursor with
    | None -> accept_error cursor t
    | Some (cp, cursor') when Codepoint.(cp = nl) -> accept_directive ~path n cursor' t
    | Some (_, cursor') -> error cursor' t

  let path_start n cursor t =
    (* Scan using the normal token scanner, and extract the path from the [Tok_istring_lditto;
     * Tok_isubstring; Tok_istring_rditto] token sequence. If all goes well, path_finish does the
     * final work of accepting the line directive token; the three tokens accepted while extracting
     * the path never propagate out of this function. *)
    let t_space = {t with cursor} in
    let t_lditto, lditto = start t_space in
    match ConcreteToken.atoken lditto with
    | Tok_istring_lditto -> begin
        let t_path, path_tok = start t_lditto in
        match ConcreteToken.atoken path_tok with
        | Tok_isubstring (Constant path) -> begin
            let t_rditto, rditto = start t_path in
            match ConcreteToken.atoken rditto with
            | Tok_istring_rditto -> path_finish path n t_rditto.cursor t
            | _ -> error cursor t
          end
        | _ -> error cursor t
      end
    | _ -> error cursor t

  let rec n_cont n cursor t =
    match Text.Cursor.next_opt cursor with
    | None -> accept_error cursor t
    | Some (cp, cursor') -> begin
        match Map.get cp trailing_digit_map  with
        | Some LDMapDigit -> n_cont Nat.(n * k_a + (nat_of_cp cp)) cursor' t
        | Some LDMapSpace -> path_start n cursor' t
        | Some LDMapNewline -> accept_directive n cursor' t
        | None -> error cursor' t
      end

  let start cursor t =
    match Text.Cursor.next_opt cursor with
    | None -> accept_error cursor t
    | Some (cp, cursor') -> begin
        match Set.mem cp digits_1_9 with
        | true -> n_cont (nat_of_cp cp) cursor' t
        | false -> error cursor' t
      end
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
    Tok_indent (malformed (malformation ~base:t.cursor ~past:t.cursor "Indent absent" t))
  let tok_dedent_absent t =
    Tok_dedent (malformed (
      malformation ~base:t.cursor ~past:t.cursor "Dedent absent" t))

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
                | Line_dentation -> start {t with line_state=Line_body}
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
              accept tok_dedent t.cursor {t with level=pred t_level}
            | 2L, t_level, level when t_level = succ level ->
              accept_dentation tok_dedent cursor {t with level}

            (* New expression at lower level. *)
            | 0L, t_level, level when t_level > succ level ->
              accept tok_dedent t.cursor {t with level=pred t_level}
            | 0L, t_level, level when t_level = succ level ->
              accept_dentation tok_dedent cursor {t with level}

            (* Off by one column at lower level. *)
            | 3L, t_level, level when t_level > succ level ->
              accept (tok_dedent_absent t) t.cursor {t with level=pred t_level}
            | 1L, t_level, level when t_level > level ->
              accept (tok_dedent_absent t) t.cursor {t with level=pred t_level}

            (* Off by one column at current level. *)
            | 3L, t_level, level when t_level = succ level ->
              accept_dentation Tok_misaligned cursor t
            | 1L, t_level, level when t_level = level ->
              accept_dentation Tok_misaligned cursor t

            (* Excess aligned indentation. *)
            | 0L, t_level, level when succ t_level < level ->
              accept (tok_indent_absent t) t.cursor {t with level=succ t_level}
            (* Off by one column at higher level. *)
            | 3L, t_level, level when t_level < succ level ->
              accept (tok_indent_absent t) t.cursor {t with level=succ t_level}
            | 1L, t_level, level when t_level < level ->
              accept (tok_indent_absent t) t.cursor {t with level=succ t_level}

            (* Continuation of expression at higher level. *)
            | 2L, t_level, level when t_level < level ->
              accept (tok_indent_absent t) t.cursor {t with level=succ t_level}

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
      let t', ctoken = start t in
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
        | Line_dentation -> start {t with line_state=Line_body}
        | Line_delim -> accept_dentation Tok_line_delim cursor t
        | Line_body -> not_reached ()
      end
    | 1L -> accept_dentation (Tok_dedent (Constant ())) cursor {t with level=0L}
    | _ -> accept (Tok_dedent (Constant ())) cursor {t with level=pred t.level}

  let rec start cursor t =
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
        | cp when Codepoint.(cp = of_char ':') -> begin
            let t', tok_opt = LineDirective.start cursor' t in
            match tok_opt with
            | None -> start t'.cursor t'
            | Some tok -> t', tok
          end
        | _ -> other cursor t
      end
end

let next t =
  match t.line_state with
  | Line_dentation
  | Line_delim -> Dentation.start t.cursor t
  | Line_body -> start t
