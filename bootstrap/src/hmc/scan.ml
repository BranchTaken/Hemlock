open Basis
open Basis.Rudiments

let operator_cps = "-+*/%@^$<=>|:.~?"

let bin_cps = "01"
let oct_cps = String.concat [bin_cps; "234567"]
let dec_cps = String.concat [oct_cps; "89"]
let hex_cps = String.concat [dec_cps; "abcdef"]

let ident_cident_cps = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let ident_uident_cps = "abcdefghijklmnopqrstuvwxyz"
let ident_continue_cps = String.concat [dec_cps; "'"]
let ident_cps = String.concat ["_"; ident_cident_cps; ident_uident_cps; ident_continue_cps]

let whitespace_cps = " \n"

let cpset_of_cps cps =
  String.fold ~init:(Set.empty (module Codepoint)) ~f:(fun set cp ->
    Set.insert cp set
  ) cps

let map_of_cpsets_alist alist =
  List.fold ~init:(Map.empty (module Codepoint)) ~f:(fun edges (cpset, v) ->
    Set.fold ~init:edges ~f:(fun edges cp ->
      Map.insert_hlt ~k:cp ~v edges
    ) cpset
  ) alist

let map_of_cps_alist alist =
  map_of_cpsets_alist (List.map ~f:(fun (cps, v) -> cpset_of_cps cps, v) alist)

let nat_digit_map = String.foldi ~init:(Map.empty (module Codepoint))
  ~f:(fun i digit_map cp ->
    Map.insert_hlt ~k:cp ~v:(Nat.of_uns i) digit_map
  ) hex_cps

let nat_of_cp digit =
  Map.get_hlt digit nat_digit_map

let zint_digit_map = String.foldi ~init:(Map.empty (module Codepoint))
  ~f:(fun i digit_map cp ->
    Map.insert_hlt ~k:cp ~v:(Zint.of_uns i) digit_map
  ) hex_cps

let zint_of_cp digit =
  Map.get_hlt digit zint_digit_map

module Radix = struct
  type t =
    | Bin
    | Oct
    | Dec
    | Hex

  let pp t formatter =
    formatter
    |> Fmt.fmt (match t with
      | Bin -> "Bin"
      | Oct -> "Oct"
      | Dec -> "Dec"
      | Hex -> "Hex"
    )

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

module AbstractToken = struct
  module Rendition = struct
    module Malformation = struct
      type t = {
        source: Source.Slice.t;
        description: string;
      }

      let cmp t0 t1 =
        Source.Slice.cmp t0.source t1.source

      let init ~base ~past ~description =
        {source=Source.Slice.of_cursors ~base ~past; description}

      let source t =
        t.source

      let description t =
        t.description

      let pp t formatter =
        formatter
        |> Fmt.fmt "\""
        |> Source.Slice.pp t.source
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

  type indent_omit = {
    indent: uns;
    omit: uns;
  }

  let pp_indent_omit {indent; omit} formatter =
    formatter
    |> Fmt.fmt "{indent=" |> Uns.pp indent
    |> Fmt.fmt "; omit=" |> Uns.pp omit
    |> Fmt.fmt "}"

  type source_directive = {
    path: string option;
    line: uns option;
    io: indent_omit option;
  }

  let pp_source_directive {path; line; io} formatter =
    formatter
    |> Fmt.fmt "{path=" |> (Option.pp String.pp) path
    |> Fmt.fmt "; line=" |> (Option.pp Uns.pp) line
    |> Fmt.fmt "; io=" |> (Option.pp pp_indent_omit) io
    |> Fmt.fmt "}"

  type t =
    (* Keywords. *)
    | Tok_and
    | Tok_also
    | Tok_as
    | Tok_conceal
    | Tok_effect
    | Tok_else
    | Tok_expose
    | Tok_external
    | Tok_false
    | Tok_fn
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
    | Tok_lcapture
    | Tok_rcapture
    | Tok_larray
    | Tok_rarray
    | Tok_bslash
    | Tok_tick
    | Tok_caret
    | Tok_amp
    | Tok_xmark
    | Tok_arrow
    | Tok_carrow

    | Tok_source_directive of source_directive Rendition.t
    | Tok_line_delim
    | Tok_indent of unit Rendition.t
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
    | Tok_nat of Nat.t Rendition.t
    | Tok_zint of Nat.t Rendition.t
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
      | Tok_conceal -> formatter |> Fmt.fmt "Tok_conceal"
      | Tok_effect -> formatter |> Fmt.fmt "Tok_effect"
      | Tok_else -> formatter |> Fmt.fmt "Tok_else"
      | Tok_expose -> formatter |> Fmt.fmt "Tok_expose"
      | Tok_external -> formatter |> Fmt.fmt "Tok_external"
      | Tok_false -> formatter |> Fmt.fmt "Tok_false"
      | Tok_fn -> formatter |> Fmt.fmt "Tok_fn"
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
      | Tok_lcapture -> formatter |> Fmt.fmt "Tok_lcapture"
      | Tok_rcapture -> formatter |> Fmt.fmt "Tok_rcapture"
      | Tok_larray -> formatter |> Fmt.fmt "Tok_larray"
      | Tok_rarray -> formatter |> Fmt.fmt "Tok_rarray"
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
      | Tok_line_delim -> formatter |> Fmt.fmt "Tok_line_delim"
      | Tok_indent rendition -> formatter |> Rendition.pp_unit "Tok_indent" rendition
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
      | Tok_nat rendition ->
        formatter |> Fmt.fmt "Tok_nat=" |> (Rendition.pp Nat.pp) rendition
      | Tok_zint rendition ->
        formatter |> Fmt.fmt "Tok_zint=" |> (Rendition.pp Nat.pp) rendition
      | Tok_end_of_input -> formatter |> Fmt.fmt "Tok_end_of_input"
      | Tok_misaligned -> formatter |> Fmt.fmt "Tok_misaligned"
      | Tok_error -> formatter |> Fmt.fmt "Tok_error"
    )
    |> Fmt.fmt ">"
end

module ConcreteToken = struct
  type t = {
    atoken: AbstractToken.t;
    source: Source.Slice.t;
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
    |> Fmt.fmt "; source=" |> Source.Slice.pp t.source
    |> Fmt.fmt "}"
end

module View = struct
  type t = {
    ppcursor: Source.Cursor.t;
    pcursor: Source.Cursor.t;
    cursor: Source.Cursor.t;
  }

  let init ~ppcursor ~pcursor ~cursor =
    {ppcursor; pcursor; cursor}

  let pp {ppcursor; pcursor; cursor} formatter =
    formatter
    |> Fmt.fmt "{ppcursor=" |> Text.Pos.pp (Source.Cursor.pos ppcursor)
    |> Fmt.fmt "; pcursor=" |> Text.Pos.pp (Source.Cursor.pos pcursor)
    |> Fmt.fmt "; cursor=" |> Text.Pos.pp (Source.Cursor.pos cursor)
    |> Fmt.fmt "}"

  let next {pcursor; cursor; _} =
    match Source.Cursor.next_opt cursor with
    | None -> None
    | Some (cp, cursor') -> Some (cp, {ppcursor=pcursor; pcursor=cursor; cursor=cursor'})
end

module Level = struct
  type t =
    | Primary of uns
    | Embedded of {
        primary: uns;
        embedded: uns;
      }

  let pp t formatter =
    match t with
    | Primary primary -> formatter |> Fmt.fmt "Primary " |> Uns.pp primary
    | Embedded {primary; embedded} -> begin
        formatter
        |> Fmt.fmt "Embedded {primary=" |> Uns.pp primary
        |> Fmt.fmt "; embedded=" |> Uns.pp embedded
        |> Fmt.fmt "}"
      end

  let level = function
    | Primary primary -> primary
    | Embedded {primary=_; embedded} -> embedded

  let cmp t0 t1 =
    Uns.cmp (level t0) (level t1)

  let ( = ) t0 t1 =
    Cmp.is_eq (cmp t0 t1)

  let init u =
    Primary u

  let update u = function
    | Primary _ -> Primary u
    | Embedded e -> Embedded {e with embedded=u}

  let succ t =
    update (succ (level t)) t

  let pred t =
    update (pred (level t)) t

  let embed u = function
    | Primary primary -> Embedded {primary; embedded=u}
    | Embedded e -> Embedded {e with embedded=u}

  let reset = function
    | Primary _ as level -> level
    | Embedded {primary; embedded=_} -> Primary primary
end

(* Dentation code block state. The scanner needs to track whether any expressions have been scanned
 * in the current block. *)
type block_state =
  (* At beginning of block, no non-whitespace/comment tokens yet scanned. *)
  | Block_primal

  (* At least one non-whitespace/comment token scanned in block. *)
  | Block_nonempty

let pp_block_state block_state formatter =
  match block_state with
  | Block_primal -> formatter |> Fmt.fmt "Block_primal"
  | Block_nonempty -> formatter |> Fmt.fmt "Block_nonempty"

(* Dentation line state. The scanner switches to a dentation scanning mode at the beginning of each
 * line and accepts synthetic tokens while scanning leading whitespace,
 * Tok_{line_delim,indent,dedent}, as syntactic analogues to Tok_{semi_semi,lparen,rparen}. *)
type line_state =
  (* At beginning of line (column 0). Dentation adjustment may be required. *)
  | Line_begin

  (* Just after leading whitespace token. Dentation adjustment may be required. *)
  | Line_whitespace

  (* First non-whitspace is a paren comment or bslash-nl continuation starting at specified column;
   * no non-whitespace/comment tokens scanned yet, but subsequent whitespace and/or paren comments
   * may have been scanned. Dentation adjustment may be required, but if so, the specified starting
   * column is used rather than that of the first non-whitespace/comment token. *)
  | Line_start_col of uns

  (* Normal (non-dentation) scanning mode. *)
  | Line_body

let pp_line_state line_state formatter =
  match line_state with
  | Line_begin -> formatter |> Fmt.fmt "Line_begin"
  | Line_whitespace -> formatter |> Fmt.fmt "Line_whitespace"
  | Line_start_col col -> formatter |> Fmt.fmt "Line_start_col " |> Uns.pp col
  | Line_body -> formatter |> Fmt.fmt "Line_body"

(* istring_state determines what starting state to feed to Dfa.next. States may be skipped, e.g. if
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
  | Istring_expr_value of Source.Cursor.t (* Cursor is start of value expression to be stringified. *)
  | Istring_rditto

let pp_istring_state istring_state formatter =
  match istring_state with
  | Istring_interp -> formatter |> Fmt.fmt "Istring_interp"
  | Istring_spec_pct -> formatter |> Fmt.fmt "Istring_spec_pct"
  | Istring_expr_width -> formatter |> Fmt.fmt "Istring_expr_width"
  | Istring_expr_value cursor ->
    formatter |> Fmt.fmt "Istring_exp_value " |> Source.Cursor.pp cursor
  | Istring_rditto -> formatter |> Fmt.fmt "Istring_rditto"

type t = {
  tok_base: Source.Cursor.t;
  level: Level.t;
  block_state: block_state;
  line_state: line_state;
  istring_state: istring_state list;
}

let init text =
  {
    tok_base=Source.Cursor.hd (Source.init text);
    level=Level.init 0L;
    block_state=Block_primal;
    line_state=Line_begin;
    istring_state=[];
  }

let pp t formatter =
  formatter
  |> Fmt.fmt "{tok_base=" |> Text.Pos.pp (Source.Cursor.pos t.tok_base)
  |> Fmt.fmt "; level=" |> Level.pp t.level
  |> Fmt.fmt "; block_state=" |> pp_block_state t.block_state
  |> Fmt.fmt "; line_state=" |> pp_line_state t.line_state
  |> Fmt.fmt "; istring_state=" |> (List.pp pp_istring_state) t.istring_state
  |> Fmt.fmt "}"

let view_of_t t =
  View.init ~ppcursor:t.tok_base ~pcursor:t.tok_base ~cursor:t.tok_base

let source_at cursor t =
  Source.Slice.of_cursors ~base:t.tok_base ~past:cursor

let text t =
  Source.(text (Cursor.container t.tok_base))

let str_of_cursor cursor t =
  Source.Slice.to_string (Source.Slice.of_cursors ~base:t.tok_base ~past:cursor)

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

(***************************************************************************************************
 * Convenience routines for reporting malformations. *)

let malformation ~base ~past description =
  AbstractToken.Rendition.Malformation.init ~base ~past ~description

let malformed malformation =
  AbstractToken.Rendition.of_mals [malformation]

let unexpected_codepoint_source_directive base past =
  malformation ~base ~past "Unexpected codepoint in source directive"

let unterminated_source_directive base past =
  malformation ~base ~past "Unterminated source directive"

let invalid_unicode base past =
  malformation ~base ~past "Invalid Unicode value"

let illegal_backslash base past =
  malformation ~base ~past "Illegal backslash escape"

let missing_backslash base past =
  malformation ~base ~past "Missing backslash escape"

let invalid_unicode_escape base past =
  malformation ~base ~past "Invalid \\u{...}"

let empty_codepoint base past =
  malformation ~base ~past "Empty codepoint literal"

let excess_codepoint base past =
  malformation ~base ~past "Excess codepoint before terminator"

let unterminated_comment base past =
  malformation ~base ~past "Unterminated comment"

let unterminated_codepoint base past =
  malformation ~base ~past "Unterminated codepoint literal"

let unterminated_string base past =
  malformation ~base ~past "Unterminated string literal"

let invalid_utf8 base past =
  malformation ~base ~past "Invalid UTF-8 encoding"

let invalid_bar_indent base past =
  malformation ~base ~past "Invalid bar string indentation"

let invalid_numerical base past =
  malformation ~base ~past "Invalid codepoint in numerical constant"

let invalid_type_suffix_leading_zero base past =
  malformation ~base ~past "Leading zero in numerical constant type suffix"

let unsupported_bitwidth base past =
  malformation ~base ~past "Unsupported bitwidth in numerical constant"

let out_of_range_int radix limit base past =
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
  malformation ~base ~past description

let out_of_range_real base past =
  malformation ~base ~past "Numerical constant cannot be precisely represented"

(**************************************************************************************************)

module String_ : sig
  val bstring: Source.Cursor.t -> Source.Cursor.t -> Source.Cursor.t -> t -> t * ConcreteToken.t
  val rstring: Source.Cursor.t -> Source.Cursor.t -> Source.Cursor.t -> t -> t * ConcreteToken.t
  val accept_unterminated_rstring: Source.Cursor.t -> t -> t * ConcreteToken.t
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
    accept (Tok_rstring (malformed (unterminated_string t.tok_base cursor))) cursor t

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
      match Source.Cursor.nextv_opt cursor with
      | None -> begin
          let mal = unterminated_string t.tok_base cursor in
          let rtag = tag_of_accum (tag_accum_mal mal rtag_accum) in
          accept_rstring rtag saved_body_accum ltag cursor t
        end
      | Some (_, false, cursor') -> begin
          let mal = invalid_utf8 cursor cursor' in
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
      match Source.Cursor.nextv_opt cursor with
      | None -> begin
          let mal = unterminated_string t.tok_base cursor in
          let body_accum' = accum_mal mal body_accum in
          let rtag = tag_of_accum tag_accum_empty in
          accept_rstring rtag body_accum' ltag cursor t
        end
      | Some (_, false, cursor') -> begin
          let mal = invalid_utf8 cursor cursor' in
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
      match Source.Cursor.nextv_opt cursor with
      | None -> begin
          let mal = unterminated_string t.tok_base cursor in
          let ltag_accum' = tag_accum_mal mal ltag_accum in
          let ltag = tag_of_accum ltag_accum' in
          let body_accum = Codepoints [] in
          let rtag = tag_of_accum tag_accum_empty in
          accept_rstring rtag body_accum ltag cursor t
        end
      | Some (_, false, cursor') -> begin
          let mal = invalid_utf8 cursor cursor' in
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
      match Source.Cursor.nextv_opt cursor with
      | None -> begin
          let mal = unterminated_string t.tok_base cursor in
          accept_bstring (accum_mal mal accum) cursor t
        end
      | Some (_, false, cursor') -> begin
          let mal = invalid_utf8 cursor cursor' in
          fn (accum_mal mal accum) lmargin cursor' t
        end
      | Some (cp, true, cursor') -> begin
          match cp with
          | cp when Codepoint.(cp = of_char ' ') -> fn_lpad c0_cursor accum lmargin cursor' t
          | cp when Codepoint.(cp = of_char '|') -> begin
              match (Text.Pos.col (Source.Cursor.pos cursor')) = lmargin with
              | true -> fn accum lmargin cursor' t
              | false -> begin
                  let mal = invalid_bar_indent c0_cursor cursor' in
                  fn (accum_mal mal accum) lmargin cursor' t
                end
            end
          | cp when Codepoint.(cp = of_char '`') -> accept_bstring accum cursor' t
          | cp when Codepoint.(cp = nl) -> begin
              let mal = invalid_bar_indent c0_cursor cursor in
              fn_lpad cursor' (accum_mal mal accum) lmargin cursor' t
            end
          | _ -> begin
              let mal = invalid_bar_indent c0_cursor cursor in
              fn (accum_mal mal accum) lmargin cursor' t
            end
        end
    end
    and fn accum lmargin cursor t = begin
      match Source.Cursor.nextv_opt cursor with
      | None -> begin
          let mal = unterminated_string t.tok_base cursor in
          accept_bstring (accum_mal mal accum) cursor t
        end
      | Some (_, false, cursor') -> begin
          let mal = invalid_utf8 cursor cursor' in
          fn (accum_mal mal accum) lmargin cursor' t
        end
      | Some (cp, true, cursor') -> begin
          let accum' = accum_cp cp accum in
          match cp with
          | cp when Codepoint.(cp = nl) -> fn_lpad cursor' accum' lmargin cursor' t
          | _ -> fn accum' lmargin cursor' t
        end
    end in
    let lmargin = Text.Pos.col (Source.Cursor.pos cursor) in
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
  val r_suffix: t -> Source.Cursor.t -> Radix.t -> Source.Cursor.t -> outer
    -> outer * ConcreteToken.t
  val exp: t -> Source.Cursor.t -> Radix.t -> Source.Cursor.t -> outer -> outer * ConcreteToken.t
  val dot: t -> Source.Cursor.t -> Radix.t -> Source.Cursor.t -> outer -> outer * ConcreteToken.t
  val zero_r_suffix: Source.Cursor.t -> Source.Cursor.t -> Source.Cursor.t -> outer
    -> outer * ConcreteToken.t
  val zero_frac: Source.Cursor.t -> Source.Cursor.t -> Source.Cursor.t -> outer
    -> outer * ConcreteToken.t
  val zero_exp: Source.Cursor.t -> Source.Cursor.t -> Source.Cursor.t -> outer
    -> outer * ConcreteToken.t
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
                | None -> malformed (out_of_range_real t.tok_base cursor)
              )
            end
          | Subtype_r64 -> begin
              Tok_r64 (
                match Realer.to_r64_opt realer with
                | Some r -> (Constant r)
                | None -> malformed (out_of_range_real t.tok_base cursor)
              )
            end
        in
        accept tok cursor t
      end
    | R _, Dec -> not_reached ()
    | R_dec, Dec -> begin
        let r = Real.of_string Source.Slice.(to_string (of_cursors ~base:t.tok_base
            ~past:suffix_cursor)) in
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
                let _cp, digits_cursor = Source.Cursor.next suffix_cursor in
                match Source.Cursor.(digits_cursor = cursor) with
                | true -> Some Subtype_r64 (* "r" suffix. *)
                | false -> None
              end
            | _ -> None
          end
        | None -> None
      in
      let accum' = match subtype_opt with
        | None -> begin
            let mal = unsupported_bitwidth suffix_cursor cursor in
            accum_mal mal accum
          end
        | Some _ -> accum
      in
      accept subtype_opt suffix_cursor accum' radix cursor t
    end in
    match Source.Cursor.next_opt cursor with
    | None -> accept_subtype bitwidth suffix_cursor accum radix cursor t
    | Some (cp, cursor') -> begin
        match Map.get cp num_suffix_map with
        | Some NSMapDigit -> begin
            let digit = nat_of_cp cp in
            match Nat.(bitwidth = zero && digit = zero) with
            | true -> begin
                let mal = invalid_type_suffix_leading_zero cursor cursor' in
                let accum' = accum_mal mal accum in
                next_suffix bitwidth suffix_cursor accum' radix cursor' t
              end
            | false -> begin
                let bitwidth' = Radix.(nat_accum digit bitwidth Dec) in
                next_suffix bitwidth' suffix_cursor accum radix cursor' t
              end
          end
        | Some NSMapIdent -> begin
            let mal = invalid_numerical cursor cursor' in
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
    match Source.Cursor.next_opt cursor with
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
            let mal = invalid_numerical cursor cursor' in
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
    match Source.Cursor.next_opt cursor with
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
            let mal = invalid_numerical cursor cursor' in
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
    match Source.Cursor.next_opt cursor with
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
            let mal = invalid_numerical cursor cursor' in
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

module State = struct
  module Integer_bin = struct
    type t = {
      n: Nat.t;
    }

    let pp {n} formatter =
      formatter |> Fmt.fmt "{n=" |> Nat.pp n |> Fmt.fmt "}"

    let init ~n =
      {n}

    let bin_accum digit {n} =
      {n=Nat.(n * k_2 + digit)}

    let oct_accum digit {n} =
      {n=Nat.(n * k_8 + digit)}

    let dec_accum digit {n} =
      {n=Nat.(n * k_a + digit)}

    let hex_accum digit {n} =
      {n=Nat.(n * k_g + digit)}
  end

  module Integer_oct = Integer_bin

  module Integer_dec = Integer_bin

  module Integer_hex = Integer_bin

  module Integer_bin_dot = Integer_bin

  module Integer_oct_dot = Integer_bin

  module Integer_dec_dot = Integer_bin

  module Integer_hex_dot = Integer_bin

  module Integer_u = struct
    type t = {
      n: Nat.t;
      radix: Radix.t;
    }

    let pp {n; radix} formatter =
      formatter
      |> Fmt.fmt "{n=" |> Nat.pp n
      |> Fmt.fmt "; radix=" |> Radix.pp radix
      |> Fmt.fmt "}"

    let init ~n ~radix =
      {n; radix}
  end

  module Integer_i = Integer_u

  module Integer_n = Integer_u

  module Integer_z = Integer_u

  module Integer_u_bitwidth = struct
    type t = {
      n: Nat.t;
      radix: Radix.t;
      bitwidth: Nat.t;
    }

    let pp {n; radix; bitwidth} formatter =
      formatter
      |> Fmt.fmt "{n=" |> Nat.pp n
      |> Fmt.fmt "; radix=" |> Radix.pp radix
      |> Fmt.fmt "; bitwidth=" |> Nat.pp bitwidth
      |> Fmt.fmt "}"

    let init ~n ~radix ~bitwidth =
      {n; radix; bitwidth}

    let bitwidth_accum digit ({bitwidth; _} as t) =
      {t with bitwidth=Nat.(bitwidth * k_a + digit)}
  end

  module Integer_i_bitwidth = Integer_u_bitwidth

  module Paren_comment_body = struct
    type t = {
      nesting: uns;
    }

    let pp {nesting} formatter =
      formatter |> Fmt.fmt "{nesting=" |> Uns.pp nesting |> Fmt.fmt "}"

    let init ~nesting =
      {nesting}
  end

  module Paren_comment_lparen = Paren_comment_body

  module Paren_comment_star = Paren_comment_body

  module Src_directive_path = struct
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

  module Src_directive_path_bslash = struct
    type t = {
      mals: AbstractToken.Rendition.Malformation.t list;
      path: codepoint list option;
      bslash_cursor: Source.Cursor.t;
    }

    let pp {mals; path; bslash_cursor} formatter =
      formatter
      |> Fmt.fmt "{mals=" |> (List.pp AbstractToken.Rendition.Malformation.pp) mals
      |> Fmt.fmt "; path=" |> (Option.pp (List.pp Codepoint.pp)) path
      |> Fmt.fmt "; bslash_cursor=" |> Text.Pos.pp (Source.Cursor.pos bslash_cursor)
      |> Fmt.fmt "}"

    let mals_accum mal {mals; path; _} =
      Src_directive_path.{mals=mal :: mals; path}

    let path_accum cp {mals; path; _} =
      match path with
      | None -> Src_directive_path.{mals; path=Some [cp]}
      | Some cps -> Src_directive_path.{mals; path=Some (cp :: cps)}
  end

  module Src_directive_path_bslash_u = Src_directive_path_bslash

  module Src_directive_path_bslash_u_lcurly = struct
    type t = {
      mals: AbstractToken.Rendition.Malformation.t list;
      path: codepoint list option;
      bslash_cursor: Source.Cursor.t;
    }

    let pp {mals; path; bslash_cursor} formatter =
      formatter
      |> Fmt.fmt "{mals=" |> (List.pp AbstractToken.Rendition.Malformation.pp) mals
      |> Fmt.fmt "; path=" |> (Option.pp (List.pp Codepoint.pp)) path
      |> Fmt.fmt "; bslash_cursor=" |> Text.Pos.pp (Source.Cursor.pos bslash_cursor)
      |> Fmt.fmt "}"

    let init ~mals ~path ~bslash_cursor =
      {mals; path; bslash_cursor}

    let mals_accum mal {mals; path; _} =
      Src_directive_path.{mals=mal :: mals; path}
  end

  module Src_directive_path_bslash_u_lcurly_hex = struct
    type t = {
      mals: AbstractToken.Rendition.Malformation.t list;
      path: codepoint list option;
      bslash_cursor: Source.Cursor.t;
      hex: Nat.t;
    }

    let pp {mals; path; bslash_cursor; hex} formatter =
      formatter
      |> Fmt.fmt "{mals=" |> (List.pp AbstractToken.Rendition.Malformation.pp) mals
      |> Fmt.fmt "; path=" |> (Option.pp (List.pp Codepoint.pp)) path
      |> Fmt.fmt "; bslash_cursor=" |> Text.Pos.pp (Source.Cursor.pos bslash_cursor)
      |> Fmt.fmt "; hex=" |> Nat.fmt ~alt:true ~base:Fmt.Hex ~pretty:true hex
      |> Fmt.fmt "}"

    let init ~mals ~path ~bslash_cursor ~hex =
      {mals; path; bslash_cursor; hex}

    let mals_accum mal {mals; path; _} =
      Src_directive_path.{mals=mal :: mals; path}

    let path_accum cp {mals; path; _} =
      match path with
      | None -> Src_directive_path.{mals; path=Some [cp]}
      | Some cps -> Src_directive_path.{mals; path=Some (cp :: cps)}

    let hex_accum digit ({hex; _} as t) =
      {t with hex=Nat.(hex * k_g + digit)}
  end

  module Src_directive_rditto = Src_directive_path

  module Src_directive_path_colon = Src_directive_path

  module Src_directive_line = struct
    type t = {
      mals: AbstractToken.Rendition.Malformation.t list;
      path: codepoint list option;
      line_cursor: Source.Cursor.t;
      line: Nat.t option;
    }

    let pp {mals; path; line_cursor; line} formatter =
      formatter
      |> Fmt.fmt "{mals=" |> (List.pp AbstractToken.Rendition.Malformation.pp) mals
      |> Fmt.fmt "; path=" |> (Option.pp (List.pp Codepoint.pp)) path
      |> Fmt.fmt "; line_cursor=" |> Text.Pos.pp (Source.Cursor.pos line_cursor)
      |> Fmt.fmt "; line=" |> (Option.pp Nat.pp) line
      |> Fmt.fmt "}"

    let init ~mals ~path ~line_cursor ~line =
      {mals; path; line_cursor; line}

    let line_accum digit ({line; _} as t) =
      {t with line=match line with None -> None | Some l -> Some Nat.(l * k_a + digit)}
  end

  module Src_directive_line_colon = struct
    type t = {
      mals: AbstractToken.Rendition.Malformation.t list;
      path: codepoint list option;
      line: Nat.t option;
    }

    let pp {mals; path; line} formatter =
      formatter
      |> Fmt.fmt "{mals=" |> (List.pp AbstractToken.Rendition.Malformation.pp) mals
      |> Fmt.fmt "; path=" |> (Option.pp (List.pp Codepoint.pp)) path
      |> Fmt.fmt "; line=" |> (Option.pp Nat.pp) line
      |> Fmt.fmt "}"

    let init ~mals ~path ~line =
      {mals; path; line}

    let mals_accum mal ({mals; _} as t) =
      {t with mals=mal :: mals}
  end

  module Src_directive_indent = struct
    type t = {
      mals: AbstractToken.Rendition.Malformation.t list;
      path: codepoint list option;
      line: Nat.t option;
      indent_cursor: Source.Cursor.t;
      indent: Nat.t;
    }

    let pp {mals; path; line; indent_cursor; indent} formatter =
      formatter
      |> Fmt.fmt "{mals=" |> (List.pp AbstractToken.Rendition.Malformation.pp) mals
      |> Fmt.fmt "; path=" |> (Option.pp (List.pp Codepoint.pp)) path
      |> Fmt.fmt "; line=" |> (Option.pp Nat.pp) line
      |> Fmt.fmt "; indent_cursor=" |> Text.Pos.pp (Source.Cursor.pos indent_cursor)
      |> Fmt.fmt "; indent=" |> Nat.pp indent
      |> Fmt.fmt "}"

    let init ~mals ~path ~line ~indent_cursor ~indent =
      {mals; path; line; indent_cursor; indent}

    let mals_accum mal ({mals; _} as t) =
      {t with mals=mal :: mals}

    let indent_accum digit ({indent; _} as t) =
      {t with indent=Nat.(indent * k_a + digit)}
  end

  module Src_directive_indent_0 = struct
    type t = {
      mals: AbstractToken.Rendition.Malformation.t list;
      path: codepoint list option;
      line: Nat.t option;
    }

    let pp {mals; path; line} formatter =
      formatter
      |> Fmt.fmt "{mals=" |> (List.pp AbstractToken.Rendition.Malformation.pp) mals
      |> Fmt.fmt "; path=" |> (Option.pp (List.pp Codepoint.pp)) path
      |> Fmt.fmt "; line=" |> (Option.pp Nat.pp) line
      |> Fmt.fmt "}"

    let init ~mals ~path ~line =
      {mals; path; line}

    let mals_accum mal ({mals; _} as t) =
      {t with mals=mal :: mals}
  end

  module Src_directive_indent_plus = struct
    type t = {
      mals: AbstractToken.Rendition.Malformation.t list;
      path: codepoint list option;
      line: Nat.t option;
      indent: Nat.t option;
    }

    let pp {mals; path; line; indent} formatter =
      formatter
      |> Fmt.fmt "{mals=" |> (List.pp AbstractToken.Rendition.Malformation.pp) mals
      |> Fmt.fmt "; path=" |> (Option.pp (List.pp Codepoint.pp)) path
      |> Fmt.fmt "; line=" |> (Option.pp Nat.pp) line
      |> Fmt.fmt "; indent=" |> (Option.pp Nat.pp) indent
      |> Fmt.fmt "}"

    let init ~mals ~path ~line ~indent =
      {mals; path; line; indent}

    let mals_accum mal ({mals; _} as t) =
      {t with mals=mal :: mals}
  end

  module Src_directive_omit = struct
    type t = {
      mals: AbstractToken.Rendition.Malformation.t list;
      path: codepoint list option;
      line: Nat.t option;
      indent: Nat.t option;
      omit_cursor: Source.Cursor.t;
      omit: Nat.t;
    }

    let pp {mals; path; line; indent; omit_cursor; omit} formatter =
      formatter
      |> Fmt.fmt "{mals=" |> (List.pp AbstractToken.Rendition.Malformation.pp) mals
      |> Fmt.fmt "; path=" |> (Option.pp (List.pp Codepoint.pp)) path
      |> Fmt.fmt "; line=" |> (Option.pp Nat.pp) line
      |> Fmt.fmt "; indent=" |> (Option.pp Nat.pp) indent
      |> Fmt.fmt "; omit_cursor=" |> Text.Pos.pp (Source.Cursor.pos omit_cursor)
      |> Fmt.fmt "; omit=" |> Nat.pp omit
      |> Fmt.fmt "}"

    let init ~mals ~path ~line ~indent ~omit_cursor ~omit =
      {mals; path; line; indent; omit_cursor; omit}

    let mals_accum mal ({mals; _} as t) =
      {t with mals=mal :: mals}

    let omit_accum digit ({omit; _} as t) =
      {t with omit=Nat.(omit * k_a + digit)}
  end

  module Src_directive_omit_0 = Src_directive_indent_plus

  module Codepoint_bslash = struct
    type t = {
      bslash_cursor: Source.Cursor.t;
    }

    let pp {bslash_cursor} formatter =
      formatter
      |> Fmt.fmt "{bslash_cursor=" |> Text.Pos.pp (Source.Cursor.pos bslash_cursor) |> Fmt.fmt "}"

    let init ~bslash_cursor =
      {bslash_cursor}
  end

  module Codepoint_bslash_u = Codepoint_bslash

  module Codepoint_bslash_u_lcurly = Codepoint_bslash

  module Codepoint_bslash_u_lcurly_hex = struct
    type t = {
      bslash_cursor: Source.Cursor.t;
      hex: Nat.t;
    }

    let pp {bslash_cursor; hex} formatter =
      formatter
      |> Fmt.fmt "{bslash_cursor=" |> Text.Pos.pp (Source.Cursor.pos bslash_cursor)
      |> Fmt.fmt "; hex=" |> Nat.fmt ~alt:true ~base:Fmt.Hex ~pretty:true hex
      |> Fmt.fmt "}"

    let init ~bslash_cursor ~hex =
      {bslash_cursor; hex}

    let hex_accum digit ({hex; _} as t) =
      {t with hex=Nat.(hex * k_g + digit)}
  end

  module Codepoint_cp = struct
    type t = {
      cp: codepoint;
    }

    let pp {cp} formatter =
      formatter |> Fmt.fmt "{cp=" |> Codepoint.pp cp |> Fmt.fmt "}"

    let init ~cp =
      {cp}
  end

  module Codepoint_mal = struct
    type t = {
      mal: AbstractToken.Rendition.Malformation.t;
    }

    let pp {mal} formatter =
      formatter
      |> Fmt.fmt "{mal=" |> AbstractToken.Rendition.Malformation.pp mal |> Fmt.fmt "}"

    let init mal =
      {mal}
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
      bslash_cursor: Source.Cursor.t;
    }

    let pp t formatter =
      formatter
      |> Fmt.fmt "{accum=" |> CodepointAccum.pp t.accum
      |> Fmt.fmt "; bslash_cursor=" |> Text.Pos.pp (Source.Cursor.pos t.bslash_cursor)
      |> Fmt.fmt "}"
  end

  module Isubstring_bslash_u = Isubstring_bslash

  module Isubstring_bslash_u_lcurly = struct
    type t = {
      accum: CodepointAccum.t;
      bslash_cursor: Source.Cursor.t;
      u: Nat.t;
    }

    let pp t formatter =
      formatter
      |> Fmt.fmt "{accum=" |> CodepointAccum.pp t.accum
      |> Fmt.fmt "; bslash_cursor=" |> Text.Pos.pp (Source.Cursor.pos t.bslash_cursor)
      |> Fmt.fmt "; u=" |> Nat.fmt ~alt:true ~base:Fmt.Hex ~pretty:true t.u
      |> Fmt.fmt "}"
  end

  type t =
    | State_start
    | State_semi
    | State_lparen
    | State_lbrack
    | State_tilde
    | State_qmark
    | State_star
    | State_caret
    | State_bar
    | State_uscore
    | State_tick
    | State_tick_lookahead
    | State_btick
    | State_integer_0
    | State_integer_0_dot
    | State_integer_0b
    | State_integer_0o
    | State_integer_0x
    | State_integer_bin of Integer_bin.t
    | State_integer_oct of Integer_oct.t
    | State_integer_dec of Integer_dec.t
    | State_integer_hex of Integer_hex.t
    | State_integer_bin_dot of Integer_bin_dot.t
    | State_integer_oct_dot of Integer_oct_dot.t
    | State_integer_dec_dot of Integer_dec_dot.t
    | State_integer_hex_dot of Integer_hex_dot.t
    | State_integer_u of Integer_u.t
    | State_integer_i of Integer_i.t
    | State_integer_n of Integer_n.t
    | State_integer_z of Integer_z.t
    | State_integer_u_bitwidth of Integer_u_bitwidth.t
    | State_integer_i_bitwidth of Integer_i_bitwidth.t
    | State_integer_mal_ident
    | State_ident_uscore
    | State_ident_cident
    | State_ident_uident
    | State_ident_mal
    | State_operator_tilde
    | State_operator_qmark
    | State_operator_star_star
    | State_operator_star
    | State_operator_slash
    | State_operator_pct
    | State_operator_plus
    | State_operator_minus
    | State_operator_at
    | State_operator_caret
    | State_operator_dollar
    | State_operator_lt
    | State_operator_eq
    | State_operator_gt
    | State_operator_bar
    | State_operator_colon
    | State_operator_dot
    | State_paren_comment_body of Paren_comment_body.t
    | State_paren_comment_lparen of Paren_comment_lparen.t
    | State_paren_comment_star of Paren_comment_star.t
    | State_src_directive_colon
    | State_src_directive_path of Src_directive_path.t
    | State_src_directive_path_bslash of Src_directive_path_bslash.t
    | State_src_directive_path_bslash_u of Src_directive_path_bslash_u.t
    | State_src_directive_path_bslash_u_lcurly of Src_directive_path_bslash_u_lcurly.t
    | State_src_directive_path_bslash_u_lcurly_hex of Src_directive_path_bslash_u_lcurly_hex.t
    | State_src_directive_rditto of Src_directive_rditto.t
    | State_src_directive_path_colon of Src_directive_path_colon.t
    | State_src_directive_line of Src_directive_line.t
    | State_src_directive_line_colon of Src_directive_line_colon.t
    | State_src_directive_indent of Src_directive_indent.t
    | State_src_directive_indent_0 of Src_directive_indent_0.t
    | State_src_directive_indent_plus of Src_directive_indent_plus.t
    | State_src_directive_omit of Src_directive_omit.t
    | State_src_directive_omit_0 of Src_directive_omit_0.t
    | State_dentation_start
    | State_dentation_lparen
    | State_dentation_space
    | State_whitespace
    | State_hash_comment
    | State_codepoint_tick
    | State_codepoint_bslash of Codepoint_bslash.t
    | State_codepoint_bslash_u of Codepoint_bslash_u.t
    | State_codepoint_bslash_u_lcurly of Codepoint_bslash_u_lcurly.t
    | State_codepoint_bslash_u_lcurly_hex of Codepoint_bslash_u_lcurly_hex.t
    | State_codepoint_cp of Codepoint_cp.t
    | State_codepoint_mal of Codepoint_mal.t
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
    | State_tilde -> formatter |> Fmt.fmt "State_tilde"
    | State_qmark -> formatter |> Fmt.fmt "State_qmark"
    | State_star -> formatter |> Fmt.fmt "State_star"
    | State_caret -> formatter |> Fmt.fmt "State_caret"
    | State_bar -> formatter |> Fmt.fmt "State_bar"
    | State_uscore -> formatter |> Fmt.fmt "State_uscore"
    | State_tick -> formatter |> Fmt.fmt "State_tick"
    | State_tick_lookahead -> formatter |> Fmt.fmt "State_tick_lookahead"
    | State_btick -> formatter |> Fmt.fmt "State_btick"
    | State_integer_0 -> formatter |> Fmt.fmt "State_integer_0"
    | State_integer_0_dot -> formatter |> Fmt.fmt "State_integer_0_dot"
    | State_integer_0b -> formatter |> Fmt.fmt "State_integer_0b"
    | State_integer_0o -> formatter |> Fmt.fmt "State_integer_0o"
    | State_integer_0x -> formatter |> Fmt.fmt "State_integer_0x"
    | State_integer_bin v -> formatter |> Fmt.fmt "State_integer_bin " |> Integer_bin.pp v
    | State_integer_oct v -> formatter |> Fmt.fmt "State_integer_oct " |> Integer_oct.pp v
    | State_integer_dec v -> formatter |> Fmt.fmt "State_integer_dec " |> Integer_dec.pp v
    | State_integer_hex v -> formatter |> Fmt.fmt "State_integer_hex " |> Integer_hex.pp v
    | State_integer_bin_dot v ->
      formatter |> Fmt.fmt "State_integer_bin_dot " |> Integer_bin_dot.pp v
    | State_integer_oct_dot v ->
      formatter |> Fmt.fmt "State_integer_oct_dot " |> Integer_oct_dot.pp v
    | State_integer_dec_dot v ->
      formatter |> Fmt.fmt "State_integer_dec_dot " |> Integer_dec_dot.pp v
    | State_integer_hex_dot v ->
      formatter |> Fmt.fmt "State_integer_hex_dot " |> Integer_hex_dot.pp v
    | State_integer_u v -> formatter |> Fmt.fmt "State_integer_u " |> Integer_u.pp v
    | State_integer_i v -> formatter |> Fmt.fmt "State_integer_i " |> Integer_i.pp v
    | State_integer_n v -> formatter |> Fmt.fmt "State_integer_n " |> Integer_n.pp v
    | State_integer_z v -> formatter |> Fmt.fmt "State_integer_z " |> Integer_z.pp v
    | State_integer_u_bitwidth v ->
      formatter |> Fmt.fmt "State_integer_u_bitwidth " |> Integer_u_bitwidth.pp v
    | State_integer_i_bitwidth v ->
      formatter |> Fmt.fmt "State_integer_i_bitwidth " |> Integer_i_bitwidth.pp v
    | State_integer_mal_ident -> formatter |> Fmt.fmt "State_integer_mal_ident"
    | State_ident_uscore -> formatter |> Fmt.fmt "State_ident_uscore"
    | State_ident_cident -> formatter |> Fmt.fmt "State_ident_cident"
    | State_ident_uident -> formatter |> Fmt.fmt "State_ident_uident"
    | State_ident_mal -> formatter |> Fmt.fmt "State_ident_mal"
    | State_operator_tilde -> formatter |> Fmt.fmt "State_operator_tilde"
    | State_operator_qmark -> formatter |> Fmt.fmt "State_operator_qmark"
    | State_operator_star_star -> formatter |> Fmt.fmt "State_operator_star"
    | State_operator_star -> formatter |> Fmt.fmt "State_operator_star"
    | State_operator_slash -> formatter |> Fmt.fmt "State_operator_slash"
    | State_operator_pct -> formatter |> Fmt.fmt "State_operator_pct"
    | State_operator_plus -> formatter |> Fmt.fmt "State_operator_plus"
    | State_operator_minus -> formatter |> Fmt.fmt "State_operator_minus"
    | State_operator_at -> formatter |> Fmt.fmt "State_operator_at"
    | State_operator_caret -> formatter |> Fmt.fmt "State_operator_caret"
    | State_operator_dollar -> formatter |> Fmt.fmt "State_operator_dollar"
    | State_operator_lt -> formatter |> Fmt.fmt "State_operator_lt"
    | State_operator_eq -> formatter |> Fmt.fmt "State_operator_eq"
    | State_operator_gt -> formatter |> Fmt.fmt "State_operator_gt"
    | State_operator_bar -> formatter |> Fmt.fmt "State_operator_bar"
    | State_operator_colon -> formatter |> Fmt.fmt "State_operator_colon"
    | State_operator_dot -> formatter |> Fmt.fmt "State_operator_dot"
    | State_paren_comment_body v ->
      formatter |> Fmt.fmt "State_paren_comment_body " |> Paren_comment_body.pp v
    | State_paren_comment_lparen v ->
      formatter |> Fmt.fmt "State_paren_comment_lparen " |> Paren_comment_lparen.pp v
    | State_paren_comment_star v ->
      formatter |> Fmt.fmt "State_paren_comment_star " |> Paren_comment_star.pp v
    | State_src_directive_colon -> formatter |> Fmt.fmt "State_src_directive_colon"
    | State_src_directive_path v -> formatter |> Fmt.fmt "State_path " |> Src_directive_path.pp v
    | State_src_directive_path_bslash v ->
      formatter |> Fmt.fmt "State_path_bslash " |> Src_directive_path_bslash.pp v
    | State_src_directive_path_bslash_u v ->
      formatter |> Fmt.fmt "State_path_bslash_u " |> Src_directive_path_bslash_u.pp v
    | State_src_directive_path_bslash_u_lcurly v ->
      formatter |> Fmt.fmt "State_path_bslash_u_lcurly " |> Src_directive_path_bslash_u_lcurly.pp v
    | State_src_directive_path_bslash_u_lcurly_hex v ->
      formatter |> Fmt.fmt "State_path_bslash_u_lcurly_hex "
      |> Src_directive_path_bslash_u_lcurly_hex.pp v
    | State_src_directive_rditto v ->
      formatter |> Fmt.fmt "State_rditto " |> Src_directive_rditto.pp v
    | State_src_directive_path_colon v ->
      formatter |> Fmt.fmt "State_path_colon " |> Src_directive_path_colon.pp v
    | State_src_directive_line v -> formatter |> Fmt.fmt "State_line " |> Src_directive_line.pp v
    | State_src_directive_line_colon v ->
      formatter |> Fmt.fmt "State_line_colon " |> Src_directive_line_colon.pp v
    | State_src_directive_indent v ->
      formatter |> Fmt.fmt "State_indent " |> Src_directive_indent.pp v
    | State_src_directive_indent_0 v ->
      formatter |> Fmt.fmt "State_indent_0 " |> Src_directive_indent_0.pp v
    | State_src_directive_indent_plus v ->
      formatter |> Fmt.fmt "State_indent_plus " |> Src_directive_indent_plus.pp v
    | State_src_directive_omit v -> formatter |> Fmt.fmt "State_omit " |> Src_directive_omit.pp v
    | State_src_directive_omit_0 v ->
      formatter |> Fmt.fmt "State_omit_0 " |> Src_directive_omit_0.pp v
    | State_dentation_start -> formatter |> Fmt.fmt "State_dentation_start"
    | State_dentation_lparen -> formatter |> Fmt.fmt "State_dentation_lparen"
    | State_dentation_space -> formatter |> Fmt.fmt "State_dentation_space"
    | State_whitespace -> formatter |> Fmt.fmt "State_whitespace"
    | State_hash_comment -> formatter |> Fmt.fmt "State_hash_comment"
    | State_codepoint_tick -> formatter |> Fmt.fmt "State_codepoint_tick"
    | State_codepoint_bslash v ->
      formatter |> Fmt.fmt "State_codepoint_bslash " |> Codepoint_bslash.pp v
    | State_codepoint_bslash_u v ->
      formatter |> Fmt.fmt "State_codepoint_bslash_u " |> Codepoint_bslash_u.pp v
    | State_codepoint_bslash_u_lcurly v ->
      formatter |> Fmt.fmt "State_codepoint_bslash_u_lcurly " |> Codepoint_bslash_u_lcurly.pp v
    | State_codepoint_bslash_u_lcurly_hex v ->
      formatter |> Fmt.fmt "State_codepoint_bslash_u_lcurly_hex "
      |> Codepoint_bslash_u_lcurly_hex.pp v
    | State_codepoint_cp v -> formatter |> Fmt.fmt "State_codepoint_cp " |> Codepoint_cp.pp v
    | State_codepoint_mal v -> formatter |> Fmt.fmt "State_codepoint_mal " |> Codepoint_mal.pp v
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
  let dentation_start = State_dentation_start
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

  let accept_source_directive atoken View.{cursor; _} t =
    (* Treat the directive as having come from the unbiased source. Rebias the cursors such that
     * they are unbiased. This is different than unbiasing, in that it preserves the cursors' bias
     * chain, which enables recovering source bias when moving leftwards. *)
    let base = Source.Cursor.bias (Source.unbias (Source.Cursor.container t.tok_base)) t.tok_base in
    let past = begin
      let rec f cursor cursor_fork = begin
        match Source.Cursor.(cursor < cursor_fork) with
        | false -> cursor
        | true -> f (Source.Cursor.succ cursor) cursor_fork
      end in
      f base cursor
    end in
    let cursor', level' = match atoken with
      | AbstractToken.Tok_source_directive Constant {path=None; line=None; io=None}
        -> begin
            (* Rebias the source such that it is unbiased. *)
            past, Level.reset t.level
          end
      | AbstractToken.Tok_source_directive Constant {path; line; io} -> begin
          let source = Source.Cursor.container past in
          let path = match path with
            | None -> Source.path source
            | Some path -> Some path
          in
          let line = match line with
            | None -> Sint.(kv 1L)
            | Some line -> line
          in
          let line_bias = Sint.((Uns.bits_to_sint line) - (Uns.bits_to_sint (Text.(Pos.line
              (Cursor.pos (Source.Cursor.text_cursor past)))))) in
          let indent, omit = match io with
            | None -> Sint.zero, Sint.zero
            | Some {indent; omit} -> indent, omit
          in
          let col = Sint.(indent + omit) in
          let col_bias = Sint.((Uns.bits_to_sint col) - (Uns.bits_to_sint (Text.(Pos.col (Cursor.pos
              (Source.Cursor.text_cursor past)))))) in
          let source' = Source.bias ~path ~line_bias ~col_bias source in
          (Source.Cursor.bias source' past), Level.embed (indent / 4L) t.level
        end
      | AbstractToken.Tok_source_directive Malformed _ -> past, t.level
      | _ -> not_reached ()
    in
    let source_slice = Source.Slice.of_cursors ~base ~past in
    {t with tok_base=cursor'; level=level'}, Accept (ConcreteToken.init atoken source_slice)

  let accept_line_break atoken cursor t =
    let source = source_at cursor t in
    {t with tok_base=cursor; line_state=Line_begin}, Accept (ConcreteToken.init atoken source)

  let accept_line_break_incl atoken View.{cursor; _} t =
    accept_line_break atoken cursor t

  (* XXX Remove? *)
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
        ("{", accept_incl Tok_lcurly);
        ("}", accept_incl Tok_rcurly);
        ("\\", accept_incl Tok_bslash);
        ("&", accept_incl Tok_amp);
        ("!", accept_incl Tok_xmark);
        ("\n", accept_line_break_incl Tok_whitespace);
        ("~", advance State_tilde);
        ("?", advance State_qmark);
        ("*", advance State_star);
        ("/", advance State_operator_slash);
        ("%", advance State_operator_pct);
        ("+", advance State_operator_plus);
        ("-", advance State_operator_minus);
        ("@", advance State_operator_at);
        ("^", advance State_caret);
        ("$", advance State_operator_dollar);
        ("<", advance State_operator_lt);
        ("=", advance State_operator_eq);
        (">", advance State_operator_gt);
        ("|", advance State_bar);
        (":", advance State_operator_colon);
        (".", advance State_operator_dot);
        (" ", advance State_whitespace);
        ("#", advance State_hash_comment);
        ("_", advance State_uscore);
        ("ABCDEFGHIJKLMNOPQRSTUVWXYZ", advance State_ident_cident);
        ("abcdefghijklmnopqrstuvwxyz", advance State_ident_uident);
        ("'", advance State_tick);
        ("\"", wrap_legacy (accept_istring_push Istring_interp Tok_istring_lditto));
        ("`", advance State_btick);
        ("0", advance State_integer_0);
        ("123456789", (fun (View.{pcursor; _} as view) t ->
            let digit = nat_of_cp (Source.Cursor.rget pcursor) in
            advance (State_integer_dec (State.Integer_dec.init ~n:digit)) view t
          )
        );
      ];
      default0=accept_incl Tok_error;
      eoi0=(fun view t ->
        match Level.level t.level, t.line_state with
        | 0L, Line_begin -> accept_dentation Tok_line_delim view t
        | 0L, _ -> accept_incl Tok_end_of_input view t
        | _ -> accept_dentation (Tok_dedent (Constant ())) view {t with level=Level.pred t.level}
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
      ("|", accept_incl Tok_lcapture);
      ("*", advance (State_paren_comment_body (State.Paren_comment_body.init ~nesting:1L)));
    ];
    default0=accept_excl Tok_lparen;
    eoi0=accept_incl Tok_lparen;
  }

  let node0_lbrack = {
    edges0=map_of_cps_alist [
      ("|", accept_incl Tok_larray);
      (":", advance State_src_directive_colon);
    ];
    default0=accept_excl Tok_lbrack;
    eoi0=accept_incl Tok_lbrack;
  }

  let node0_tilde = {
    edges0=map_of_cps_alist [
      (operator_cps, advance State_operator_tilde);
    ];
    default0=accept_excl Tok_tilde;
    eoi0=accept_incl Tok_tilde;
  }

  let node0_qmark = {
    edges0=map_of_cps_alist [
      (operator_cps, advance State_operator_qmark);
    ];
    default0=accept_excl Tok_qmark;
    eoi0=accept_incl Tok_qmark;
  }

  let node0_star = {
    edges0=map_of_cpsets_alist [
      (cpset_of_cps "*", advance State_operator_star_star);
      (Set.diff (cpset_of_cps operator_cps) (cpset_of_cps "*"),
        advance State_operator_star);
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
      (operator_cps, advance State_operator_caret);
    ];
    default0=accept_excl Tok_caret;
    eoi0=accept_incl Tok_caret;
  }

  let node0_bar = {
    edges0=map_of_cps_alist [
      (")", accept_incl Tok_rcapture);
      ("]", accept_incl Tok_rarray);
      (operator_cps, advance State_operator_bar);
    ];
    default0=accept_excl Tok_bar;
    eoi0=accept_incl Tok_bar;
  }

  let node0_uscore = {
    edges0=map_of_cps_alist [
      ("_", advance State_ident_uscore);
      (ident_cident_cps, advance State_ident_cident);
      (ident_uident_cps, advance State_ident_uident);
      (ident_continue_cps, advance State_ident_mal);
    ];
    default0=accept_excl Tok_uscore;
    eoi0=accept_incl Tok_uscore;
  }

  let node0_tick = {
    edges0=map_of_cps_alist [
      (String.concat ["_"; ident_uident_cps; whitespace_cps], advance State_tick_lookahead);
    ];
    default0=(fun _view t -> retry State_codepoint_tick t);
    eoi0=accept_incl Tok_tick;
  }

  let node0_tick_lookahead = {
    edges0=map_of_cps_alist [
      ("'", (fun View.{ppcursor; cursor; _} t ->
          let cp = Source.Cursor.rget ppcursor in
          accept (Tok_codepoint (Constant cp)) cursor t
        )
      );
    ];
    default0=accept_pexcl Tok_tick;
    eoi0=accept_excl Tok_tick;
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

  module Integer = struct
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
      | Subtype_nat
      | Subtype_zint

    (* Prefix signs are scanned as separate tokens, and there is no way to distinguish them from
     * infix operators until parsing is complete. Therefore the scanner accepts min_value (e.g.
     * 0x80i8) regardless of whether it's negative. Thanks to 2s complement representation, both
     * 0x80i8 and -0x80i8 encode min_value, and no correctness issues arise from allowing 0x80i8.
     *
     * It would be possible to disallow this edge case during an AST optimization pass which
     * combines constants and their prefix signs; if the compiler does so, it should take care to
     * emit error messages formatted the same as those emitted by the scanner. Although it would be
     * possible to push the limit checking into the post-parsing optimization, doing so would make
     * the optimization mandatory, as well as making it more likely for the programmer to not see
     * such errors unless there are no parse errors which prevent code generation. *)
    let limit_of_subtype subtype =
      let open Nat in
      match subtype with
      | Subtype_u8 -> Some max_u8
      | Subtype_i8 -> Some max_abs_i8
      | Subtype_u16 -> Some max_u16
      | Subtype_i16 -> Some max_abs_i16
      | Subtype_u32 -> Some max_u32
      | Subtype_i32 -> Some max_abs_i32
      | Subtype_u64 -> Some max_u64
      | Subtype_i64 -> Some max_abs_i64
      | Subtype_u128 -> Some max_u128
      | Subtype_i128 -> Some max_abs_i128
      | Subtype_u256 -> Some max_u256
      | Subtype_i256 -> Some max_abs_i256
      | Subtype_u512 -> Some max_u512
      | Subtype_i512 -> Some max_abs_i512
      | Subtype_nat
      | Subtype_zint -> None

    let accept_integer ?subtype n radix cursor t =
      let subtype = match subtype with
        | None -> Subtype_u64
        | Some subtype -> subtype
      in
      let limit = match limit_of_subtype subtype with
        | None -> None
        | Some limit when Nat.(n <= limit) -> None
        | Some limit -> Some limit
      in
      let open AbstractToken in
      let tok = match limit with
        | Some limit -> begin
            let mal = out_of_range_int radix limit t.tok_base cursor in
            let malformed = Rendition.of_mals [mal] in
            match subtype with
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
            | Subtype_nat
            | Subtype_zint -> not_reached ()
          end
        | None -> begin
            match subtype with
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
            | Subtype_nat -> Tok_nat (Constant n)
            | Subtype_zint -> Tok_zint (Constant n)
          end
      in
      accept tok cursor t

    let accept_integer_incl ?subtype n radix View.{cursor; _} t =
      accept_integer ?subtype n radix cursor t

    let accept_integer_excl ?subtype n radix View.{pcursor; _} t =
      accept_integer ?subtype n radix pcursor t

    let accept_integer_pexcl ?subtype n radix View.{ppcursor; _} t =
      accept_integer ?subtype n radix ppcursor t

    let accept_zero cursor t =
      accept_integer Nat.zero Hex cursor t

    let accept_integer_bitwidth ~signedness ~bitwidth n radix cursor t =
      let subtype_opt = match signedness, Nat.to_uns_opt bitwidth with
        | Unsigned, Some 8L -> Some Subtype_u8
        | Signed, Some 8L -> Some Subtype_i8
        | Unsigned, Some 16L -> Some Subtype_u16
        | Signed, Some 16L -> Some Subtype_i16
        | Unsigned, Some 32L -> Some Subtype_u32
        | Signed, Some 32L -> Some Subtype_i32
        | Unsigned, Some 64L -> Some Subtype_u64
        | Signed, Some 64L -> Some Subtype_i64
        | Unsigned, Some 128L -> Some Subtype_u128
        | Signed, Some 128L -> Some Subtype_i128
        | Unsigned, Some 256L -> Some Subtype_u256
        | Signed, Some 256L -> Some Subtype_i256
        | Unsigned, Some 512L -> Some Subtype_u512
        | Signed, Some 512L -> Some Subtype_i512
        | _ -> None
      in
      match subtype_opt with
      | None -> begin
          let mal = unsupported_bitwidth t.tok_base cursor in
          let malformed = AbstractToken.Rendition.of_mals [mal] in
          accept (Tok_u64 malformed) cursor t
        end
      | Some subtype -> accept_integer ~subtype n radix cursor t

    let accept_integer_bitwidth_incl ~signedness ~bitwidth n radix View.{cursor; _} t =
      accept_integer_bitwidth ~signedness ~bitwidth n radix cursor t

    let accept_integer_bitwidth_excl ~signedness ~bitwidth n radix View.{pcursor; _} t =
      accept_integer_bitwidth ~signedness ~bitwidth n radix pcursor t

    let accept_zero_incl View.{cursor; _} t =
      accept_zero cursor t

    let accept_zero_excl View.{pcursor; _} t =
      accept_zero pcursor t

    let accept_zero_pexcl View.{ppcursor; _} t =
      accept_zero ppcursor t

    let accept_mal_indent cursor t =
      let mal = malformed (invalid_numerical t.tok_base cursor) in
      accept (Tok_uident mal) cursor t

    let accept_mal_indent_incl View.{cursor; _} t =
      accept_mal_indent cursor t

    let accept_mal_indent_excl View.{pcursor; _} t =
      accept_mal_indent pcursor t

    let node0_0 = {
      edges0=map_of_cpsets_alist [
        (cpset_of_cps "_", advance (State_integer_dec (State.Integer_dec.init ~n:Nat.k_0)));
        (cpset_of_cps dec_cps, (fun (View.{pcursor; _} as view) t ->
            let digit = nat_of_cp (Source.Cursor.rget pcursor) in
            advance (State_integer_dec (State.Integer_dec.init ~n:digit)) view t;
          )
        );
        (cpset_of_cps "b", advance State_integer_0b);
        (cpset_of_cps "o", advance State_integer_0o);
        (cpset_of_cps "x", advance State_integer_0x);
        (cpset_of_cps "u", advance (State_integer_u (State.Integer_u.init ~n:Nat.k_0 ~radix:Dec)));
        (cpset_of_cps "i", advance (State_integer_i (State.Integer_i.init ~n:Nat.k_0 ~radix:Dec)));
        (cpset_of_cps "n", advance (State_integer_n (State.Integer_n.init ~n:Nat.k_0 ~radix:Dec)));
        (cpset_of_cps "z", advance (State_integer_z (State.Integer_z.init ~n:Nat.k_0 ~radix:Dec)));
        (cpset_of_cps "r", wrap_legacy Real.zero_r_suffix);
        (cpset_of_cps ".", advance State_integer_0_dot);
        (cpset_of_cps "e", wrap_legacy Real.zero_exp);
        (Set.diff (cpset_of_cps ident_cps)
            (Set.union (cpset_of_cps dec_cps) (cpset_of_cps "_beinoruxz")),
          advance State_integer_mal_ident);
      ];
      default0=accept_zero_excl;
      eoi0=accept_zero_incl;
    }

    let node0_0_dot = {
      edges0=map_of_cps_alist [
        (operator_cps, accept_zero_pexcl);
      ];
      default0=wrap_legacy Real.zero_frac;
      eoi0=accept_incl Real.zero;
    }

    let node0_0box ~base_cps ~state ~state_base_init =
      {
        edges0=map_of_cpsets_alist [
          (cpset_of_cps "_", advance state);
          (cpset_of_cps base_cps, (fun (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (state_base_init digit) view t
            )
          );
          (Set.diff (cpset_of_cps ident_cps) (Set.union (cpset_of_cps "_") (cpset_of_cps base_cps)),
            advance State_integer_mal_ident);
        ];
        default0=accept_mal_indent_excl;
        eoi0=accept_mal_indent_incl;
      }
    let node0_0b = node0_0box ~base_cps:bin_cps ~state:State_integer_0b
        ~state_base_init:(fun n -> State_integer_bin (State.Integer_bin.init ~n))
    let node0_0o = node0_0box ~base_cps:oct_cps ~state:State_integer_0o
        ~state_base_init:(fun n -> State_integer_oct (State.Integer_oct.init ~n))
    let node0_0x = node0_0box ~base_cps:hex_cps ~state:State_integer_0x
        ~state_base_init:(fun n -> State_integer_hex (State.Integer_hex.init ~n))

    let node1_base ~radix ~base_cps ~state_init ~n_of_state ~digit_accum ~state_dot_init =
      {
        edges1=map_of_cpsets_alist [
          (cpset_of_cps "_", (fun state view t -> advance (state_init state) view t));
          (cpset_of_cps base_cps, (fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (state_init (state |> digit_accum digit)) view t));
          (cpset_of_cps ".", (fun state view t -> advance (state_dot_init state) view t));
          (cpset_of_cps "p", (fun state View.{pcursor; cursor; _} t ->
              let n = n_of_state state in
              let t', tok = Real.exp (Real.of_whole n radix) pcursor radix cursor t in
              t', Accept tok
            )
          );
          (cpset_of_cps "u", (fun state view t ->
              let n = n_of_state state in
              advance (State_integer_u (State.Integer_u.init ~n ~radix)) view t
            )
          );
          (cpset_of_cps "i", (fun state view t ->
              let n = n_of_state state in
              advance (State_integer_i (State.Integer_i.init ~n ~radix)) view t
            )
          );
          (cpset_of_cps "n", (fun state view t ->
              let n = n_of_state state in
              advance (State_integer_n (State.Integer_n.init ~n ~radix)) view t
            )
          );
          (cpset_of_cps "z", (fun state view t ->
              let n = n_of_state state in
              advance (State_integer_z (State.Integer_z.init ~n ~radix)) view t
            )
          );
          (cpset_of_cps "r", (fun state View.{pcursor; cursor; _} t ->
              let n = n_of_state state in
              let t', tok = Real.r_suffix (Real.of_whole n radix) pcursor radix cursor t in
              t', Accept tok
            )
          );
          (Set.diff (cpset_of_cps ident_cps)
              (Set.union (cpset_of_cps base_cps) (cpset_of_cps "_inpruz")),
            (fun _state view t -> advance State_integer_mal_ident view t));
        ];
        default1=(fun state view t ->
          let n = n_of_state state in
          accept_integer_excl n radix view t
        );
        eoi1=(fun state view t ->
          let n = n_of_state state in
          accept_integer_incl n radix view t
        );
      }
    let node1_bin =
      node1_base ~radix:Bin ~base_cps:bin_cps
        ~state_init:(fun state -> State.State_integer_bin state)
        ~n_of_state:(fun State.Integer_bin.{n} -> n)
        ~digit_accum:State.Integer_bin.bin_accum
        ~state_dot_init:(fun state -> State.State_integer_bin_dot state)
    let node1_oct =
      node1_base ~radix:Oct ~base_cps:oct_cps
        ~state_init:(fun state -> State.State_integer_oct state)
        ~n_of_state:(fun State.Integer_oct.{n} -> n)
        ~digit_accum:State.Integer_oct.oct_accum
        ~state_dot_init:(fun state -> State.State_integer_oct_dot state)
    let node1_dec =
      node1_base ~radix:Dec ~base_cps:dec_cps
        ~state_init:(fun state -> State.State_integer_dec state)
        ~n_of_state:(fun State.Integer_dec.{n} -> n)
        ~digit_accum:State.Integer_dec.dec_accum
        ~state_dot_init:(fun state -> State.State_integer_dec_dot state)
    let node1_hex =
      node1_base ~radix:Hex ~base_cps:hex_cps
        ~state_init:(fun state -> State.State_integer_hex state)
        ~n_of_state:(fun State.Integer_hex.{n} -> n)
        ~digit_accum:State.Integer_hex.hex_accum
        ~state_dot_init:(fun state -> State.State_integer_hex_dot state)

    let node1_base_dot ~radix ~n_of_state =
      {
        edges1=map_of_cps_alist [
          (".", (fun state view t ->
              let n = n_of_state state in
              accept_integer_pexcl n radix view t
            )
          );
        ];
        default1=(fun state View.{pcursor; _} t ->
          let mantissa_cursor = Source.Cursor.(seek 2L t.tok_base) in
          let n = n_of_state state in
          let t', tok = Real.dot (Real.of_whole n radix) mantissa_cursor radix pcursor t in
          t', Accept tok
        );
        eoi1=(fun state view t ->
          let n = n_of_state state in
          accept_integer_excl n radix view t
        );
      }
    let node1_bin_dot = node1_base_dot ~radix:Bin ~n_of_state:(fun State.Integer_bin_dot.{n} -> n)
    let node1_oct_dot = node1_base_dot ~radix:Oct ~n_of_state:(fun State.Integer_oct_dot.{n} -> n)
    let node1_dec_dot = node1_base_dot ~radix:Dec ~n_of_state:(fun State.Integer_dec_dot.{n} -> n)
    let node1_hex_dot = node1_base_dot ~radix:Hex ~n_of_state:(fun State.Integer_hex_dot.{n} -> n)

    let node1_u =
      let open State.Integer_u in
      {
        edges1=map_of_cps_alist [
          (dec_cps, (fun {n; radix} (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_integer_u_bitwidth (State.Integer_u_bitwidth.init ~n ~radix
                  ~bitwidth:digit)) view t
            )
          );
        ];
        default1=(fun {n; radix} view t -> accept_integer_excl ~subtype:Subtype_u64 n radix view t);
        eoi1=(fun {n; radix} view t -> accept_integer_incl ~subtype:Subtype_u64 n radix view t);
      }

    let node1_i =
      let open State.Integer_i in
      {
        edges1=map_of_cps_alist [
          (dec_cps, (fun {n; radix} (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_integer_i_bitwidth (State.Integer_i_bitwidth.init ~n ~radix
                  ~bitwidth:digit)) view t
            )
          );
        ];
        default1=(fun {n; radix} view t -> accept_integer_excl ~subtype:Subtype_i64 n radix view t);
        eoi1=(fun {n; radix} view t -> accept_integer_incl ~subtype:Subtype_i64 n radix view t);
      }

    let node1_n =
      let open State.Integer_n in
      {
        edges1=map_of_cps_alist [
          (dec_cps, (fun _state view t -> advance State_integer_mal_ident view t));
        ];
        default1=(fun {n; radix} view t -> accept_integer_excl ~subtype:Subtype_nat n radix view t);
        eoi1=(fun {n; radix} view t -> accept_integer_incl ~subtype:Subtype_nat n radix view t);
      }

    let node1_z =
      let open State.Integer_z in
      {
        edges1=map_of_cps_alist [
          (dec_cps, (fun _state view t -> advance State_integer_mal_ident view t));
        ];
        default1=(fun {n; radix} view t ->
          accept_integer_excl ~subtype:Subtype_zint n radix view t);
        eoi1=(fun {n; radix} view t -> accept_integer_incl ~subtype:Subtype_zint n radix view t);
      }

    let node1_u_bitwidth =
      let open State.Integer_u_bitwidth in
      {
        edges1=map_of_cps_alist [
          (dec_cps, (fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_integer_u_bitwidth (state |> bitwidth_accum digit)) view t
            )
          );
        ];
        default1=(fun {n; radix; bitwidth} view t ->
          accept_integer_bitwidth_excl ~signedness:Unsigned ~bitwidth n radix view t);
        eoi1=(fun {n; radix; bitwidth} view t ->
          accept_integer_bitwidth_incl ~signedness:Unsigned ~bitwidth n radix view t);
      }

    let node1_i_bitwidth =
      let open State.Integer_i_bitwidth in
      {
        edges1=map_of_cps_alist [
          (dec_cps, (fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_integer_i_bitwidth (state |> bitwidth_accum digit)) view t
            )
          );
        ];
        default1=(fun {n; radix; bitwidth} view t ->
          accept_integer_bitwidth_excl ~signedness:Signed ~bitwidth n radix view t);
        eoi1=(fun {n; radix; bitwidth} view t ->
          accept_integer_bitwidth_incl ~signedness:Signed  ~bitwidth n radix view t);
      }

    let node0_mal_ident = {
      edges0=map_of_cps_alist [
        (ident_cps, advance State_integer_mal_ident);
      ];
      default0=accept_mal_indent_excl;
      eoi0=accept_mal_indent_incl;
    }
  end

  module Ident = struct
    let keyword_map = Map.of_alist (module String) [
      ("and", AbstractToken.Tok_and);
      ("also", Tok_also);
      ("as", Tok_as);
      ("conceal", Tok_conceal);
      ("effect", Tok_effect);
      ("else", Tok_else);
      ("expose", Tok_expose);
      ("external", Tok_external);
      ("false", Tok_false);
      ("fn", Tok_fn);
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
      ("when", Tok_when);
      ("with", Tok_with);
    ]

    let accept_uident cursor t =
      let uident_str = str_of_cursor cursor t in
      let tok = match Map.get uident_str keyword_map with
        | Some tok -> tok
        | None -> Tok_uident (Constant uident_str)
      in
      accept tok cursor t

    let accept_uident_incl View.{cursor; _} t =
      accept_uident cursor t

    let accept_uident_excl View.{pcursor; _} t =
      accept_uident pcursor t

    let accept_cident cursor t =
      let cident_str = str_of_cursor cursor t in
      accept (Tok_cident cident_str) cursor t

    let accept_cident_incl View.{cursor; _} t =
      accept_cident cursor t

    let accept_cident_excl View.{pcursor; _} t =
      accept_cident pcursor t

    let accept_mal cursor t =
      let uident_str = str_of_cursor cursor t in
      let description =
        String.Fmt.empty
        |> Fmt.fmt "Identifier "
        |> Fmt.fmt uident_str
        |> Fmt.fmt " lacks _*[A-Za-z] prefix"
        |> Fmt.to_string
      in
      let mal = malformed (malformation ~base:t.tok_base ~past:cursor description) in
      accept (Tok_uident mal) cursor t

    let accept_mal_incl View.{cursor; _} t =
      accept_mal cursor t

    let accept_mal_excl View.{pcursor; _} t =
      accept_mal pcursor t

    let node0_uscore = {
      edges0=map_of_cps_alist [
        ("_", advance State_ident_uscore);
        (ident_cident_cps, advance State_ident_cident);
        (ident_uident_cps, advance State_ident_uident);
        (ident_continue_cps, advance State_ident_mal);
      ];
      default0=accept_mal_excl;
      eoi0=accept_mal_incl;
    }

    let node0_uident = {
      edges0=map_of_cps_alist [
        (ident_cps, advance State_ident_uident);
      ];
      default0=accept_uident_excl;
      eoi0=accept_uident_incl;
    }

    let node0_cident = {
      edges0=map_of_cps_alist [
        (ident_cps, advance State_ident_cident);
      ];
      default0=accept_cident_excl;
      eoi0=accept_cident_incl;
    }

    let node0_mal = {
      edges0=map_of_cps_alist [
        (ident_cps, advance State_ident_mal);
      ];
      default0=accept_mal_excl;
      eoi0=accept_mal_incl;
    }
  end

  module Operator = struct
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

    let accept_operator f cursor t =
      let op = str_of_cursor cursor t in
      match Map.get op operator_map with
      | None -> accept (f op) cursor t
      | Some tok -> accept tok cursor t

    let accept_operator_incl f View.{cursor; _} t =
      accept_operator f cursor t

    let accept_operator_excl f View.{pcursor; _} t =
      accept_operator f pcursor t

    let node0_tilde = {
      edges0=map_of_cps_alist [(operator_cps, advance State_operator_tilde)];
      default0=accept_operator_excl (fun s -> Tok_tilde_op s);
      eoi0=accept_operator_incl (fun s -> Tok_tilde_op s);
    }

    let node0_qmark = {
      edges0=map_of_cps_alist [(operator_cps, advance State_operator_qmark)];
      default0=accept_operator_excl (fun s -> Tok_qmark_op s);
      eoi0=accept_operator_incl (fun s -> Tok_qmark_op s);
    }

    let node0_star_star = {
      edges0=map_of_cps_alist [(operator_cps, advance State_operator_star_star)];
      default0=accept_operator_excl (fun s -> Tok_star_star_op s);
      eoi0=accept_operator_incl (fun s -> Tok_star_star_op s);
    }

    let node0_star = {
      edges0=map_of_cps_alist [(operator_cps, advance State_operator_star)];
      default0=accept_operator_excl (fun s -> Tok_star_op s);
      eoi0=accept_operator_incl (fun s -> Tok_star_op s);
    }

    let node0_slash = {
      edges0=map_of_cps_alist [(operator_cps, advance State_operator_slash)];
      default0=accept_operator_excl (fun s -> Tok_slash_op s);
      eoi0=accept_operator_incl (fun s -> Tok_slash_op s);
    }

    let node0_pct = {
      edges0=map_of_cps_alist [(operator_cps, advance State_operator_pct)];
      default0=accept_operator_excl (fun s -> Tok_pct_op s);
      eoi0=accept_operator_incl (fun s -> Tok_pct_op s);
    }

    let node0_plus = {
      edges0=map_of_cps_alist [(operator_cps, advance State_operator_plus)];
      default0=accept_operator_excl (fun s -> Tok_plus_op s);
      eoi0=accept_operator_incl (fun s -> Tok_plus_op s);
    }

    let node0_minus = {
      edges0=map_of_cps_alist [(operator_cps, advance State_operator_minus)];
      default0=accept_operator_excl (fun s -> Tok_minus_op s);
      eoi0=accept_operator_incl (fun s -> Tok_minus_op s);
    }

    let node0_at = {
      edges0=map_of_cps_alist [(operator_cps, advance State_operator_at)];
      default0=accept_operator_excl (fun s -> Tok_at_op s);
      eoi0=accept_operator_incl (fun s -> Tok_at_op s);
    }

    let node0_caret = {
      edges0=map_of_cps_alist [(operator_cps, advance State_operator_caret)];
      default0=accept_operator_excl (fun s -> Tok_caret_op s);
      eoi0=accept_operator_incl (fun s -> Tok_caret_op s);
    }

    let node0_dollar = {
      edges0=map_of_cps_alist [(operator_cps, advance State_operator_dollar)];
      default0=accept_operator_excl (fun s -> Tok_dollar_op s);
      eoi0=accept_operator_incl (fun s -> Tok_dollar_op s);
    }

    let node0_lt = {
      edges0=map_of_cps_alist [(operator_cps, advance State_operator_lt)];
      default0=accept_operator_excl (fun s -> Tok_lt_op s);
      eoi0=accept_operator_incl (fun s -> Tok_lt_op s);
    }

    let node0_eq = {
      edges0=map_of_cps_alist [(operator_cps, advance State_operator_eq)];
      default0=accept_operator_excl (fun s -> Tok_eq_op s);
      eoi0=accept_operator_incl (fun s -> Tok_eq_op s);
    }

    let node0_gt = {
      edges0=map_of_cps_alist [(operator_cps, advance State_operator_gt)];
      default0=accept_operator_excl (fun s -> Tok_gt_op s);
      eoi0=accept_operator_incl (fun s -> Tok_gt_op s);
    }

    let node0_bar = {
      edges0=map_of_cps_alist [(operator_cps, advance State_operator_bar)];
      default0=accept_operator_excl (fun s -> Tok_bar_op s);
      eoi0=accept_operator_incl (fun s -> Tok_bar_op s);
    }

    let node0_colon = {
      edges0=map_of_cps_alist [(operator_cps, advance State_operator_colon)];
      default0=accept_operator_excl (fun s -> Tok_colon_op s);
      eoi0=accept_operator_incl (fun s -> Tok_colon_op s);
    }

    let node0_dot = {
      edges0=map_of_cps_alist [(operator_cps, advance State_operator_dot)];
      default0=accept_operator_excl (fun s -> Tok_dot_op s);
      eoi0=accept_operator_incl (fun s -> Tok_dot_op s);
    }
  end

  module ParenComment = struct
    let eoi1 _state View.{cursor; _} t =
      accept (Tok_paren_comment (malformed (unterminated_comment t.tok_base cursor))) cursor t

    let default1 state view t =
      advance (State_paren_comment_body state) view t

    let node1_body = {
      edges1=map_of_cps_alist [
        ("*", (fun state view t -> advance (State_paren_comment_star state) view t));
        ("(", (fun state view t -> advance (State_paren_comment_lparen state) view t));
      ];
      default1;
      eoi1;
    }

    let node1_lparen = {
      edges1=map_of_cps_alist [
        ("*", (fun State.Paren_comment_lparen.{nesting} view t ->
            advance (State_paren_comment_body (State.Paren_comment_body.init ~nesting:(succ
                nesting))) view t
          )
        );
        ("(", (fun state view t -> advance (State_paren_comment_lparen state) view t));
      ];
      default1;
      eoi1;
    }

    let node1_star = {
      edges1=map_of_cps_alist [
        ("*", (fun state view t -> advance (State_paren_comment_star state) view t));
        ("(", (fun state view t -> advance (State_paren_comment_lparen state) view t));
        (")", (fun {nesting} view t ->
            match nesting with
            | 1L -> accept_incl (Tok_paren_comment (Constant ())) view t
            | _ ->
              advance (State_paren_comment_body (State.Paren_comment_body.init
                  ~nesting:(pred nesting))) view t
          )
        );
      ];
      default1;
      eoi1;
    }
  end

  module SrcDirective = struct
    let render_source_directive ~mals ~path ~line ~indent ~omit =
      match mals with
      | [] -> begin
          let path = match path with
            | None -> None
            | Some cps -> Some (String.of_list_rev cps)
          in
          let line = match line with
            | None -> None
            | Some line -> Some (Nat.to_u64_hlt line)
          in
          let io = match indent, omit with
            | None, None -> None
            | Some indent, Some omit ->
              Some AbstractToken.{indent=(Nat.to_u64_hlt indent); omit=(Nat.to_u64_hlt omit)}
            | Some _, None
            | None, Some _ -> not_reached ()
          in
          AbstractToken.Tok_source_directive (Constant {path; line; io})
        end
      | _ :: _ -> AbstractToken.Tok_source_directive (AbstractToken.Rendition.of_mals mals)

    let node0_colon = {
      edges0=map_of_cps_alist [
        ("\"", advance (State_src_directive_path State.Src_directive_path.empty));
        ("_", advance (State_src_directive_path_colon State.Src_directive_path_colon.empty));
        ("123456789", (fun ({pcursor; _} as view) t ->
            let digit = nat_of_cp (Source.Cursor.rget pcursor) in
            advance (State_src_directive_line (State.Src_directive_line.init ~mals:[] ~path:None
              ~line_cursor:pcursor ~line:(Some digit))) view t));
        ("]", (fun view t ->
            let tok =
              render_source_directive ~mals:[] ~path:None ~line:None ~indent:None ~omit:None in
            accept_source_directive tok view t
          )
        );
      ];
      default0=(fun ({pcursor; cursor; _} as view) t ->
        let mal = unexpected_codepoint_source_directive pcursor cursor in
        advance (State_src_directive_line (State.Src_directive_line.init ~mals:[mal] ~path:None
          ~line_cursor:cursor ~line:None)) view t
      );
      eoi0=(fun {cursor; _} t ->
        let mal = unterminated_source_directive t.tok_base cursor in
        accept (Tok_source_directive (AbstractToken.Rendition.of_mals [mal])) cursor t
      );
    }

    let node1_path =
      let open State.Src_directive_path in
      let open View in
      {
        edges1=map_of_cps_alist [
          ("%", (fun state ({pcursor; cursor; _} as view) t ->
              let mal = missing_backslash pcursor cursor in
              advance (State_src_directive_path (state |> mals_accum mal)) view t
            )
          );
          ("\"", (fun state view t -> advance (State_src_directive_rditto state) view t));
          ("\\", (fun {mals; path} ({pcursor; _} as view) t ->
              advance (State_src_directive_path_bslash {mals; path; bslash_cursor=pcursor}) view t
            )
          );
        ];
        default1=(fun state ({pcursor; _} as view) t ->
          advance (State_src_directive_path (state |> path_accum (Source.Cursor.rget pcursor))) view
            t
        );
        eoi1=(fun {mals; _} {cursor; _} t ->
          let mal = unterminated_source_directive t.tok_base cursor in
          accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
        );
      }

    let node1_path_bslash =
      let open State.Src_directive_path_bslash in
      {
        edges1=map_of_cps_alist [
          ("u", (fun state view t -> advance (State_src_directive_path_bslash_u state) view t));
          ("t", (fun state view t ->
              advance (State_src_directive_path (state |> path_accum Codepoint.ht)) view t));
          ("n", (fun state view t ->
              advance (State_src_directive_path (state |> path_accum Codepoint.nl)) view t));
          ("r", (fun state view t ->
              advance (State_src_directive_path (state |> path_accum Codepoint.cr)) view t));
          ("\"\\%", (fun state ({pcursor; _} as view) t ->
              advance (State_src_directive_path (state |> path_accum (Source.Cursor.rget pcursor)))
                view t
            )
          );
        ];
        default1=(fun ({bslash_cursor; _} as state) {cursor; _} t ->
          let mal = illegal_backslash bslash_cursor cursor in
          retry (State_src_directive_path (state |> mals_accum mal)) t
        );
        eoi1=(fun {mals; _} {cursor; _} t ->
          let mal = unterminated_source_directive t.tok_base cursor in
          accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
        );
      }

    let node1_path_bslash_u =
      let open State.Src_directive_path_bslash_u in
      {
        edges1=map_of_cps_alist [
          ("{", (fun {mals; path; bslash_cursor} view t ->
              advance (State_src_directive_path_bslash_u_lcurly
                  (State.Src_directive_path_bslash_u_lcurly.init ~mals ~path ~bslash_cursor)) view t
            )
          );
          ("\"", (fun ({bslash_cursor; _} as state) ({cursor; _} as view) t ->
              let mal = invalid_unicode_escape bslash_cursor cursor in
              advance (State_src_directive_rditto (state |> mals_accum mal)) view t
            )
          );
          ("]", (fun {mals; _} {pcursor; cursor; _} t ->
              let mal = unexpected_codepoint_source_directive pcursor cursor in
              accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
            )
          );
        ];
        default1=(fun ({bslash_cursor; _} as state) ({cursor; _} as view) t ->
          let mal = invalid_unicode_escape bslash_cursor cursor in
          advance (State_src_directive_path (state |> mals_accum mal)) view t
        );
        eoi1=(fun {mals; _} {cursor; _} t ->
          let mal = unterminated_source_directive t.tok_base cursor in
          accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
        );
      }

    let node1_path_bslash_u_lcurly =
      let open State.Src_directive_path_bslash_u_lcurly in
      {
        edges1=map_of_cps_alist [
          ("_", (fun state view t ->
              advance (State_src_directive_path_bslash_u_lcurly state) view t));
          (hex_cps, (fun {mals; path; bslash_cursor} ({pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_src_directive_path_bslash_u_lcurly_hex
                  (State.Src_directive_path_bslash_u_lcurly_hex.init ~mals ~path ~bslash_cursor
                      ~hex:digit)) view t
            )
          );
          ("\"", (fun ({bslash_cursor; _} as state) ({cursor; _} as view) t ->
              let mal = invalid_unicode_escape bslash_cursor cursor in
              advance (State_src_directive_rditto (state |> mals_accum mal)) view t
            )
          );
          ("]", (fun {mals; _} {pcursor; cursor; _} t ->
              let mal = unexpected_codepoint_source_directive pcursor cursor in
              accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
            )
          );
        ];
        default1=(fun ({bslash_cursor; _} as state) ({cursor; _} as view) t ->
          let mal = invalid_unicode_escape bslash_cursor cursor in
          advance (State_src_directive_path (state |> mals_accum mal)) view t
        );
        eoi1=(fun {mals; _} {cursor; _} t ->
          let mal = unterminated_source_directive t.tok_base cursor in
          accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
        );
      }

    let node1_path_bslash_u_lcurly_hex =
      let open State.Src_directive_path_bslash_u_lcurly_hex in
      {
        edges1=map_of_cps_alist [
          ("_", (fun state view t ->
              advance (State_src_directive_path_bslash_u_lcurly_hex state) view t));
          (hex_cps, (fun state ({pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_src_directive_path_bslash_u_lcurly_hex (state |> hex_accum digit)) view
                t
            )
          );
          ("}", (fun ({bslash_cursor; hex; _} as state) ({cursor; _} as view) t ->
              advance (State_src_directive_path (
                Option.value_map (Nat.to_uns_opt hex)
                  ~f:(fun u -> Codepoint.narrow_of_uns_opt u)
                  ~default:None
                |> Option.value_map
                  ~f:(fun cp -> (state |> path_accum cp))
                  ~default:(state |> mals_accum (invalid_unicode_escape bslash_cursor cursor))
              )) view t
            )
          );
          ("\"", (fun ({bslash_cursor; _} as state) ({cursor; _} as view) t ->
              let mal = invalid_unicode_escape bslash_cursor cursor in
              advance (State_src_directive_rditto (state |> mals_accum mal)) view t
            )
          );
          ("]", (fun {mals; _} {pcursor; cursor; _} t ->
              let mal = unexpected_codepoint_source_directive pcursor cursor in
              accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
            )
          );
        ];
        default1=(fun ({bslash_cursor; _} as state) ({cursor; _} as view) t ->
          let mal = invalid_unicode_escape bslash_cursor cursor in
          advance (State_src_directive_path (state |> mals_accum mal)) view t
        );
        eoi1=(fun {mals; _} {cursor; _} t ->
          let mal = unterminated_source_directive t.tok_base cursor in
          accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
        );
      }

    let node1_rditto =
      let open State.Src_directive_rditto in
      {
        edges1=map_of_cps_alist [
          (":", (fun state view t -> advance (State_src_directive_path_colon state) view t));
          ("]", (fun {mals; path} view t ->
              let tok = render_source_directive ~mals ~path ~line:None ~indent:None ~omit:None in
              accept_source_directive tok view t
            )
          );
        ];
        default1=(fun state ({pcursor; cursor; _} as view) t ->
          let mal = unexpected_codepoint_source_directive pcursor cursor in
          advance (State_src_directive_rditto (state |> mals_accum mal)) view t
        );
        eoi1=(fun {mals; _} {cursor; _} t ->
          let mal = unterminated_source_directive t.tok_base cursor in
          accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
        );
      }

    let node1_path_colon =
      let open State.Src_directive_path_colon in
      let open View in
      {
        edges1=map_of_cps_alist [
          ("_", (fun state view t -> advance (State_src_directive_path_colon state) view t));
          ("123456789", (fun {mals; path} ({pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_src_directive_line (State.Src_directive_line.init ~mals ~path
                  ~line_cursor:pcursor ~line:(Some digit))) view t));
          ("]", (fun {mals; _} {pcursor; cursor; _} t ->
              let mal = unexpected_codepoint_source_directive pcursor cursor in
              accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
            )
          );
        ];
        default1=(fun state ({pcursor; cursor; _} as view) t ->
          let mal = unexpected_codepoint_source_directive pcursor cursor in
          advance (State_src_directive_rditto (state |> mals_accum mal)) view t
        );
        eoi1=(fun {mals; _} {cursor; _} t ->
          let mal = unterminated_source_directive t.tok_base cursor in
          accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
        );
      }

    let node1_line =
      let open State.Src_directive_line in
      let validate_line {mals; line_cursor; line; _} View.{pcursor; _} = begin
        match line with
        | Some line -> begin
            match Nat.(line > max_abs_i64) with
            | false -> mals, Some line
            | true -> begin
                let description =
                  String.Fmt.empty
                  |> Fmt.fmt "Line exceeds "
                  |> Nat.fmt ~alt:true Nat.max_abs_i64
                  |> Fmt.to_string
                in
                let mal = malformation ~base:line_cursor ~past:pcursor description in
                (mal :: mals), None
              end
          end
        | None -> mals, None
      end in
      {
        edges1=map_of_cps_alist [
          ("_", (fun state view t -> advance (State_src_directive_line state) view t));
          (dec_cps, (fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_src_directive_line (state |> line_accum digit)) view t
            )
          );
          (":", (fun ({path; _} as state) view t ->
              let mals, line = validate_line state view in
              advance (State_src_directive_line_colon (State.Src_directive_line_colon.init ~mals
                  ~path ~line)) view t
            )
          );
          ("]", (fun ({path; _} as state) view t ->
              let mals, line = validate_line state view in
              let tok = render_source_directive ~mals ~path ~line ~indent:None ~omit:None in
              accept_source_directive tok view t
            )
          );
        ];
        default1=(fun {mals; path; _} ({pcursor; cursor; _} as view) t ->
          let mal = unexpected_codepoint_source_directive pcursor cursor in
          advance (State_src_directive_line (init ~mals:(mal :: mals) ~path ~line_cursor:cursor
            ~line:None)) view t
        );
        eoi1=(fun {mals; _} {cursor; _} t ->
          let mal = unterminated_source_directive t.tok_base cursor in
          accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
        );
      }

    let node1_line_colon =
      let open State.Src_directive_line_colon in
      {
        edges1=map_of_cps_alist [
          ("_", (fun state view t -> advance (State_src_directive_line_colon state) view t));
          ("0", (fun {mals; path; line} view t ->
              advance (State_src_directive_indent_0 (State.Src_directive_indent_0.init ~mals ~path
                  ~line)) view t));
          ("123456789", (fun {mals; path; line} (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_src_directive_indent (State.Src_directive_indent.init ~mals ~path ~line
                  ~indent_cursor:pcursor ~indent:digit)) view t));
          ("]", (fun {mals; _} {pcursor; cursor; _} t ->
              let mal = unexpected_codepoint_source_directive pcursor cursor in
              accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
            )
          );
        ];
        default1=(fun state ({pcursor; cursor; _} as view) t ->
          let mal = unexpected_codepoint_source_directive pcursor cursor in
          advance (State_src_directive_line_colon (state |> mals_accum mal)) view t
        );
        eoi1=(fun {mals; _} {cursor; _} t ->
          let mal = unterminated_source_directive t.tok_base cursor in
          accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
        );
      }

    let node1_indent =
      let open State.Src_directive_indent in
      {
        edges1=map_of_cps_alist [
          ("_", (fun state view t -> advance (State_src_directive_indent state) view t));
          (dec_cps, (fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_src_directive_indent (state |> indent_accum digit)) view t
            )
          );
          ("+", (fun {mals; path; line; indent_cursor; indent} ({pcursor; _} as view) t ->
              let mals = match Nat.(indent % k_4 = k_0) with
                | true -> mals
                | false -> begin
                    let mal = malformation ~base:indent_cursor ~past:pcursor
                        "Indentation is not a multiple of 4" in
                    (mal :: mals)
                  end
              in
              let mals, indent = match Nat.(indent > max_abs_i64) with
                | false -> mals, Some indent
                | true -> begin
                    let description =
                      String.Fmt.empty
                      |> Fmt.fmt "Indentation exceeds "
                      |> Nat.fmt ~alt:true Nat.max_abs_i64
                      |> Fmt.to_string
                    in
                    let mal = malformation ~base:indent_cursor ~past:pcursor description in
                    (mal :: mals), None
                  end
              in
              advance (State_src_directive_indent_plus (State.Src_directive_indent_plus.init ~mals
                  ~path ~line ~indent)) view t
            )
          );
          ("]", (fun {mals; _} {pcursor; cursor; _} t ->
              let mal = unexpected_codepoint_source_directive pcursor cursor in
              accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
            )
          );
        ];
        default1=(fun state ({pcursor; cursor; _} as view) t ->
          let mal = unexpected_codepoint_source_directive pcursor cursor in
          advance (State_src_directive_indent (state |> mals_accum mal)) view t
        );
        eoi1=(fun {mals; _} {cursor; _} t ->
          let mal = unterminated_source_directive t.tok_base cursor in
          accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
        );
      }

    let node1_indent_0 =
      let open State.Src_directive_indent_0 in
      {
        edges1=map_of_cps_alist [
          ("_", (fun state view t -> advance (State_src_directive_indent_0 state) view t));
          ("+", (fun {mals; path; line} view t ->
              advance (State_src_directive_indent_plus (State.Src_directive_indent_plus.init ~mals
                  ~path ~line ~indent:(Some Nat.k_0))) view t
            )
          );
          ("]", (fun {mals; _} {pcursor; cursor; _} t ->
              let mal = unexpected_codepoint_source_directive pcursor cursor in
              accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
            )
          );
        ];
        default1=(fun state ({pcursor; cursor; _} as view) t ->
          let mal = unexpected_codepoint_source_directive pcursor cursor in
          advance (State_src_directive_indent_0 (state |> mals_accum mal)) view t
        );
        eoi1=(fun {mals; _} {cursor; _} t ->
          let mal = unterminated_source_directive t.tok_base cursor in
          accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
        );
      }

    let node1_indent_plus =
      let open State.Src_directive_indent_plus in
      {
        edges1=map_of_cps_alist [
          ("_", (fun state view t -> advance (State_src_directive_indent_plus state) view t));
          ("0", (fun {mals; path; line; indent} view t ->
              advance (State_src_directive_omit_0 (State.Src_directive_omit_0.init ~mals ~path
                  ~line ~indent)) view t));
          ("123456789", (fun {mals; path; line; indent} (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_src_directive_omit (State.Src_directive_omit.init ~mals ~path ~line
                  ~indent ~omit_cursor:pcursor ~omit:digit)) view t));
          ("]", (fun {mals; _} {pcursor; cursor; _} t ->
              let mal = unexpected_codepoint_source_directive pcursor cursor in
              accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
            )
          );
        ];
        default1=(fun state ({pcursor; cursor; _} as view) t ->
          let mal = unexpected_codepoint_source_directive pcursor cursor in
          advance (State_src_directive_indent_plus (state |> mals_accum mal)) view t
        );
        eoi1=(fun {mals; _} {cursor; _} t ->
          let mal = unterminated_source_directive t.tok_base cursor in
          accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
        );
      }

    let node1_omit =
      let open State.Src_directive_omit in
      {
        edges1=map_of_cps_alist [
          ("_", (fun state view t -> advance (State_src_directive_omit state) view t));
          (dec_cps, (fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_src_directive_omit (state |> omit_accum digit)) view t
            )
          );
          ("]", (fun {mals; path; line; indent; omit_cursor; omit} ({pcursor; _} as view) t ->
              let mals, omit = match Nat.(omit > max_abs_i64) with
                | false -> mals, Some omit
                | true -> begin
                    let description =
                      String.Fmt.empty
                      |> Fmt.fmt "Omitted columns exceeds "
                      |> Nat.fmt ~alt:true Nat.max_abs_i64
                      |> Fmt.to_string
                    in
                    let mal = malformation ~base:omit_cursor ~past:pcursor description in
                    (mal :: mals), None
                  end
              in
              let tok = render_source_directive ~mals ~path ~line ~indent ~omit in
              accept_source_directive tok view t
            )
          );
        ];
        default1=(fun state ({pcursor; cursor; _} as view) t ->
          let mal = unexpected_codepoint_source_directive pcursor cursor in
          advance (State_src_directive_omit (state |> mals_accum mal)) view t
        );
        eoi1=(fun {mals; _} {cursor; _} t ->
          let mal = unterminated_source_directive t.tok_base cursor in
          accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
        );
      }

    let node1_omit_0 =
      let open State.Src_directive_omit_0 in
      {
        edges1=map_of_cps_alist [
          ("_", (fun state view t -> advance (State_src_directive_omit_0 state) view t));
          ("]", (fun {mals; path; line; indent} view t ->
              let tok = render_source_directive ~mals ~path ~line ~indent ~omit:(Some Nat.k_0) in
              accept_source_directive tok view t
            )
          );
        ];
        default1=(fun state ({pcursor; cursor; _} as view) t ->
          let mal = unexpected_codepoint_source_directive pcursor cursor in
          advance (State_src_directive_omit_0 (state |> mals_accum mal)) view t
        );
        eoi1=(fun {mals; _} {cursor; _} t ->
          let mal = unterminated_source_directive t.tok_base cursor in
          accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
        );
      }
  end

  module Dentation = struct
    type alignment =
      | Aligned
      | Continued
      | Misaligned

    type level_change =
      | Level_dedent
      | Level_stable
      | Level_indent
      | Level_excess_indent

    let tok_indent = AbstractToken.Tok_indent (Constant ())
    let tok_dedent = AbstractToken.Tok_dedent (Constant ())

    let tok_missing_indent cursor =
      AbstractToken.Tok_indent (malformed (malformation ~base:cursor ~past:cursor "Missing indent"))

    let tok_missing_dedent cursor =
      AbstractToken.Tok_dedent (malformed (malformation ~base:cursor ~past:cursor "Missing dedent"))

    let accept_dentation atok cursor t =
      assert Source.Cursor.(t.tok_base = cursor);
      accept atok cursor t

    let other ~retry_state cursor t =
      let col = match t.line_state with
        | Line_begin
        | Line_whitespace -> Text.Pos.col (Source.Cursor.pos cursor)
        | Line_start_col col -> col
        | Line_body -> not_reached ()
      in
      (* Compute level an alignement such that the misaligned cases rounded to the nearest level.
      *)
      let level, alignment = match col / 4L, col % 4L with
        | floor_level, 0L -> Level.update floor_level t.level, Aligned
        | floor_level, 1L -> Level.update floor_level t.level, Misaligned
        | floor_level, 2L -> Level.update floor_level t.level, Continued
        | floor_level, 3L -> Level.update (succ floor_level) t.level, Misaligned
        | _ -> not_reached ()
      in
      let level_change = match Level.cmp t.level level with
        | Lt -> begin
            match Level.(=) (Level.succ t.level) level with
            | true -> Level_indent
            | false -> Level_excess_indent
          end
        | Eq -> Level_stable
        | Gt -> Level_dedent
      in
      (* The following patterns incrementally handle all dentation/alignment cases. Malformed tokens
       * are synthesized in error cases such that the cursor does not advance, but the level is
       * incrementally adjusted to converge. The overall result is that Tok_indent/Tok_dedent
       * nesting is always well formed. *)
      match t.block_state, t.line_state, level_change, alignment with
      (* New expression at same level. *)
      | Block_primal, (Line_begin|Line_whitespace|Line_start_col _), Level_stable, Aligned ->
        retry retry_state {t with level; block_state=Block_nonempty; line_state=Line_body}
      | Block_nonempty, (Line_begin|Line_whitespace|Line_start_col _), Level_stable, Aligned ->
        accept_dentation Tok_line_delim cursor {t with line_state=Line_body}

      (* Continued expression at current level. *)
      | Block_primal, (Line_begin|Line_whitespace|Line_start_col _), Level_stable, Continued ->
        accept_dentation Tok_misaligned cursor
          {t with block_state=Block_nonempty; line_state=Line_body}
      | Block_nonempty, (Line_begin|Line_whitespace|Line_start_col _), Level_stable, Continued ->
        retry retry_state {t with line_state=Line_body}

      (* New expression at higher level. *)
      | Block_primal, (Line_begin|Line_whitespace|Line_start_col _), Level_indent, Aligned ->
        accept_dentation (tok_missing_indent cursor) cursor
          {t with level=Level.succ t.level; block_state=Block_nonempty; line_state=Line_body}
      | Block_nonempty, (Line_begin|Line_whitespace|Line_start_col _), Level_indent, Aligned ->
        accept_dentation tok_indent cursor {t with level; line_state=Line_body}

      (* New/continued expression at lower level. *)
      | Block_primal, (Line_begin|Line_whitespace|Line_start_col _), Level_dedent,
        (Aligned|Continued) -> not_reached ()
      | Block_nonempty, (Line_begin|Line_whitespace|Line_start_col _), Level_dedent,
        (Aligned|Continued) ->
        accept_dentation tok_dedent cursor {t with level=Level.pred t.level}

      (* Misaligned at lower level. *)
      | Block_primal, (Line_begin|Line_whitespace|Line_start_col _), Level_dedent, Misaligned ->
        not_reached ()
      | Block_nonempty, (Line_begin|Line_whitespace|Line_start_col _), Level_dedent, Misaligned ->
        accept_dentation (tok_missing_dedent cursor) cursor {t with level=Level.pred t.level}

      (* Misaligned at current level. *)
      | Block_primal, (Line_begin|Line_whitespace|Line_start_col _), Level_stable, Misaligned ->
        accept_dentation Tok_misaligned cursor
          {t with block_state=Block_nonempty; line_state=Line_body}
      | Block_nonempty, (Line_begin|Line_whitespace|Line_start_col _), Level_stable, Misaligned ->
        accept_dentation Tok_misaligned cursor {t with line_state=Line_body}

      (* Excess indentation. *)
      | Block_primal, (Line_begin|Line_whitespace|Line_start_col _), Level_indent,
        (Continued|Misaligned)
      | Block_primal, (Line_begin|Line_whitespace|Line_start_col _), Level_excess_indent,
        (Aligned|Continued|Misaligned) ->
        accept_dentation (tok_missing_indent cursor) cursor
          {t with level=Level.succ t.level; block_state=Block_nonempty}
      | Block_nonempty, (Line_begin|Line_whitespace|Line_start_col _), Level_indent,
        (Continued|Misaligned)
      | Block_nonempty, (Line_begin|Line_whitespace|Line_start_col _),
        Level_excess_indent, (Aligned|Continued|Misaligned) ->
        accept_dentation (tok_missing_indent cursor) cursor {t with level=Level.succ t.level}

      | _, Line_body, _, _ -> not_reached ()

    let other_excl ~retry_state View.{pcursor; _} t =
      other ~retry_state pcursor t

    let other_pexcl ~retry_state View.{ppcursor; _} t =
      other ~retry_state ppcursor t

    let accept_whitespace cursor t =
      assert Source.Cursor.(t.tok_base < cursor);
      match t.line_state with
      | Line_begin -> accept Tok_whitespace cursor {t with line_state=Line_whitespace}
      | Line_start_col _ -> accept Tok_whitespace cursor t
      | Line_whitespace (* Consecutive whitespace tokens is impossible. *)
      | Line_body -> not_reached ()

    let accept_whitespace_incl View.{cursor; _} t =
      accept_whitespace cursor t

    let accept_whitespace_excl View.{pcursor; _} t =
      accept_whitespace pcursor t

    let advance ?line_state state view t =
      let t' = match line_state with
        | None -> t
        | Some line_state -> {t with line_state}
      in
      advance state view t'

    let node0_start = {
      edges0=map_of_cps_alist [
        (" ", advance State_dentation_space);
        ("#", advance ~line_state:Line_body State_hash_comment);
        ("(", advance State_dentation_lparen);
        ("\n", (fun view t ->
            match t.line_state with
            | Line_begin -> accept_incl Tok_whitespace view t
            | Line_whitespace
            | Line_start_col _ -> accept_line_break_incl Tok_whitespace view t
            | Line_body -> not_reached ()
          )
        );
      ];
      default0=other_excl ~retry_state:State_start;
      eoi0=(fun view t ->
        match t.line_state, Level.level t.level with
        | (Line_begin|Line_whitespace|Line_start_col _), 0L -> accept_incl Tok_end_of_input view t
        | (Line_begin|Line_whitespace|Line_start_col _), t_level ->
          accept_incl (Tok_dedent (Constant ())) view {t with level=Level.update (pred t_level)
            t.level}
        | _ -> not_reached ()
      );
    }

    let node0_lparen = {
      edges0=map_of_cps_alist [
        ("*", (fun (View.{ppcursor; _} as view) t ->
            match t.line_state with
            | Line_begin
            | Line_whitespace -> begin
                let col = Text.Pos.col (Source.Cursor.pos ppcursor) in
                advance ~line_state:(Line_start_col col) (State_paren_comment_body
                    (State.Paren_comment_body.init ~nesting:1L)) view t
              end
            | Line_start_col _ ->
              advance (State_paren_comment_body (State.Paren_comment_body.init ~nesting:1L)) view t
            | Line_body -> not_reached ()
          )
        );
      ];
      default0=other_pexcl ~retry_state:State_lparen;
      eoi0=other_excl ~retry_state:State_lparen;
    }

    let node0_space = {
      edges0=map_of_cps_alist [
        (" ", advance State_dentation_space);
        ("\n", accept_line_break_incl Tok_whitespace);
      ];
      default0=accept_whitespace_excl;
      eoi0=accept_whitespace_incl;
    }
  end

  let node0_whitespace = {
    edges0=map_of_cps_alist [
      (" ", advance State_whitespace);
      ("\n", accept_line_break_incl Tok_whitespace);
    ];
    default0=accept_excl Tok_whitespace;
    eoi0=accept_incl Tok_whitespace;
  }

  let node0_hash_comment = {
    edges0=map_of_cps_alist [
      ("\n", accept_line_break_incl Tok_hash_comment);
    ];
    default0=advance State_hash_comment;
    eoi0=accept_line_break_incl Tok_hash_comment;
  }

  module Codepoint_ = struct
    let eoi0 View.{cursor; _} t =
      let mal = unterminated_codepoint t.tok_base cursor in
      accept (Tok_codepoint (AbstractToken.Rendition.of_mals [mal])) cursor t

    let eoi1 _state view t =
      eoi0 view t

    let node0_tick = {
      edges0=map_of_cps_alist [
        ("\\", (fun (View.{pcursor=bslash_cursor; _} as view) t ->
            advance (State_codepoint_bslash (State.Codepoint_bslash.init ~bslash_cursor)) view t));
        ("'", (fun ({cursor; _} as view) t ->
            let mal = empty_codepoint t.tok_base cursor in
            accept_incl (Tok_codepoint (AbstractToken.Rendition.of_mals [mal])) view t
          )
        );
      ];
      default0=(fun (View.{pcursor; _} as view) t ->
        let cp = Source.Cursor.rget pcursor in
        advance (State.State_codepoint_cp (State.Codepoint_cp.init ~cp)) view t
      );
      eoi0;
    }

    let node1_bslash =
      let open State.Codepoint_bslash in
      {
        edges1=map_of_cps_alist [
          ("u", (fun state view t -> advance (State_codepoint_bslash_u state) view t));
          ("t", (fun _state view t ->
              advance (State_codepoint_cp (State.Codepoint_cp.init ~cp:Codepoint.ht)) view t));
          ("n", (fun _state view t ->
              advance (State_codepoint_cp (State.Codepoint_cp.init ~cp:Codepoint.nl)) view t));
          ("r", (fun _state view t ->
              advance (State_codepoint_cp (State.Codepoint_cp.init ~cp:Codepoint.cr)) view t));
          ("'\\", (fun _state ({pcursor; _} as view) t ->
              let cp = Source.Cursor.rget pcursor in
              advance (State_codepoint_cp (State.Codepoint_cp.init ~cp)) view t
            )
          );
        ];
        default1=(fun {bslash_cursor} (View.{cursor; _} as view) t ->
          let mal = illegal_backslash bslash_cursor cursor in
          advance (State.State_codepoint_mal (State.Codepoint_mal.init mal)) view t
        );
        eoi1;
      }

    let node1_bslash_u = {
      edges1=map_of_cps_alist [
        ("{", (fun state view t -> advance (State_codepoint_bslash_u_lcurly state) view t));
      ];
      default1=(fun {bslash_cursor} (View.{cursor; _} as view) t ->
        let mal = invalid_unicode_escape bslash_cursor cursor in
        advance (State.State_codepoint_mal (State.Codepoint_mal.init mal)) view t
      );
      eoi1;
    }

    let node1_bslash_u_lcurly = {
      edges1=map_of_cps_alist [
        ("_", (fun state view t -> advance (State_codepoint_bslash_u_lcurly state) view t));
        (hex_cps, (fun {bslash_cursor} (View.{pcursor; _} as view) t ->
            let digit = nat_of_cp (Source.Cursor.rget pcursor) in
            advance (State_codepoint_bslash_u_lcurly_hex (State.Codepoint_bslash_u_lcurly_hex.init
                ~bslash_cursor ~hex:digit)) view t
          )
        );
      ];
      default1=(fun {bslash_cursor} (View.{cursor; _} as view) t ->
        let mal = invalid_unicode_escape bslash_cursor cursor in
        advance (State.State_codepoint_mal (State.Codepoint_mal.init mal)) view t
      );
      eoi1;
    }

    let node1_bslash_u_lcurly_hex =
      let open State.Codepoint_bslash_u_lcurly_hex in
      {
        edges1=map_of_cps_alist [
          ("_", (fun state view t -> advance (State_codepoint_bslash_u_lcurly_hex state) view t));
          (hex_cps, (fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_codepoint_bslash_u_lcurly_hex (state |> hex_accum digit)) view t
            )
          );
          ("}", (fun {bslash_cursor; hex} ({cursor; _} as view) t ->
              advance (
                Option.value_map (Nat.to_uns_opt hex)
                  ~f:(fun u -> Codepoint.narrow_of_uns_opt u)
                  ~default:None
                |> Option.value_map
                  ~f:(fun cp -> State.State_codepoint_cp (State.Codepoint_cp.init ~cp))
                  ~default:(State.State_codepoint_mal (State.Codepoint_mal.init
                      (invalid_unicode_escape bslash_cursor cursor)))
              ) view t
            )
          );
        ];
        default1=(fun {bslash_cursor; _} (View.{cursor; _} as view) t ->
          let mal = invalid_unicode_escape bslash_cursor cursor in
          advance (State.State_codepoint_mal (State.Codepoint_mal.init mal)) view t
        );
        eoi1;
      }

    let node1_cp =
      let open State.Codepoint_cp in
      {
        edges1=map_of_cps_alist [
          ("'", (fun {cp} View.{cursor; _} t -> accept (Tok_codepoint (Constant cp)) cursor t));
        ];
        default1=(fun _state (View.{pcursor; cursor; _} as view) t ->
          let mal = excess_codepoint pcursor cursor in
          advance (State.State_codepoint_mal (State.Codepoint_mal.init mal)) view t
        );
        eoi1;
      }

    let node1_mal =
      let open State.Codepoint_mal in
      {
        edges1=map_of_cps_alist [
          ("'", (fun {mal} View.{cursor; _} t ->
              accept (Tok_codepoint (AbstractToken.Rendition.of_mals [mal])) cursor t));
        ];
        default1=(fun state view t -> advance (State.State_codepoint_mal state) view t);
        eoi1=(fun {mal} View.{cursor; _} t ->
          let mal2 = unterminated_codepoint t.tok_base cursor in
          accept (Tok_codepoint (AbstractToken.Rendition.of_mals (mal2 :: [mal]))) cursor t
        );
      }
  end

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
      let cp = Source.Cursor.rget pcursor in
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
      let mal = illegal_backslash bslash_cursor cursor in
      t, Retry (State_isubstring_start {accum=State.CodepointAccum.accum_mal mal accum})
    end in
    let default1 {accum; bslash_cursor} {cursor; _} t = begin
      accum_illegal_backslash accum bslash_cursor cursor t
    end in
    let accum_cp cp {accum; _} view t = begin
      advance (State_isubstring_start {accum=State.CodepointAccum.accum_cp cp accum}) view t
    end in
    let accum_raw state ({pcursor; _} as view) t = begin
      let cp = Source.Cursor.rget pcursor in
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
      let mal = illegal_backslash bslash_cursor cursor in
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
            let mal = illegal_backslash bslash_cursor cursor in
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
      let mal = invalid_unicode bslash_cursor cursor in
      t, Retry (State_isubstring_start {accum=State.CodepointAccum.accum_mal mal accum})
    end in
    let accum_invalid_unicode_escape accum bslash_cursor cursor t = begin
      let mal = invalid_unicode_escape bslash_cursor cursor in
      t, Retry (State_isubstring_start {accum=State.CodepointAccum.accum_mal mal accum})
    end in
    let default1 {accum; bslash_cursor; _} {pcursor; _} t = begin
      accum_invalid_unicode_escape accum bslash_cursor pcursor t
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
        (hex_cps, (fun {accum; bslash_cursor; u} ({pcursor; _} as view) t ->
            let cp = Source.Cursor.rget pcursor in
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
            let mal = invalid_unicode_escape bslash_cursor cursor in
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
      (* ("'", XXX); *)
      ("(", advance State_spec_lparen)
    ]; default0; eoi0;
  }

  let node0_spec_lparen = {
    edges0=map_of_cps_alist [
      ("^", accept ~f:(accept_istring_trans Istring_expr_width Tok_istring_lparen_caret));
    ]; default0; eoi0;
  }

  let transition_of_state trace state view t =
    match state with
    | State.State_start -> act0 trace node0_start view t
    | State_semi -> act0 trace node0_semi view t
    | State_lparen -> act0 trace node0_lparen view t
    | State_lbrack -> act0 trace node0_lbrack view t
    | State_tilde -> act0 trace node0_tilde view t
    | State_qmark -> act0 trace node0_qmark view t
    | State_star -> act0 trace node0_star view t
    | State_caret -> act0 trace node0_caret view t
    | State_bar -> act0 trace node0_bar view t
    | State_uscore -> act0 trace node0_uscore view t
    | State_tick -> act0 trace node0_tick view t
    | State_tick_lookahead -> act0 trace node0_tick_lookahead view t
    | State_btick -> act0 trace node0_btick view t
    | State_integer_0 -> act0 trace Integer.node0_0 view t
    | State_integer_0_dot -> act0 trace Integer.node0_0_dot view t
    | State_integer_0b -> act0 trace Integer.node0_0b view t
    | State_integer_0o -> act0 trace Integer.node0_0o view t
    | State_integer_0x -> act0 trace Integer.node0_0x view t
    | State_integer_bin v -> act1 trace Integer.node1_bin v view t
    | State_integer_oct v -> act1 trace Integer.node1_oct v view t
    | State_integer_dec v -> act1 trace Integer.node1_dec v view t
    | State_integer_hex v -> act1 trace Integer.node1_hex v view t
    | State_integer_bin_dot v -> act1 trace Integer.node1_bin_dot v view t
    | State_integer_oct_dot v -> act1 trace Integer.node1_oct_dot v view t
    | State_integer_dec_dot v -> act1 trace Integer.node1_dec_dot v view t
    | State_integer_hex_dot v -> act1 trace Integer.node1_hex_dot v view t
    | State_integer_u v -> act1 trace Integer.node1_u v view t
    | State_integer_i v -> act1 trace Integer.node1_i v view t
    | State_integer_n v -> act1 trace Integer.node1_n v view t
    | State_integer_z v -> act1 trace Integer.node1_z v view t
    | State_integer_u_bitwidth v -> act1 trace Integer.node1_u_bitwidth v view t
    | State_integer_i_bitwidth v -> act1 trace Integer.node1_i_bitwidth v view t
    | State_integer_mal_ident -> act0 trace Integer.node0_mal_ident view t
    | State_ident_uscore -> act0 trace Ident.node0_uscore view t
    | State_ident_uident -> act0 trace Ident.node0_uident view t
    | State_ident_cident -> act0 trace Ident.node0_cident view t
    | State_ident_mal -> act0 trace Ident.node0_mal view t
    | State_operator_tilde -> act0 trace Operator.node0_tilde view t
    | State_operator_qmark -> act0 trace Operator.node0_qmark view t
    | State_operator_star_star -> act0 trace Operator.node0_star_star view t
    | State_operator_star -> act0 trace Operator.node0_star view t
    | State_operator_slash -> act0 trace Operator.node0_slash view t
    | State_operator_pct -> act0 trace Operator.node0_pct view t
    | State_operator_plus -> act0 trace Operator.node0_plus view t
    | State_operator_minus -> act0 trace Operator.node0_minus view t
    | State_operator_at -> act0 trace Operator.node0_at view t
    | State_operator_caret -> act0 trace Operator.node0_caret view t
    | State_operator_dollar -> act0 trace Operator.node0_dollar view t
    | State_operator_lt -> act0 trace Operator.node0_lt view t
    | State_operator_eq -> act0 trace Operator.node0_eq view t
    | State_operator_gt -> act0 trace Operator.node0_gt view t
    | State_operator_bar -> act0 trace Operator.node0_bar view t
    | State_operator_colon -> act0 trace Operator.node0_colon view t
    | State_operator_dot -> act0 trace Operator.node0_dot view t
    | State_paren_comment_body v -> act1 trace ParenComment.node1_body v view t
    | State_paren_comment_lparen v -> act1 trace ParenComment.node1_lparen v view t
    | State_paren_comment_star v -> act1 trace ParenComment.node1_star v view t
    | State_src_directive_colon -> act0 trace SrcDirective.node0_colon view t
    | State_src_directive_path v -> act1 trace SrcDirective.node1_path v view t
    | State_src_directive_path_bslash v -> act1 trace SrcDirective.node1_path_bslash v view t
    | State_src_directive_path_bslash_u v -> act1 trace SrcDirective.node1_path_bslash_u v view t
    | State_src_directive_path_bslash_u_lcurly v ->
      act1 trace SrcDirective.node1_path_bslash_u_lcurly v view t
    | State_src_directive_path_bslash_u_lcurly_hex v ->
      act1 trace SrcDirective.node1_path_bslash_u_lcurly_hex v view t
    | State_src_directive_rditto v -> act1 trace SrcDirective.node1_rditto v view t
    | State_src_directive_path_colon v -> act1 trace SrcDirective.node1_path_colon v view t
    | State_src_directive_line v -> act1 trace SrcDirective.node1_line v view t
    | State_src_directive_line_colon v -> act1 trace SrcDirective.node1_line_colon v view t
    | State_src_directive_indent v -> act1 trace SrcDirective.node1_indent v view t
    | State_src_directive_indent_0 v -> act1 trace SrcDirective.node1_indent_0 v view t
    | State_src_directive_indent_plus v -> act1 trace SrcDirective.node1_indent_plus v view t
    | State_src_directive_omit v -> act1 trace SrcDirective.node1_omit v view t
    | State_src_directive_omit_0 v -> act1 trace SrcDirective.node1_omit_0 v view t
    | State_dentation_start -> act0 trace Dentation.node0_start view t
    | State_dentation_lparen -> act0 trace Dentation.node0_lparen view t
    | State_dentation_space -> act0 trace Dentation.node0_space view t
    | State_whitespace -> act0 trace node0_whitespace view t
    | State_hash_comment -> act0 trace node0_hash_comment view t
    | State_codepoint_tick -> act0 trace Codepoint_.node0_tick view t
    | State_codepoint_bslash v -> act1 trace Codepoint_.node1_bslash v view t
    | State_codepoint_bslash_u v -> act1 trace Codepoint_.node1_bslash_u v view t
    | State_codepoint_bslash_u_lcurly v -> act1 trace Codepoint_.node1_bslash_u_lcurly v view t
    | State_codepoint_bslash_u_lcurly_hex v ->
      act1 trace Codepoint_.node1_bslash_u_lcurly_hex v view t
    | State_codepoint_cp v -> act1 trace Codepoint_.node1_cp v view t
    | State_codepoint_mal v -> act1 trace Codepoint_.node1_mal v view t
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
    match transition trace state (view_of_t t) t with
    | _, Advance _ -> not_reached ()
    | _, Retry _ -> not_reached ()
    | t', Accept token -> t', token
end

let next t =
  assert (Istring_expr_value t.tok_base |> (fun _ -> true)); (* XXX Remove. *)
  let trace = None in
  let _trace = Some true in
  match t.line_state, t.istring_state with
  | Line_begin, _
  | Line_whitespace, _
  | Line_start_col _, _ -> Dfa.next ?trace State.dentation_start t
  | Line_body, [] -> Dfa.next ?trace State.start t
  | Line_body, istring_state :: _ -> begin
      match istring_state with
      | Istring_interp -> Dfa.next ?trace State.isubstring_start t
      | Istring_spec_pct -> Dfa.next ?trace State.spec_start t
      | Istring_rditto -> Dfa.next ?trace State.rditto_start t
      | Istring_expr_width
      | _ -> not_implemented "XXX"
    end
