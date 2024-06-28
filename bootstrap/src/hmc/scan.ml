(* The scanner is based on a DFA, but it has several augmentations:
 *
 * - Nodes can take state as input. The `node0` nodes take no state; the `node1` nodes do. This
 *   mechanism is used for per token state, e.g. tracking nesting level for comments and
 *   accumulating digits/codepoints for numerical/string constants.
 * - Block/line state drives the emission of `Tok_{line_delim,indent,dedent}` tokens. In some cases
 *   such tokens are emitted without even advancing the input view, e.g. when emitting a sequence of
 *   dedents.
 * - Fstring state drives starting state transitions as (potentially nested) fstring token sequences
 *   are emitted.
 *
 * DFA operation can be traced by enabling `trace` in the `next` function (near the end of this
 * source file). The trace output is detailed enough to diagnose nearly any scanner flaw, as well as
 * to aid understanding of how a particular token is scanned.
 *
 * Most language syntax changes require modifying the DFA, but keywords and special operators can be
 * added/removed with low effort.
 *
 * - Keywords are mapped to tokens in `Dfa.Ident.keyword_map`.
 * - Special operators are mapped to tokens in `Dfa.Operator.operator_map`.
 *
 * Adding/removing a DFA state may require changes in several places:
 *
 * - The `State.t` type must have a constructor which corresponds to the state.
 * - If the state is parametric (i.e. the corresponding node takes state as input), then there must
 *   be a corresponding `State.<Param>` module. The module must provide a pretty printer for its
 *   type; this supports the DFA tracing functionality.
 * - A `node0` or `node1` record with the appropriate behavior must be provided.
 * - `Dfa.transition_of_state` must have an entry that associates the constructor with the node
 *   record.
 *
 * The DFA data structures are created during module loading. This makes it possible to operate on
 * maps/sets rather than having to explicitly enumerate every relevant codepoint in matches. Were
 * the DFA machine-generated it would probably be better to use match expressions rather than map
 * lookups.
 *
 * Mutually recursive functions would be a viable alternative to the DFA driver, but that would
 * require all the functions corresponding to nodes to be in the same module. The code as written
 * uses submodules to delineate machinery for the various kinds of tokens. Mutual recursion would
 * also require repetitive boilerplate in each node's function to handle end of input and whatnot.
 * This approach might be reasonable if machine-generated, but not for manually written code. *)

open Basis
open Basis.Rudiments

(* Various codepoint classes, used as keys in the nodes' `edges[01]` transition maps. Most nodes
 * construct their maps using `map_of_cps_alist`, but more involved nodes use `map_of_cpsets_alist`
 * so that `Set` operations can be used in constructing the codepoint classes. *)
let operator_prefix_lead_cps = "~?"
let operator_infix_lead_cps = "-+*/%@^$<=>|:."
let operator_cps = String.join [operator_infix_lead_cps; operator_prefix_lead_cps]

let bin_lead_cps = "1"
let bin_cps = String.join ["0"; bin_lead_cps]
let oct_lead_cps = String.join [bin_lead_cps; "234567"]
let oct_cps = String.join ["0"; oct_lead_cps]
let dec_lead_cps = String.join [oct_lead_cps; "89"]
let dec_cps = String.join ["0"; dec_lead_cps]
let hex_lead_cps = String.join [dec_lead_cps; "abcdef"]
let hex_cps = String.join ["0"; hex_lead_cps]

let ident_cident_cps = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let ident_uident_cps = "abcdefghijklmnopqrstuvwxyz"
let ident_continue_cps = String.join [dec_cps; "'"]
let ident_cps = String.join ["_"; ident_cident_cps; ident_uident_cps; ident_continue_cps]

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

let real_digit_map = String.foldi ~init:(Map.empty (module Codepoint))
  ~f:(fun i digit_map cp ->
    Map.insert_hlt ~k:cp ~v:(Uns.to_real i) digit_map
  ) hex_cps

let real_of_cp digit =
  Map.get_hlt digit real_digit_map

let realer_of_nat nat =
  let sig_bits = (Nat.bit_length nat) - (Nat.bit_clz nat) in
  let exponent = match sig_bits with
    | 0L -> Zint.zero
    | _ -> Zint.of_uns (pred sig_bits)
  in
  Realer.create ~sign:Realer.Pos ~exponent ~mantissa:nat

module Radix = struct
  include Radix

  let to_nat t =
    (to_uns t) |> Uns.extend_to_nat

  let accum_nat digit nat t =
    Nat.(nat * (to_nat t) + digit)
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
        |> Fmt.fmt "(Constant "
        |> pp_a a
        |> Fmt.fmt ")"
      | Malformed malformations ->
        formatter
        |> Fmt.fmt "(Malformed "
        |> (List.pp Malformation.pp) malformations
        |> Fmt.fmt ")"

    let pp_unit tok_name t formatter =
      formatter
      |> Fmt.fmt tok_name
      |> (function formatter -> match t with
        | Constant _ -> formatter
        | Malformed _ -> formatter |> Fmt.fmt " " |> pp Unit.pp t
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
    path: Path.t option;
    line: uns option;
    io: indent_omit option;
  }

  let pp_source_directive {path; line; io} formatter =
    formatter
    |> Fmt.fmt "{path=" |> (Option.pp Path.pp) path
    |> Fmt.fmt "; line=" |> (Option.pp Uns.pp) line
    |> Fmt.fmt "; io=" |> (Option.pp pp_indent_omit) io
    |> Fmt.fmt "}"

  type fmt =
    | Fmt_b
    | Fmt_u
    | Fmt_u8
    | Fmt_u16
    | Fmt_u32
    | Fmt_u64
    | Fmt_u128
    | Fmt_u256
    | Fmt_u512
    | Fmt_i
    | Fmt_i8
    | Fmt_i16
    | Fmt_i32
    | Fmt_i64
    | Fmt_i128
    | Fmt_i256
    | Fmt_i512
    | Fmt_n
    | Fmt_z
    | Fmt_r
    | Fmt_r32
    | Fmt_r64
    | Fmt_c
    | Fmt_s
    | Fmt_f

  let pp_fmt fmt formatter =
    formatter |> Fmt.fmt (
      match fmt with
      | Fmt_b -> "Fmt_b"
      | Fmt_u -> "Fmt_u"
      | Fmt_u8 -> "Fmt_u8"
      | Fmt_u16 -> "Fmt_u16"
      | Fmt_u32 -> "Fmt_u32"
      | Fmt_u64 -> "Fmt_u64"
      | Fmt_u128 -> "Fmt_u128"
      | Fmt_u256 -> "Fmt_u256"
      | Fmt_u512 -> "Fmt_u512"
      | Fmt_i -> "Fmt_i"
      | Fmt_i8 -> "Fmt_i8"
      | Fmt_i16 -> "Fmt_i16"
      | Fmt_i32 -> "Fmt_i32"
      | Fmt_i64 -> "Fmt_i64"
      | Fmt_i128 -> "Fmt_i128"
      | Fmt_i256 -> "Fmt_i256"
      | Fmt_i512 -> "Fmt_i512"
      | Fmt_n -> "Fmt_n"
      | Fmt_z -> "Fmt_z"
      | Fmt_r -> "Fmt_r"
      | Fmt_r32 -> "Fmt_r32"
      | Fmt_r64 -> "Fmt_r64"
      | Fmt_c -> "Fmt_c"
      | Fmt_s -> "Fmt_s"
      | Fmt_f -> "Fmt_f"
    )

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
    | Tok_dot_dot
    | Tok_semi
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
    | Tok_rstring of string Rendition.t
    | Tok_istring of string Rendition.t
    | Tok_fstring_lditto
    | Tok_fstring_interpolated of string Rendition.t
    | Tok_fstring_pct
    | Tok_fstring_pad of codepoint Rendition.t
    | Tok_fstring_just of Fmt.just
    | Tok_fstring_sign of Fmt.sign
    | Tok_fstring_alt
    | Tok_fstring_zpad
    | Tok_fstring_width_star
    | Tok_fstring_width of uns Rendition.t
    | Tok_fstring_pmode of Fmt.pmode
    | Tok_fstring_precision_star
    | Tok_fstring_precision of uns Rendition.t
    | Tok_fstring_radix of Radix.t
    | Tok_fstring_notation of Fmt.notation
    | Tok_fstring_pretty
    | Tok_fstring_fmt of fmt Rendition.t
    | Tok_fstring_sep of string Rendition.t
    | Tok_fstring_label of string
    | Tok_fstring_lparen_caret
    | Tok_fstring_caret_rparen
    | Tok_fstring_rditto
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
    | Tok_zint of Zint.t Rendition.t
    | Tok_end_of_input
    | Tok_misaligned
    | Tok_error of Rendition.Malformation.t list

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
      | Tok_tilde_op op -> formatter |> Fmt.fmt "Tok_tilde_op " |> String.pp op
      | Tok_qmark_op op -> formatter |> Fmt.fmt "Tok_qmark_op " |> String.pp op
      | Tok_star_star_op op -> formatter |> Fmt.fmt "Tok_star_star_op " |> String.pp op
      | Tok_star_op op -> formatter |> Fmt.fmt "Tok_star_op " |> String.pp op
      | Tok_slash_op op -> formatter |> Fmt.fmt "Tok_slash_op " |> String.pp op
      | Tok_pct_op op -> formatter |> Fmt.fmt "Tok_pct_op " |> String.pp op
      | Tok_plus_op op -> formatter |> Fmt.fmt "Tok_plus_op " |> String.pp op
      | Tok_minus_op op -> formatter |> Fmt.fmt "Tok_minus_op " |> String.pp op
      | Tok_at_op op -> formatter |> Fmt.fmt "Tok_at_op " |> String.pp op
      | Tok_caret_op op -> formatter |> Fmt.fmt "Tok_caret_op " |> String.pp op
      | Tok_dollar_op op -> formatter |> Fmt.fmt "Tok_dollar_op " |> String.pp op
      | Tok_lt_op op -> formatter |> Fmt.fmt "Tok_lt_op " |> String.pp op
      | Tok_eq_op op -> formatter |> Fmt.fmt "Tok_eq_op " |> String.pp op
      | Tok_gt_op op -> formatter |> Fmt.fmt "Tok_gt_op " |> String.pp op
      | Tok_bar_op op -> formatter |> Fmt.fmt "Tok_bar_op " |> String.pp op
      | Tok_colon_op op -> formatter |> Fmt.fmt "Tok_colon_op " |> String.pp op
      | Tok_dot_op op -> formatter |> Fmt.fmt "Tok_dot_op " |> String.pp op

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
      | Tok_dot_dot -> formatter |> Fmt.fmt "Tok_dot_dot"
      | Tok_semi -> formatter |> Fmt.fmt "Tok_semi"
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
        formatter |> Fmt.fmt "Tok_source_directive "
        |> (Rendition.pp pp_source_directive) rendition
      | Tok_line_delim -> formatter |> Fmt.fmt "Tok_line_delim"
      | Tok_indent rendition -> formatter |> Rendition.pp_unit "Tok_indent" rendition
      | Tok_dedent rendition -> formatter |> Rendition.pp_unit "Tok_dedent" rendition
      | Tok_whitespace -> formatter |> Fmt.fmt "Tok_whitespace"
      | Tok_hash_comment -> formatter |> Fmt.fmt "Tok_hash_comment"
      | Tok_paren_comment rendition -> formatter |> Rendition.pp_unit "Tok_paren_comment" rendition
      | Tok_uscore -> formatter |> Fmt.fmt "Tok_uscore"
      | Tok_uident rendition ->
        formatter |> Fmt.fmt "Tok_uident " |> (Rendition.pp String.pp) rendition
      | Tok_cident cident -> formatter |> Fmt.fmt "Tok_cident " |> String.pp cident
      | Tok_codepoint rendition ->
        formatter |> Fmt.fmt "Tok_codepoint " |> (Rendition.pp Codepoint.pp) rendition
      | Tok_rstring rendition ->
        formatter |> Fmt.fmt "Tok_rstring " |> (Rendition.pp String.pp) rendition
      | Tok_istring rendition ->
        formatter |> Fmt.fmt "Tok_istring " |> (Rendition.pp String.pp) rendition
      | Tok_fstring_lditto -> formatter |> Fmt.fmt "Tok_fstring_lditto"
      | Tok_fstring_interpolated rendition ->
        formatter |> Fmt.fmt "Tok_fstring_interpolated " |> (Rendition.pp String.pp) rendition
      | Tok_fstring_pct -> formatter |> Fmt.fmt "Tok_fstring_pct"
      | Tok_fstring_pad rendition ->
        formatter |> Fmt.fmt "Tok_fstring_pad " |> (Rendition.pp Codepoint.pp) rendition
      | Tok_fstring_just just -> formatter |> Fmt.fmt "Tok_fstring_just " |> Fmt.pp_just just
      | Tok_fstring_sign sign -> formatter |> Fmt.fmt "Tok_fstring_sign " |> Fmt.pp_sign sign
      | Tok_fstring_alt -> formatter |> Fmt.fmt "Tok_fstring_alt"
      | Tok_fstring_zpad -> formatter |> Fmt.fmt "Tok_fstring_zpad"
      | Tok_fstring_width_star -> formatter |> Fmt.fmt "Tok_fstring_width_star"
      | Tok_fstring_width rendition ->
        formatter |> Fmt.fmt "Tok_fstring_width " |> (Rendition.pp Uns.pp) rendition
      | Tok_fstring_pmode pmode -> formatter |> Fmt.fmt "Tok_fstring_pmode " |> Fmt.pp_pmode pmode
      | Tok_fstring_precision_star -> formatter |> Fmt.fmt "Tok_fstring_precision_star"
      | Tok_fstring_precision rendition ->
        formatter |> Fmt.fmt "Tok_fstring_precision " |> (Rendition.pp Uns.pp) rendition
      | Tok_fstring_radix radix -> formatter |> Fmt.fmt "Tok_fstring_radix " |> Radix.pp radix
      | Tok_fstring_notation notation ->
        formatter |> Fmt.fmt "Tok_fstring_notation " |> Fmt.pp_notation notation
      | Tok_fstring_pretty -> formatter |> Fmt.fmt "Tok_fstring_pretty"
      | Tok_fstring_fmt rendition ->
        formatter |> Fmt.fmt "Tok_fstring_fmt " |> (Rendition.pp pp_fmt) rendition
      | Tok_fstring_sep rendition ->
        formatter |> Fmt.fmt "Tok_fstring_sep " |> (Rendition.pp String.pp) rendition
      | Tok_fstring_label label -> formatter |> Fmt.fmt "Tok_fstring_label " |> String.pp label
      | Tok_fstring_lparen_caret -> formatter |> Fmt.fmt "Tok_fstring_lparen_caret"
      | Tok_fstring_caret_rparen -> formatter |> Fmt.fmt "Tok_fstring_caret_rparen"
      | Tok_fstring_rditto -> formatter |> Fmt.fmt "Tok_fstring_rditto"
      | Tok_r32 rendition ->
        formatter |> Fmt.fmt "Tok_r32 "
        |> (Rendition.pp Real.(fmt ~alt:true ~radix:Radix.Hex ~precision:6L
            ~notation:Fmt.Normalized)) rendition
      | Tok_r64 rendition ->
        formatter |> Fmt.fmt "Tok_r64 "
        |> (Rendition.pp Real.(fmt ~alt:true ~radix:Radix.Hex ~precision:13L
            ~notation:Fmt.Normalized)) rendition
      | Tok_u8 rendition ->
        formatter |> Fmt.fmt "Tok_u8 " |> (Rendition.pp U8.pp) rendition
      | Tok_i8 rendition ->
        formatter |> Fmt.fmt "Tok_i8 " |> (Rendition.pp I8.pp) rendition
      | Tok_u16 rendition ->
        formatter |> Fmt.fmt "Tok_u16 " |> (Rendition.pp U16.pp) rendition
      | Tok_i16 rendition ->
        formatter |> Fmt.fmt "Tok_i16 " |> (Rendition.pp I16.pp) rendition
      | Tok_u32 rendition ->
        formatter |> Fmt.fmt "Tok_u32 " |> (Rendition.pp U32.pp) rendition
      | Tok_i32 rendition ->
        formatter |> Fmt.fmt "Tok_i32 " |> (Rendition.pp I32.pp) rendition
      | Tok_u64 rendition ->
        formatter |> Fmt.fmt "Tok_u64 " |> (Rendition.pp U64.pp) rendition
      | Tok_i64 rendition ->
        formatter |> Fmt.fmt "Tok_i64 " |> (Rendition.pp I64.pp) rendition
      | Tok_u128 rendition ->
        formatter |> Fmt.fmt "Tok_u128 " |> (Rendition.pp U128.pp) rendition
      | Tok_i128 rendition ->
        formatter |> Fmt.fmt "Tok_i128 " |> (Rendition.pp I128.pp) rendition
      | Tok_u256 rendition ->
        formatter |> Fmt.fmt "Tok_u256 " |> (Rendition.pp U256.pp) rendition
      | Tok_i256 rendition ->
        formatter |> Fmt.fmt "Tok_i256 " |> (Rendition.pp I256.pp) rendition
      | Tok_u512 rendition ->
        formatter |> Fmt.fmt "Tok_u512 " |> (Rendition.pp U512.pp) rendition
      | Tok_i512 rendition ->
        formatter |> Fmt.fmt "Tok_i512 " |> (Rendition.pp I512.pp) rendition
      | Tok_nat rendition ->
        formatter |> Fmt.fmt "Tok_nat " |> (Rendition.pp Nat.pp) rendition
      | Tok_zint rendition ->
        formatter |> Fmt.fmt "Tok_zint " |> (Rendition.pp Zint.pp) rendition
      | Tok_end_of_input -> formatter |> Fmt.fmt "Tok_end_of_input"
      | Tok_misaligned -> formatter |> Fmt.fmt "Tok_misaligned"
      | Tok_error mals ->
        formatter |> Fmt.fmt "Tok_error " |> (List.pp Rendition.Malformation.pp) mals
    )
    |> Fmt.fmt ">"

  let malformations = function
    (* Keywords. *)
    | Tok_and | Tok_also | Tok_as | Tok_conceal | Tok_effect | Tok_else | Tok_expose | Tok_external
    | Tok_false | Tok_fn | Tok_function | Tok_if | Tok_import | Tok_include | Tok_lazy | Tok_let
    | Tok_match | Tok_mutability | Tok_of | Tok_open | Tok_or | Tok_rec | Tok_then | Tok_true
    | Tok_type | Tok_when | Tok_with
    (* Operators. *)
    | Tok_tilde_op _ | Tok_qmark_op _ | Tok_star_star_op _ | Tok_star_op _ | Tok_slash_op _
    | Tok_pct_op _ | Tok_plus_op _ | Tok_minus_op _ | Tok_at_op _ | Tok_caret_op _ | Tok_dollar_op _
    | Tok_lt_op _ | Tok_eq_op _ | Tok_gt_op _ | Tok_bar_op _ | Tok_colon_op _ | Tok_dot_op _
    (* Punctuation. *)
    | Tok_tilde | Tok_qmark | Tok_minus | Tok_lt | Tok_lt_eq | Tok_eq | Tok_lt_gt | Tok_gt_eq
    | Tok_gt | Tok_comma | Tok_dot | Tok_dot_dot | Tok_semi | Tok_colon | Tok_colon_colon
    | Tok_colon_eq | Tok_lparen | Tok_rparen | Tok_lbrack | Tok_rbrack | Tok_lcurly | Tok_rcurly
    | Tok_bar | Tok_lcapture | Tok_rcapture | Tok_larray | Tok_rarray | Tok_bslash | Tok_tick
    | Tok_caret | Tok_amp | Tok_xmark | Tok_arrow | Tok_carrow
    (* Miscellaneous. *)
    | Tok_source_directive (Constant _)
    | Tok_line_delim
    | Tok_indent (Constant _)
    | Tok_dedent (Constant _)
    | Tok_whitespace|Tok_hash_comment
    | Tok_paren_comment (Constant _)
    | Tok_uscore
    | Tok_uident (Constant _)
    | Tok_cident _
    | Tok_codepoint (Constant _)
    | Tok_rstring (Constant _)
    | Tok_istring (Constant _)
    | Tok_fstring_lditto
    | Tok_fstring_interpolated (Constant _)
    | Tok_fstring_pct
    | Tok_fstring_pad (Constant _)
    | Tok_fstring_just _ | Tok_fstring_sign _ | Tok_fstring_alt | Tok_fstring_zpad
    | Tok_fstring_width_star
    | Tok_fstring_width (Constant _)
    | Tok_fstring_pmode _ | Tok_fstring_precision_star
    | Tok_fstring_precision (Constant _)
    | Tok_fstring_radix _ | Tok_fstring_notation _ | Tok_fstring_pretty
    | Tok_fstring_fmt (Constant _)
    | Tok_fstring_sep (Constant _)
    | Tok_fstring_label _ | Tok_fstring_lparen_caret | Tok_fstring_caret_rparen | Tok_fstring_rditto
    | Tok_r32 (Constant _)
    | Tok_r64 (Constant _)
    | Tok_u8 (Constant _)
    | Tok_i8 (Constant _)
    | Tok_u16 (Constant _)
    | Tok_i16 (Constant _)
    | Tok_u32 (Constant _)
    | Tok_i32 (Constant _)
    | Tok_u64 (Constant _)
    | Tok_i64 (Constant _)
    | Tok_u128 (Constant _)
    | Tok_i128 (Constant _)
    | Tok_u256 (Constant _)
    | Tok_i256 (Constant _)
    | Tok_u512 (Constant _)
    | Tok_i512 (Constant _)
    | Tok_nat (Constant _)
    | Tok_zint (Constant _)
    | Tok_end_of_input | Tok_misaligned
      -> []
    (* Malformations. *)
    | Tok_source_directive (Malformed mals)
    | Tok_indent (Malformed mals)
    | Tok_dedent (Malformed mals)
    | Tok_paren_comment (Malformed mals)
    | Tok_uident (Malformed mals)
    | Tok_codepoint (Malformed mals)
    | Tok_rstring (Malformed mals)
    | Tok_istring (Malformed mals)
    | Tok_fstring_interpolated (Malformed mals)
    | Tok_fstring_pad (Malformed mals)
    | Tok_fstring_width (Malformed mals)
    | Tok_fstring_precision (Malformed mals)
    | Tok_fstring_fmt (Malformed mals)
    | Tok_fstring_sep (Malformed mals)
    | Tok_r32 (Malformed mals)
    | Tok_r64 (Malformed mals)
    | Tok_u8 (Malformed mals)
    | Tok_i8 (Malformed mals)
    | Tok_u16 (Malformed mals)
    | Tok_i16 (Malformed mals)
    | Tok_u32 (Malformed mals)
    | Tok_i32 (Malformed mals)
    | Tok_u64 (Malformed mals)
    | Tok_i64 (Malformed mals)
    | Tok_u128 (Malformed mals)
    | Tok_i128 (Malformed mals)
    | Tok_u256 (Malformed mals)
    | Tok_i256 (Malformed mals)
    | Tok_u512 (Malformed mals)
    | Tok_i512 (Malformed mals)
    | Tok_nat (Malformed mals)
    | Tok_zint (Malformed mals)
    | Tok_error mals
      -> mals
end

module ConcreteToken = struct
  type t = {
    atok: AbstractToken.t;
    source: Source.Slice.t;
  }

  let init atok source =
    {atok; source}

  let ctok_at ~base ~past atok =
    let source = Source.Slice.of_cursors ~base ~past in
    init atok source

  let atok t =
    t.atok

  let source t =
    t.source

  let pp t formatter =
    formatter
    |> Fmt.fmt "{atok=" |> AbstractToken.pp t.atok
    |> Fmt.fmt "; source=" |> Source.Slice.pp t.source
    |> Fmt.fmt "}"
end

(* Upon entry into a node's handler function, the codepoint just acted upon is bracketed by
 * `pcursor` and `curusor`. NB: `eoi[01]` handlers are called when no more input exists, which means
 * that `cursor` is at the end of input. *)
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

(* Dentation level. *)
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
 * Tok_{line_delim,indent,dedent}, as syntactic analogues to Tok_{semi,lparen,rparen}. *)
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

(* fstring_state determines what starting state to feed to Dfa.next. States may be skipped, e.g. if
 * justification is not specified, but ordering through the spec/expr states is strict. Upon
 * completing specifier scanning the state transition to `Fstring_body`, which is capable of
 * initiating the scan of a subsequent specifier. *)
type fstring_state =
  | Fstring_spec_pct_seen of ConcreteToken.t list
  | Fstring_spec_pad_seen
  | Fstring_spec_just_seen
  | Fstring_spec_sign_seen
  | Fstring_spec_alt_seen
  | Fstring_spec_zpad_seen
  | Fstring_spec_width_star_seen
  | Fstring_expr_width
  | Fstring_spec_width_seen
  | Fstring_spec_pmode_seen
  | Fstring_spec_precision_star_seen
  | Fstring_expr_precision
  | Fstring_spec_precision_seen
  | Fstring_spec_radix_seen
  | Fstring_spec_notation_seen
  | Fstring_spec_pretty_seen
  | Fstring_spec_fmt_f_seen
  | Fstring_expr_fmt
  | Fstring_spec_fmt_seen
  | Fstring_spec_sep_seen
  | Fstring_expr_value of Source.Cursor.t option (* Cursor is start of captured value expression. *)
  | Fstring_value_seen of ConcreteToken.t
  | Fstring_body
  | Fstring_rditto_seen of ConcreteToken.t

let pp_fstring_state fstring_state formatter =
  match fstring_state with
  | Fstring_spec_pct_seen ctoks ->
    formatter |> Fmt.fmt "Fstring_spec_pct " |> (List.pp ConcreteToken.pp) ctoks
  | Fstring_spec_pad_seen -> formatter |> Fmt.fmt "Fstring_spec_pad_seen"
  | Fstring_spec_just_seen -> formatter |> Fmt.fmt "Fstring_spec_just_seen"
  | Fstring_spec_sign_seen -> formatter |> Fmt.fmt "Fstring_spec_sign_seen"
  | Fstring_spec_alt_seen -> formatter |> Fmt.fmt "Fstring_spec_alt_seen"
  | Fstring_spec_zpad_seen -> formatter |> Fmt.fmt "Fstring_spec_zpad_seen"
  | Fstring_spec_width_star_seen -> formatter |> Fmt.fmt "Fstring_spec_width_star_seen"
  | Fstring_expr_width -> formatter |> Fmt.fmt "Fstring_expr_width"
  | Fstring_spec_width_seen -> formatter |> Fmt.fmt "Fstring_spec_width_seen"
  | Fstring_spec_pmode_seen -> formatter |> Fmt.fmt "Fstring_spec_pmode_seen"
  | Fstring_spec_precision_star_seen -> formatter |> Fmt.fmt "Fstring_spec_precision_star_seen"
  | Fstring_expr_precision -> formatter |> Fmt.fmt "Fstring_expr_precision"
  | Fstring_spec_precision_seen -> formatter |> Fmt.fmt "Fstring_spec_precision_seen"
  | Fstring_spec_radix_seen -> formatter |> Fmt.fmt "Fstring_spec_radix_seen"
  | Fstring_spec_notation_seen -> formatter |> Fmt.fmt "Fstring_spec_notation_seen"
  | Fstring_spec_pretty_seen -> formatter |> Fmt.fmt "Fstring_spec_pretty_seen"
  | Fstring_spec_fmt_f_seen -> formatter |> Fmt.fmt "Fstring_spec_fmt_f_seen"
  | Fstring_expr_fmt -> formatter |> Fmt.fmt "Fstring_expr_fmt"
  | Fstring_spec_fmt_seen -> formatter |> Fmt.fmt "Fstring_spec_fmt_seen"
  | Fstring_spec_sep_seen -> formatter |> Fmt.fmt "Fstring_spec_sep_seen"
  | Fstring_expr_value cursor_opt ->
    formatter |> Fmt.fmt "Fstring_exp_value " |> (Option.pp Source.Cursor.pp) cursor_opt
  | Fstring_value_seen ctok -> formatter |> Fmt.fmt "Fstring_spec_pct " |> ConcreteToken.pp ctok
  | Fstring_body -> formatter |> Fmt.fmt "Fstring_body"
  | Fstring_rditto_seen ctok -> formatter |> Fmt.fmt "Fstring_spec_pct " |> ConcreteToken.pp ctok

type t = {
  tok_base: Source.Cursor.t;
  level: Level.t;
  block_state: block_state;
  line_state: line_state;
  fstring_states: fstring_state list;
}

let init text =
  {
    tok_base=Source.Cursor.hd (Source.init text);
    level=Level.init 0L;
    block_state=Block_primal;
    line_state=Line_begin;
    fstring_states=[];
  }

let pp t formatter =
  formatter
  |> Fmt.fmt "{tok_base=" |> Text.Pos.pp (Source.Cursor.pos t.tok_base)
  |> Fmt.fmt "; level=" |> Level.pp t.level
  |> Fmt.fmt "; block_state=" |> pp_block_state t.block_state
  |> Fmt.fmt "; line_state=" |> pp_line_state t.line_state
  |> Fmt.fmt "; fstring_states=" |> (List.pp pp_fstring_state) t.fstring_states
  |> Fmt.fmt "}"

let view_of_t t =
  View.init ~ppcursor:t.tok_base ~pcursor:t.tok_base ~cursor:t.tok_base

let text t =
  Source.(text (Cursor.container t.tok_base))

let cursor {tok_base; _} =
  tok_base

let str_of_cursor cursor t =
  Source.Slice.to_string (Source.Slice.of_cursors ~base:t.tok_base ~past:cursor)

let in_fstring t =
  match t.fstring_states with
  | []
  | Fstring_expr_width :: _
  | Fstring_expr_precision :: _
  | Fstring_expr_fmt :: _
  | (Fstring_expr_value _) :: _  -> false
  | _ -> true

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

let invalid_codepoint base past =
  malformation ~base ~past "Invalid codepoint"

let invalid_unicode_escape base past =
  malformation ~base ~past "Invalid \\u{...}"

let invalid_tag base past =
  malformation ~base ~past "Invalid codepoint in raw string tag"

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

let invalid_numerical base past =
  malformation ~base ~past "Invalid numerical constant"

let unsupported_bitwidth base past =
  malformation ~base ~past "Unsupported bitwidth"

let out_of_range_int radix limit base past =
  let description =
    String.Fmt.empty
    |> Fmt.fmt "Numerical constant exceeds "
    |> Nat.fmt ~alt:true ~radix limit
    |> Fmt.to_string
  in
  malformation ~base ~past description

let out_of_range_real base past =
  malformation ~base ~past "Numerical constant cannot be precisely represented"

(**************************************************************************************************)

(* `State.t` contains one constructor per DFA state. The `State.<Param>` submodules encapsulate per
 * node state. *)
module State = struct
  module Real_precise_dot = struct
    type t =
      | R of {
        m: Realer.t;
        point_shift: sint;
      }
      | Malformations of AbstractToken.Rendition.Malformation.t list

    let pp t formatter =
      match t with
      | R {m; point_shift} ->
        formatter
        |> Fmt.fmt "R {m=" |> Realer.pp m
        |> Fmt.fmt "; point_shift=" |> Sint.pp point_shift
        |> Fmt.fmt "}"
      | Malformations mals ->
        formatter
        |> Fmt.fmt "Malformations " |> (List.pp AbstractToken.Rendition.Malformation.pp) mals

    let init ~m =
      R {m; point_shift=Sint.zero}

    let accum_digit_impl bits_per_digit digit = function
      | R {m; point_shift} -> begin
          let sig_bits = (Nat.bit_length digit) - (Nat.bit_clz digit) in
          let nonfrac_shift = Sint.(bits_per_digit - (pred (Uns.bits_to_sint sig_bits))) in
          let exponent = Zint.(of_sint point_shift - (of_sint nonfrac_shift)) in
          let realer_digit = Realer.create ~sign:Pos ~exponent ~mantissa:digit in
          let m' = Realer.(m + realer_digit) in
          let point_shift' = Sint.(point_shift - bits_per_digit) in
          R {m=m'; point_shift=point_shift'}
        end
      | (Malformations _) as t -> t

    let accum_mal mal = function
      | R _ -> Malformations [mal]
      | Malformations mals -> Malformations (mal :: mals)
  end

  module Real_bin_dot = struct
    include Real_precise_dot

    let accum_digit digit t =
      accum_digit_impl (Sint.kv 1L) digit t
  end

  module Real_oct_dot = struct
    include Real_precise_dot

    let accum_digit digit t =
      accum_digit_impl (Sint.kv 3L) digit t
  end

  module Real_hex_dot = struct
    include Real_precise_dot

    let accum_digit digit t =
      accum_digit_impl (Sint.kv 4L) digit t
  end

  module Real_dec_dot = struct
    type t =
      | R of {
        m: real;
        ds: real; (* (ds * digit) scales digit to its fractional value. *)
      }
      | Malformations of AbstractToken.Rendition.Malformation.t list

    let pp t formatter =
      match t with
      | R {m; ds} ->
        formatter
        |> Fmt.fmt "R {m=" |> Basis.Real.pp m
        |> Fmt.fmt "; ds=" |> Basis.Real.pp ds
        |> Fmt.fmt "}"
      | Malformations mals ->
        formatter
        |> Fmt.fmt "Malformations " |> (List.pp AbstractToken.Rendition.Malformation.pp) mals

    let init ~m =
      R {m; ds=1. /. 10.}

    let mals ~mals =
      Malformations mals

    let accum_digit digit = function
      | R {m; ds} -> begin
          let m' = m +. ds *. digit in
          let ds' = ds /. 10. in
          R {m=m'; ds=ds'}
        end
      | (Malformations _) as t -> t

    let accum_mal mal = function
      | R _ -> Malformations [mal]
      | Malformations mals -> Malformations (mal :: mals)
  end

  module Real_p = struct
    type t =
      | R of {
        m: Realer.t;
      }
      | Malformations of AbstractToken.Rendition.Malformation.t list

    let pp t formatter =
      match t with
      | R {m} ->
        formatter
        |> Fmt.fmt "R {m=" |> Realer.pp m
        |> Fmt.fmt "}"
      | Malformations mals ->
        formatter
        |> Fmt.fmt "Malformations " |> (List.pp AbstractToken.Rendition.Malformation.pp) mals

    let init ~m =
      R {m}

    let mals ~mals =
      Malformations mals

    let accum_mal mal = function
      | R _ -> Malformations [mal]
      | Malformations mals -> Malformations (mal :: mals)
  end

  module Real_e = struct
    type t =
      | R of {
        m: real;
      }
      | Malformations of AbstractToken.Rendition.Malformation.t list

    let pp t formatter =
      match t with
      | R {m} ->
        formatter
        |> Fmt.fmt "R {m=" |> Basis.Real.pp m
        |> Fmt.fmt "}"
      | Malformations mals ->
        formatter
        |> Fmt.fmt "Malformations " |> (List.pp AbstractToken.Rendition.Malformation.pp) mals

    let init ~m =
      R {m}

    let mals ~mals =
      Malformations mals

    let accum_mal mal = function
      | R _ -> Malformations [mal]
      | Malformations mals -> Malformations (mal :: mals)
  end

  module Real_p_sign = struct
    type t =
      | R of {
        m: Realer.t;
        exp_sign: Sign.t;
      }
      | Malformations of AbstractToken.Rendition.Malformation.t list

    let pp t formatter =
      match t with
      | R {m; exp_sign} ->
        formatter
        |> Fmt.fmt "R {m=" |> Realer.pp m
        |> Fmt.fmt "; exp_sign=" |> Sign.pp exp_sign
        |> Fmt.fmt "}"
      | Malformations mals ->
        formatter
        |> Fmt.fmt "Malformations " |> (List.pp AbstractToken.Rendition.Malformation.pp) mals

    let init ~m ~exp_sign =
      R {m; exp_sign}

    let mals ~mals =
      Malformations mals

    let accum_mal mal = function
      | R _ -> Malformations [mal]
      | Malformations mals -> Malformations (mal :: mals)
  end

  module Real_e_sign = struct
    type t =
      | R of {
        m: real;
        exp_sign: Sign.t;
      }
      | Malformations of AbstractToken.Rendition.Malformation.t list

    let pp t formatter =
      match t with
      | R {m; exp_sign} ->
        formatter
        |> Fmt.fmt "R {m=" |> Basis.Real.pp m
        |> Fmt.fmt "; exp_sign=" |> Sign.pp exp_sign
        |> Fmt.fmt "}"
      | Malformations mals ->
        formatter
        |> Fmt.fmt "Malformations " |> (List.pp AbstractToken.Rendition.Malformation.pp) mals

    let init ~m ~exp_sign =
      R {m; exp_sign}

    let mals ~mals =
      Malformations mals

    let accum_mal mal = function
      | R _ -> Malformations [mal]
      | Malformations mals -> Malformations (mal :: mals)
  end

  module Real_p_exp = struct
    type t =
      | R of {
        m: Realer.t;
        exp_sign: Sign.t;
        exp: Nat.t;
      }
      | Malformations of AbstractToken.Rendition.Malformation.t list

    let pp t formatter =
      match t with
      | R {m; exp_sign; exp} ->
        formatter
        |> Fmt.fmt "R {m=" |> Realer.pp m
        |> Fmt.fmt "; exp_sign=" |> Sign.pp exp_sign
        |> Fmt.fmt "; exp=" |> Nat.pp exp
        |> Fmt.fmt "}"
      | Malformations mals ->
        formatter
        |> Fmt.fmt "Malformations " |> (List.pp AbstractToken.Rendition.Malformation.pp) mals

    let init ~m ~exp_sign ~exp =
      R {m; exp_sign; exp}

    let mals ~mals =
      Malformations mals

    let accum_digit digit = function
      | R {m; exp_sign; exp} -> R {m; exp_sign; exp=Radix.accum_nat digit exp Dec}
      | (Malformations _) as t -> t

    let accum_mal mal = function
      | R _ -> Malformations [mal]
      | Malformations mals -> Malformations (mal :: mals)

    let to_r = function
      | R {m; exp_sign; exp} -> begin
          let exponent = match exp_sign with
            | Neg -> Zint.(neg (Nat.like_to_zint_hlt exp))
            | Zero -> not_reached ()
            | Pos -> Nat.(like_to_zint_hlt exp)
          in
          let e = Realer.create ~sign:Realer.Pos ~exponent ~mantissa:Nat.one in
          Ok Realer.(m * e)
        end
      | Malformations mals -> Error mals
  end

  module Real_e_exp = struct
    type t =
      | R of {
        m: real;
        exp_sign: Sign.t;
        exp: Nat.t;
      }
      | Malformations of AbstractToken.Rendition.Malformation.t list

    let pp t formatter =
      match t with
      | R {m; exp_sign; exp} ->
        formatter
        |> Fmt.fmt "R {m=" |> Basis.Real.pp m
        |> Fmt.fmt "; exp_sign=" |> Sign.pp exp_sign
        |> Fmt.fmt "; exp=" |> Nat.pp exp
        |> Fmt.fmt "}"
      | Malformations mals ->
        formatter
        |> Fmt.fmt "Malformations " |> (List.pp AbstractToken.Rendition.Malformation.pp) mals

    let init ~m ~exp_sign ~exp =
      R {m; exp_sign; exp}

    let mals ~mals =
      Malformations mals

    let accum_digit digit = function
      | R {m; exp_sign; exp} -> R {m; exp_sign; exp=Radix.accum_nat digit exp Dec}
      | (Malformations _) as t -> t

    let accum_mal mal = function
      | R _ -> Malformations [mal]
      | Malformations mals -> Malformations (mal :: mals)

    let to_r = function
      | R {m; exp_sign; exp} -> begin
          let exp_r = Nat.to_real exp in
          let e = match exp_sign with
            | Neg -> Basis.Real.neg exp_r
            | Zero -> not_reached ()
            | Pos -> exp_r
          in
          Ok Basis.Real.(m *. (10. ** e))
        end
      | Malformations mals -> Error mals
  end

  module Real_precise_r = struct
    type t =
      | R of {
        r: Realer.t;
      }
      | Malformations of AbstractToken.Rendition.Malformation.t list

    let pp t formatter =
      match t with
      | R {r} ->
        formatter
        |> Fmt.fmt "R {r=" |> Realer.pp r
        |> Fmt.fmt "}"
      | Malformations mals ->
        formatter
        |> Fmt.fmt "Malformations " |> (List.pp AbstractToken.Rendition.Malformation.pp) mals

    let init ~r =
      R {r}

    let mals ~mals =
      Malformations mals

    let accum_mal mal = function
      | R _ -> Malformations [mal]
      | Malformations mals -> Malformations (mal :: mals)
  end

  module Real_approx_r = struct
    type t =
      | R of {
        r: real;
      }
      | Malformations of AbstractToken.Rendition.Malformation.t list

    let pp t formatter =
      match t with
      | R {r} ->
        formatter
        |> Fmt.fmt "R {r=" |> Basis.Real.pp r
        |> Fmt.fmt "}"
      | Malformations mals ->
        formatter
        |> Fmt.fmt "Malformations " |> (List.pp AbstractToken.Rendition.Malformation.pp) mals

    let init ~r =
      R {r}

    let mals ~mals =
      Malformations mals

    let accum_mal mal = function
      | R _ -> Malformations [mal]
      | Malformations mals -> Malformations (mal :: mals)
  end

  module Real_precise_r_bitwidth = struct
    type t =
      | R of {
        r: Realer.t;
        bitwidth: Nat.t;
      }
      | Malformations of AbstractToken.Rendition.Malformation.t list

    let pp t formatter =
      match t with
      | R {r; bitwidth} ->
        formatter
        |> Fmt.fmt "R {r=" |> Realer.pp r
        |> Fmt.fmt "; bitwidth=" |> Nat.pp bitwidth
        |> Fmt.fmt "}"
      | Malformations mals ->
        formatter
        |> Fmt.fmt "Malformations " |> (List.pp AbstractToken.Rendition.Malformation.pp) mals

    let init ~r ~bitwidth =
      R {r; bitwidth}

    let mals ~mals =
      Malformations mals

    let accum_digit digit = function
      | R {r; bitwidth} -> R {r; bitwidth=Radix.accum_nat digit bitwidth Dec}
      | (Malformations _) as t -> t

    let accum_mal mal = function
      | R _ -> Malformations [mal]
      | Malformations mals -> Malformations (mal :: mals)
  end

  module Real_approx_r_bitwidth = struct
    type t =
      | R of {
        r: real;
        bitwidth: Nat.t;
      }
      | Malformations of AbstractToken.Rendition.Malformation.t list

    let pp t formatter =
      match t with
      | R {r; bitwidth} ->
        formatter
        |> Fmt.fmt "R {r=" |> Basis.Real.pp r
        |> Fmt.fmt "; bitwidth=" |> Nat.pp bitwidth
        |> Fmt.fmt "}"
      | Malformations mals ->
        formatter
        |> Fmt.fmt "Malformations " |> (List.pp AbstractToken.Rendition.Malformation.pp) mals

    let init ~r ~bitwidth =
      R {r; bitwidth}

    let mals ~mals =
      Malformations mals

    let accum_digit digit = function
      | R {r; bitwidth} -> R {r; bitwidth=Radix.accum_nat digit bitwidth Dec}
      | (Malformations _) as t -> t

    let accum_mal mal = function
      | R _ -> Malformations [mal]
      | Malformations mals -> Malformations (mal :: mals)
  end

  module Integer_radix = struct
    type t = {
      n: Nat.t;
    }

    let pp {n} formatter =
      formatter |> Fmt.fmt "{n=" |> Nat.pp n |> Fmt.fmt "}"

    let init ~n =
      {n}
  end

  module Integer_bin = struct
    include Integer_radix

    let accum_digit digit {n} =
      {n=Nat.(n * k_2 + digit)}
  end

  module Integer_oct = struct
    include Integer_radix

    let accum_digit digit {n} =
      {n=Nat.(n * k_8 + digit)}
  end

  module Integer_dec = struct
    include Integer_radix

    let accum_digit digit {n} =
      {n=Nat.(n * k_a + digit)}
  end

  module Integer_hex = struct
    include Integer_radix

    let accum_digit digit {n} =
      {n=Nat.(n * k_g + digit)}
  end

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

    let accum_bitwidth digit ({bitwidth; _} as t) =
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

    let accum_mals mal ({mals; _} as t) =
      {t with mals=mal :: mals}

    let accum_path cp ({path; _} as t) =
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

    let accum_mals mal {mals; path; _} =
      Src_directive_path.{mals=mal :: mals; path}

    let accum_path cp {mals; path; _} =
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

    let accum_mals mal {mals; path; _} =
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
      |> Fmt.fmt "; hex=" |> Nat.fmt ~alt:true ~radix:Radix.Hex ~pretty:true hex
      |> Fmt.fmt "}"

    let init ~mals ~path ~bslash_cursor ~hex =
      {mals; path; bslash_cursor; hex}

    let accum_mals mal {mals; path; _} =
      Src_directive_path.{mals=mal :: mals; path}

    let accum_path cp {mals; path; _} =
      match path with
      | None -> Src_directive_path.{mals; path=Some [cp]}
      | Some cps -> Src_directive_path.{mals; path=Some (cp :: cps)}

    let accum_hex digit ({hex; _} as t) =
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

    let accum_line digit ({line; _} as t) =
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

    let accum_mals mal ({mals; _} as t) =
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

    let accum_mals mal ({mals; _} as t) =
      {t with mals=mal :: mals}

    let accum_indent digit ({indent; _} as t) =
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

    let accum_mals mal ({mals; _} as t) =
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

    let accum_mals mal ({mals; _} as t) =
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

    let accum_mals mal ({mals; _} as t) =
      {t with mals=mal :: mals}

    let accum_omit digit ({omit; _} as t) =
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
      |> Fmt.fmt "; hex=" |> Nat.fmt ~alt:true ~radix:Radix.Hex ~pretty:true hex
      |> Fmt.fmt "}"

    let init ~bslash_cursor ~hex =
      {bslash_cursor; hex}

    let accum_hex digit ({hex; _} as t) =
      {t with hex=Nat.(hex * k_g + digit)}
  end

  module Codepoint_interpolated_cp = struct
    type t = {
      cp: codepoint;
    }

    let pp {cp} formatter =
      formatter |> Fmt.fmt "{cp=" |> Codepoint.pp cp |> Fmt.fmt "}"

    let init ~cp =
      {cp}
  end

  module Codepoint_raw_cp = Codepoint_interpolated_cp

  module Codepoint_mal = struct
    type t = {
      mal: AbstractToken.Rendition.Malformation.t;
    }

    let pp {mal} formatter =
      formatter
      |> Fmt.fmt "{mal=" |> AbstractToken.Rendition.Malformation.pp mal |> Fmt.fmt "}"

    let init ~mal =
      {mal}
  end

  module Rstring_ltag = struct
    type t = {
      mals: AbstractToken.Rendition.Malformation.t list;
      ltag_base: Source.Cursor.t;
    }

    let pp {mals; ltag_base} formatter =
      formatter
      |> Fmt.fmt "{mals=" |> (List.pp AbstractToken.Rendition.Malformation.pp) mals
      |> Fmt.fmt "; ltag_base=" |> Source.Cursor.pp ltag_base
      |> Fmt.fmt "}"

    let init ~ltag_base =
      {mals=[]; ltag_base}

    let accum_mal ~mal ({mals; _} as t) =
      {t with mals=mal :: mals}
  end

  module Rstring_body = struct
    type t = {
      mals: AbstractToken.Rendition.Malformation.t list;
      ltag: Source.Slice.t;
      body_base: Source.Cursor.t;
    }

    let pp {mals; ltag; body_base} formatter =
      formatter
      |> Fmt.fmt "{mals=" |> (List.pp AbstractToken.Rendition.Malformation.pp) mals
      |> Fmt.fmt "; ltag=" |> Source.Slice.pp ltag
      |> Fmt.fmt "; body_base=" |> Source.Cursor.pp body_base
      |> Fmt.fmt "}"

    let init ~mals ~ltag ~body_base =
      {mals; ltag; body_base}

    let accum_mal ~mal ({mals; _} as t) =
      {t with mals=mal :: mals}
  end

  module Rstring_rtag = struct
    type t = {
      mals: AbstractToken.Rendition.Malformation.t list;
      ltag: Source.Slice.t;
      body: Source.Slice.t;
      ltag_cursor: Source.Cursor.t;
    }

    let pp {mals; ltag; ltag_cursor; body} formatter =
      formatter
      |> Fmt.fmt "{mals=" |> (List.pp AbstractToken.Rendition.Malformation.pp) mals
      |> Fmt.fmt "; ltag=" |> Source.Slice.pp ltag
      |> Fmt.fmt "; body=" |> Source.Slice.pp body
      |> Fmt.fmt "; ltag_cursor=" |> Source.Cursor.pp ltag_cursor
      |> Fmt.fmt "}"

    let init ~mals ~ltag ~body =
      {mals; ltag; body; ltag_cursor=Source.Slice.base ltag}

    let next t =
      {t with ltag_cursor=Source.Cursor.succ t.ltag_cursor}

    let accum_mal ~mal ({mals; _} as t) =
      {t with mals=mal :: mals}
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

    let empty = Codepoints []

    let accum_cp cp = function
      | Codepoints cps -> Codepoints (cp :: cps)
      | (Malformations _) as mals -> mals

    let accum_mal mal = function
      | Codepoints _ -> Malformations [mal]
      | Malformations mals -> Malformations (mal :: mals)

    let to_atok_istring = function
      | Codepoints cps -> AbstractToken.Tok_istring (Constant (String.of_list_rev cps))
      | Malformations mals -> AbstractToken.Tok_istring (AbstractToken.Rendition.of_mals mals)

    let to_atok_fstring_interpolated = function
      | Codepoints cps -> AbstractToken.Tok_fstring_interpolated (Constant (String.of_list_rev cps))
      | Malformations mals ->
        AbstractToken.Tok_fstring_interpolated (AbstractToken.Rendition.of_mals mals)
  end

  module Istring_body = struct
    type t = {
      accum: CodepointAccum.t;
    }

    let pp t formatter =
      formatter |> Fmt.fmt "{accum=" |> CodepointAccum.pp t.accum |> Fmt.fmt "}"

    let empty = {accum=CodepointAccum.empty}

    let init ~accum =
      {accum}

    let accum_cp cp t =
      {accum=CodepointAccum.accum_cp cp t.accum}

    let accum_mal mal t =
      {accum=CodepointAccum.accum_mal mal t.accum}
  end

  module Istring_bslash = struct
    type t = {
      accum: CodepointAccum.t;
      bslash_cursor: Source.Cursor.t;
    }

    let pp t formatter =
      formatter
      |> Fmt.fmt "{accum=" |> CodepointAccum.pp t.accum
      |> Fmt.fmt "; bslash_cursor=" |> Text.Pos.pp (Source.Cursor.pos t.bslash_cursor)
      |> Fmt.fmt "}"

    let init ~accum ~bslash_cursor =
      {accum; bslash_cursor}

    let accum_cp cp t =
      {t with accum=CodepointAccum.accum_cp cp t.accum}

    let accum_mal mal t =
      {t with accum=CodepointAccum.accum_mal mal t.accum}
  end

  module Istring_bslash_u = Istring_bslash

  module Istring_bslash_u_lcurly = struct
    type t = {
      accum: CodepointAccum.t;
      bslash_cursor: Source.Cursor.t;
      u: Nat.t;
    }

    let pp t formatter =
      formatter
      |> Fmt.fmt "{accum=" |> CodepointAccum.pp t.accum
      |> Fmt.fmt "; bslash_cursor=" |> Text.Pos.pp (Source.Cursor.pos t.bslash_cursor)
      |> Fmt.fmt "; u=" |> Nat.fmt ~alt:true ~radix:Radix.Hex ~pretty:true t.u
      |> Fmt.fmt "}"

    let accum_cp cp t =
      {t with accum=CodepointAccum.accum_cp cp t.accum}

    let accum_mal mal t =
      {t with accum=CodepointAccum.accum_mal mal t.accum}

    let accum_hex cp t =
      let u' = Radix.(accum_nat (nat_of_cp cp) t.u Hex) in
      {t with u=u'}
  end

  module Fstring_width = struct
    type t = {
      n: Nat.t
    }

    let pp {n} formatter =
      formatter |> Fmt.fmt "{n=" |> Nat.pp n |> Fmt.fmt "}"

    let init ~n =
      {n}

    let accum_digit digit {n} =
      {n=Nat.(n * k_a + digit)}
  end

  module Fstring_precision = Fstring_width

  module Fstring_fmt_u_bitwidth = struct
    type t = {
      fmt_cursor: Source.Cursor.t;
      invalid: bool;
      bitwidth: Nat.t;
    }

    let pp {fmt_cursor; invalid; bitwidth} formatter =
      formatter
      |> Fmt.fmt "{fmt_cursor=" |> Source.Cursor.pp fmt_cursor
      |> Fmt.fmt "; invalid=" |> Bool.pp invalid
      |> Fmt.fmt "; bitwidth=" |> Nat.pp bitwidth
      |> Fmt.fmt "}"

    let init ~fmt_cursor =
      {fmt_cursor; invalid=false; bitwidth=Nat.k_0}

    let accum_invalid t =
      {t with invalid=true}

    let accum_bitwidth digit ({bitwidth; _} as t) =
      {t with bitwidth=Nat.(bitwidth * k_a + digit)}
  end

  module Fstring_fmt_i_bitwidth = Fstring_fmt_u_bitwidth

  module Fstring_fmt_r_bitwidth = Fstring_fmt_u_bitwidth

  type t =
    | State_start
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
    | State_real_bin_dot of Real_bin_dot.t
    | State_real_oct_dot of Real_oct_dot.t
    | State_real_hex_dot of Real_hex_dot.t
    | State_real_dec_dot of Real_dec_dot.t
    | State_real_p of Real_p.t
    | State_real_e of Real_e.t
    | State_real_p_sign of Real_p_sign.t
    | State_real_e_sign of Real_e_sign.t
    | State_real_p_exp of Real_p_exp.t
    | State_real_e_exp of Real_e_exp.t
    | State_real_precise_r of Real_precise_r.t
    | State_real_approx_r of Real_approx_r.t
    | State_real_precise_r_bitwidth of Real_precise_r_bitwidth.t
    | State_real_approx_r_bitwidth of Real_approx_r_bitwidth.t
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
    | State_codepoint_interpolated_cp of Codepoint_interpolated_cp.t
    | State_codepoint_raw_cp of Codepoint_raw_cp.t
    | State_codepoint_mal of Codepoint_mal.t
    | State_rstring_ltag of Rstring_ltag.t
    | State_rstring_body of Rstring_body.t
    | State_rstring_rtag of Rstring_rtag.t
    | State_istring_body of Istring_body.t
    | State_istring_bslash of Istring_bslash.t
    | State_istring_bslash_u of Istring_bslash_u.t
    | State_istring_bslash_u_lcurly of Istring_bslash_u_lcurly.t
    | State_fstring_pct_seen_start
    | State_fstring_pad_seen_start
    | State_fstring_just_seen_start
    | State_fstring_sign_seen_start
    | State_fstring_alt_seen_start
    | State_fstring_zpad_seen_start
    | State_fstring_width_star_seen_start
    | State_fstring_width of Fstring_width.t
    | State_fstring_width_seen_start
    | State_fstring_dot
    | State_fstring_pmode_seen_start
    | State_fstring_precision_star_seen_start
    | State_fstring_precision of Fstring_precision.t
    | State_fstring_precision_seen_start
    | State_fstring_b
    | State_fstring_radix_seen_start
    | State_fstring_notation_seen_start
    | State_fstring_pretty_seen_start
    | State_fstring_c
    | State_fstring_fmt_u
    | State_fstring_fmt_u_bitwidth of Fstring_fmt_u_bitwidth.t
    | State_fstring_fmt_i
    | State_fstring_fmt_i_bitwidth of Fstring_fmt_i_bitwidth.t
    | State_fstring_fmt_r
    | State_fstring_fmt_r_bitwidth of Fstring_fmt_r_bitwidth.t
    | State_fstring_fmt_f_seen_start
    | State_fstring_fmt_seen_start
    | State_fstring_sep_space
    | State_fstring_sep_op
    | State_fstring_sep_op_space
    | State_fstring_sep_seen_start
    | State_fstring_lparen

  let pp t formatter =
    match t with
    | State_start -> formatter |> Fmt.fmt "State_start"
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
    | State_real_bin_dot v -> formatter |> Fmt.fmt "State_real_bin_dot " |> Real_bin_dot.pp v
    | State_real_oct_dot v -> formatter |> Fmt.fmt "State_real_oct_dot " |> Real_oct_dot.pp v
    | State_real_hex_dot v -> formatter |> Fmt.fmt "State_real_hex_dot " |> Real_hex_dot.pp v
    | State_real_dec_dot v -> formatter |> Fmt.fmt "State_real_dec_dot " |> Real_dec_dot.pp v
    | State_real_p v -> formatter |> Fmt.fmt "State_real_p " |> Real_p.pp v
    | State_real_e v -> formatter |> Fmt.fmt "State_real_e " |> Real_e.pp v
    | State_real_p_sign v -> formatter |> Fmt.fmt "State_real_p_sign " |> Real_p_sign.pp v
    | State_real_e_sign v -> formatter |> Fmt.fmt "State_real_e_sign " |> Real_e_sign.pp v
    | State_real_p_exp v -> formatter |> Fmt.fmt "State_real_p_exp " |> Real_p_exp.pp v
    | State_real_e_exp v -> formatter |> Fmt.fmt "State_real_e_exp " |> Real_e_exp.pp v
    | State_real_precise_r v -> formatter |> Fmt.fmt "State_real_precise_r " |> Real_precise_r.pp v
    | State_real_approx_r v -> formatter |> Fmt.fmt "State_real_approx_r " |> Real_approx_r.pp v
    | State_real_precise_r_bitwidth v ->
      formatter |> Fmt.fmt "State_real_precise_r_bitwidth " |> Real_precise_r_bitwidth.pp v
    | State_real_approx_r_bitwidth v ->
      formatter |> Fmt.fmt "State_real_approx_r_bitwidth " |> Real_approx_r_bitwidth.pp v
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
    | State_codepoint_interpolated_cp v ->
      formatter |> Fmt.fmt "State_codepoint_interpolated_cp " |> Codepoint_interpolated_cp.pp v
    | State_codepoint_raw_cp v ->
      formatter |> Fmt.fmt "State_codepoint_raw_cp " |> Codepoint_raw_cp.pp v
    | State_codepoint_mal v -> formatter |> Fmt.fmt "State_codepoint_mal " |> Codepoint_mal.pp v
    | State_rstring_ltag v -> formatter |> Fmt.fmt "State_rstring_ltag " |> Rstring_ltag.pp v
    | State_rstring_body v -> formatter |> Fmt.fmt "State_rstring_body " |> Rstring_body.pp v
    | State_rstring_rtag v -> formatter |> Fmt.fmt "State_rstring_rtag " |> Rstring_rtag.pp v
    | State_istring_body v ->
      formatter |> Fmt.fmt "State_istring_body " |> Istring_body.pp v
    | State_istring_bslash v ->
      formatter |> Fmt.fmt "State_istring_bslash " |> Istring_bslash.pp v
    | State_istring_bslash_u v ->
      formatter |> Fmt.fmt "State_istring_bslash_u " |> Istring_bslash_u.pp v
    | State_istring_bslash_u_lcurly v ->
      formatter |> Fmt.fmt "State_istring_bslash_lcurly " |> Istring_bslash_u_lcurly.pp v
    | State_fstring_pct_seen_start -> formatter |> Fmt.fmt "State_fstring_pct_seen_start"
    | State_fstring_pad_seen_start -> formatter |> Fmt.fmt "State_fstring_pad_seen_start"
    | State_fstring_just_seen_start -> formatter |> Fmt.fmt "State_fstring_just_seen_start"
    | State_fstring_sign_seen_start -> formatter |> Fmt.fmt "State_fstring_sign_seen_start"
    | State_fstring_alt_seen_start -> formatter |> Fmt.fmt "State_fstring_alt_seen_start"
    | State_fstring_zpad_seen_start -> formatter |> Fmt.fmt "State_fstring_zpad_seen_start"
    | State_fstring_width_star_seen_start ->
      formatter |> Fmt.fmt "State_fstring_width_star_seen_start"
    | State_fstring_width v -> formatter |> Fmt.fmt "State_fstring_width " |> Fstring_width.pp v
    | State_fstring_width_seen_start -> formatter |> Fmt.fmt "State_fstring_width_seen_start"
    | State_fstring_dot -> formatter |> Fmt.fmt "State_fstring_dot"
    | State_fstring_pmode_seen_start -> formatter |> Fmt.fmt "State_fstring_pmode_seen_start"
    | State_fstring_precision_star_seen_start ->
      formatter |> Fmt.fmt "State_fstring_precision_star_seen_start"
    | State_fstring_precision v ->
      formatter |> Fmt.fmt "State_fstring_precision " |> Fstring_precision.pp v
    | State_fstring_precision_seen_start ->
      formatter |> Fmt.fmt "State_fstring_precision_seen_start"
    | State_fstring_b -> formatter |> Fmt.fmt "State_fstring_b"
    | State_fstring_radix_seen_start -> formatter |> Fmt.fmt "State_fstring_radix_seen_start"
    | State_fstring_notation_seen_start -> formatter |> Fmt.fmt "State_fstring_notation_seen_start"
    | State_fstring_pretty_seen_start -> formatter |> Fmt.fmt "State_fstring_pretty_seen_start"
    | State_fstring_c -> formatter |> Fmt.fmt "State_fstring_c"
    | State_fstring_fmt_u -> formatter |> Fmt.fmt "State_fstring_fmt_u"
    | State_fstring_fmt_u_bitwidth v ->
      formatter |> Fmt.fmt "State_fstring_fmt_u_bitwidth " |> Fstring_fmt_u_bitwidth.pp v
    | State_fstring_fmt_i -> formatter |> Fmt.fmt "State_fstring_fmt_i"
    | State_fstring_fmt_i_bitwidth v ->
      formatter |> Fmt.fmt "State_fstring_fmt_i_bitwidth " |> Fstring_fmt_i_bitwidth.pp v
    | State_fstring_fmt_r -> formatter |> Fmt.fmt "State_fstring_fmt_r"
    | State_fstring_fmt_r_bitwidth v ->
      formatter |> Fmt.fmt "State_fstring_fmt_r_bitwidth " |> Fstring_fmt_r_bitwidth.pp v
    | State_fstring_fmt_f_seen_start -> formatter |> Fmt.fmt "State_fstring_fmt_f_seen_start"
    | State_fstring_fmt_seen_start -> formatter |> Fmt.fmt "State_fstring_fmt_seen_start"
    | State_fstring_sep_space -> formatter |> Fmt.fmt "State_fstring_sep_space"
    | State_fstring_sep_op -> formatter |> Fmt.fmt "State_fstring_sep_op"
    | State_fstring_sep_op_space -> formatter |> Fmt.fmt "State_fstring_sep_op_space"
    | State_fstring_sep_seen_start -> formatter |> Fmt.fmt "State_fstring_sep_seen_start"
    | State_fstring_lparen -> formatter |> Fmt.fmt "State_fstring_lparen"

  let start_of_t t =
    match t.line_state, in_fstring t with
    | Line_begin, _
    | Line_whitespace, _
    | Line_start_col _, _ -> Some State_dentation_start
    | Line_body, false -> Some State_start
    | Line_body, true -> begin
        match List.hd t.fstring_states with
        | Fstring_spec_pct_seen [] -> Some State_fstring_pct_seen_start
        | Fstring_spec_pct_seen (_ :: _) -> None
        | Fstring_spec_pad_seen -> Some State_fstring_pad_seen_start
        | Fstring_spec_just_seen -> Some State_fstring_just_seen_start
        | Fstring_spec_sign_seen -> Some State_fstring_sign_seen_start
        | Fstring_spec_alt_seen -> Some State_fstring_alt_seen_start
        | Fstring_spec_zpad_seen -> Some State_fstring_zpad_seen_start
        | Fstring_spec_width_star_seen -> Some State_fstring_width_star_seen_start
        | Fstring_expr_width -> None
        | Fstring_spec_width_seen -> Some State_fstring_width_seen_start
        | Fstring_spec_pmode_seen -> Some State_fstring_pmode_seen_start
        | Fstring_spec_precision_star_seen -> Some State_fstring_precision_star_seen_start
        | Fstring_expr_precision -> None
        | Fstring_spec_precision_seen -> Some State_fstring_precision_seen_start
        | Fstring_spec_radix_seen -> Some State_fstring_radix_seen_start
        | Fstring_spec_notation_seen -> Some State_fstring_notation_seen_start
        | Fstring_spec_pretty_seen -> Some State_fstring_pretty_seen_start
        | Fstring_spec_fmt_f_seen -> Some State_fstring_fmt_f_seen_start
        | Fstring_expr_fmt -> None
        | Fstring_spec_fmt_seen -> Some State_fstring_fmt_seen_start
        | Fstring_spec_sep_seen -> Some State_fstring_sep_seen_start
        | Fstring_expr_value _ -> None
        | Fstring_value_seen _ -> None
        | Fstring_body -> Some (State_istring_body Istring_body.empty)
        | Fstring_rditto_seen _ -> None
      end
end

(* DFA engine and nodes. Most of the nodes are grouped into submodules so that they can share code
 * that is not relevant to the DFA as a whole. *)
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
    eoi1: 'a action1;
    edges1: (codepoint, 'a action1, Codepoint.cmper_witness) Map.t;
    default1: 'a action1;
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

  let accept_ctok ctok cursor t =
    {t with tok_base=cursor}, Accept ctok

  let accept atok cursor t =
    accept_ctok (ConcreteToken.ctok_at ~base:t.tok_base ~past:cursor atok) cursor t

  let accept_incl atok View.{cursor; _} t =
    accept atok cursor t

  let accept_excl atok View.{pcursor; _} t =
    accept atok pcursor t

  let accept_pexcl atok View.{ppcursor; _} t =
    accept atok ppcursor t

  let accept_source_directive atok View.{cursor; _} t =
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
    let cursor', level' = match atok with
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
    {t with tok_base=cursor'; level=level'}, Accept (ConcreteToken.ctok_at ~base ~past atok)

  let accept_line_break atok cursor t =
    {t with tok_base=cursor; line_state=Line_begin},
    Accept (ConcreteToken.ctok_at ~base:t.tok_base ~past:cursor atok)

  let accept_line_break_incl atok View.{cursor; _} t =
    accept_line_break atok cursor t

  let fstring_push fstring_state t =
    {t with fstring_states=fstring_state :: t.fstring_states}

  let fstring_pop t =
    {t with fstring_states=List.tl t.fstring_states}

  let fstring_trans fstring_state t =
    fstring_push fstring_state (fstring_pop t)

  let accept_fstring_push_ctok fstring_state ctok cursor t =
    accept_ctok ctok cursor (fstring_push fstring_state t)

  let accept_fstring_push_ctok_incl fstring_state ctok View.{cursor; _} t =
    accept_fstring_push_ctok fstring_state ctok cursor t

  let accept_fstring_pop_ctok ctok cursor t =
    accept_ctok ctok cursor (fstring_pop t)

  let accept_fstring_pop atok cursor t =
    accept_fstring_pop_ctok (ConcreteToken.ctok_at ~base:t.tok_base ~past:cursor atok) cursor t

  let accept_fstring_trans_ctok fstring_state ctok cursor t =
    accept_ctok ctok cursor (fstring_trans fstring_state t)

  let accept_fstring_trans_ctok_incl fstring_state ctok View.{cursor; _} t =
    accept_fstring_trans_ctok fstring_state ctok cursor t

  let accept_fstring_trans fstring_state atok cursor t =
    accept_fstring_trans_ctok fstring_state (ConcreteToken.ctok_at ~base:t.tok_base ~past:cursor
        atok) cursor t

  let accept_fstring_trans_incl trans atok View.{cursor; _} t =
    accept_fstring_trans trans atok cursor t

  let accept_fstring_trans_excl trans atok View.{pcursor; _} t =
    accept_fstring_trans trans atok pcursor t

  let retry_fstring_eoi t =
    let rec f t = begin
      let t' = fstring_pop t in
      match State.start_of_t t' with
      | Some state' -> t', Retry state'
      | None -> f t'
    end in
    f t

  let advance state' view t =
    t, Advance (view, state')

  let retry state' t =
    t, Retry state'

  let node0_start =
    {
      edges0=map_of_cps_alist [
        (",", accept_incl Tok_comma);
        (";", accept_incl Tok_semi);
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
        (ident_cident_cps, advance State_ident_cident);
        (ident_uident_cps, advance State_ident_uident);
        ("'", advance State_tick);
        ("\"", fun view t -> advance (State_istring_body (State.Istring_body.empty)) view t);
        ("`", fun (View.{cursor; _} as view) t ->
            advance (State_rstring_ltag (State.Rstring_ltag.init ~ltag_base:cursor)) view t);
        ("0", advance State_integer_0);
        (dec_lead_cps, fun (View.{pcursor; _} as view) t ->
            let digit = nat_of_cp (Source.Cursor.rget pcursor) in
            advance (State_integer_dec (State.Integer_dec.init ~n:digit)) view t
        );
      ];
      default0=(fun (View.{cursor; _} as view) t ->
        let mal = malformation ~base:t.tok_base ~past:cursor "Unsupported codepoint" in
        accept_incl (Tok_error [mal]) view t
      );
      eoi0=(fun view t ->
        let accept_dentation atok View.{cursor; _} t = begin
          accept atok cursor {t with line_state=Line_body}
        end in
        match Level.level t.level, t.line_state with
        | 0L, Line_begin -> accept_dentation Tok_line_delim view t
        | 0L, _ -> accept_incl Tok_end_of_input view t
        | _ -> accept_dentation (Tok_dedent (Constant ())) view {t with level=Level.pred t.level}
      );
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
      (String.join [" _"; ident_uident_cps], advance State_tick_lookahead);
      ("\n", accept_excl Tok_tick);
    ];
    default0=(fun _view t -> retry State_codepoint_tick t);
    eoi0=accept_incl Tok_tick;
  }

  let node0_tick_lookahead = {
    edges0=map_of_cps_alist [
      ("'", fun View.{ppcursor; cursor; _} t ->
          let cp = Source.Cursor.rget ppcursor in
          accept (Tok_codepoint (Constant cp)) cursor t
      );
    ];
    default0=accept_pexcl Tok_tick;
    eoi0=accept_excl Tok_tick;
  }

  module Real = struct
    type subtype =
      | Subtype_r32
      | Subtype_r64

    let accept_mals mals cursor t =
      let open AbstractToken in
      let malformed = Rendition.of_mals mals in
      accept (Tok_r64 malformed) cursor t

    let accept_mals_incl mals View.{cursor; _} t =
      accept_mals mals cursor t

    let accept_mals_excl mals View.{pcursor; _} t =
      accept_mals mals pcursor t

    let accept_precise subtype ~r cursor t =
      let open AbstractToken in
      let atok = match subtype with
        | Subtype_r32 -> Tok_r32 (
          match Realer.to_r32_opt r with
          | Some r32 -> (Constant r32)
          | None -> malformed (out_of_range_real t.tok_base cursor)
        )
        | Subtype_r64 -> Tok_r64 (
          match Realer.to_r64_opt r with
          | Some r64 -> (Constant r64)
          | None -> malformed (out_of_range_real t.tok_base cursor)
        )
      in
      accept atok cursor t

    let accept_precise_incl subtype ~r View.{cursor; _} t =
      accept_precise subtype ~r cursor t

    let accept_precise_excl subtype ~r View.{pcursor; _} t =
      accept_precise subtype ~r pcursor t

    let accept_approx subtype ~r cursor t =
      let open AbstractToken in
      let atok = match subtype with
        | Subtype_r32 -> Tok_r32 (Constant r)
        | Subtype_r64 -> Tok_r64 (Constant r)
      in
      accept atok cursor t

    let accept_approx_incl subtype ~r View.{cursor; _} t =
      accept_approx subtype ~r cursor t

    let accept_approx_excl subtype ~r View.{pcursor; _} t =
      accept_approx subtype ~r pcursor t

    let nat_32 = Nat.of_string "32"
    let nat_64 = Nat.of_string "64"

    let subtype_of_bitwidth bitwidth =
      match Nat.(bitwidth = nat_32), Nat.(bitwidth = nat_64) with
      | true, false -> Some Subtype_r32
      | false, true -> Some Subtype_r64
      | _ -> None

    let node1_precise_dot ~m_of_state ~under_advance ~digit_cps ~digit_advance ~ident_advance =
      {
        edges1=map_of_cpsets_alist [
          (cpset_of_cps "_", under_advance);
          (cpset_of_cps digit_cps, digit_advance);
          (cpset_of_cps "p", fun state view t ->
              let state' = match m_of_state state with
                | Ok m -> State.Real_p.init ~m
                | Error mals -> State.Real_p.mals ~mals
              in
              advance (State_real_p state') view t
          );
          (cpset_of_cps "r", fun state view t ->
              let state' = match m_of_state state with
                | Ok m -> State.Real_precise_r.init ~r:m
                | Error mals -> State.Real_precise_r.mals ~mals
              in
              advance (State_real_precise_r state') view t
          );
          (Set.diff (cpset_of_cps ident_cps)
              (Set.union (cpset_of_cps digit_cps) (cpset_of_cps "_pr")),
            ident_advance);
        ];
        default1=(fun state view t ->
          match m_of_state state with
          | Ok m -> accept_precise_excl Subtype_r64 ~r:m view t
          | Error mals -> accept_mals_excl mals view t
        );
        eoi1=(fun state view t ->
          match m_of_state state with
          | Ok m -> accept_precise_incl Subtype_r64 ~r:m view t
          | Error mals -> accept_mals_incl mals view t
        );
      }
    let node1_bin_dot = node1_precise_dot
        ~m_of_state:(fun state -> match state with
          | State.Real_bin_dot.R {m; point_shift=_} -> Ok m
          | State.Real_bin_dot.Malformations mals -> Error mals
        )
        ~under_advance:(fun state view t -> advance (State_real_bin_dot state) view t)
        ~digit_cps:bin_cps
        ~digit_advance:(fun state ({pcursor; _} as view) t ->
          let digit = nat_of_cp (Source.Cursor.rget pcursor) in
          advance (State_real_bin_dot (state |> State.Real_bin_dot.accum_digit digit)) view t
        )
        ~ident_advance:(fun state (View.{pcursor; cursor; _} as view) t ->
          let mal = invalid_numerical pcursor cursor in
          advance (State_real_bin_dot (state |> State.Real_bin_dot.accum_mal mal)) view t
        )
    let node1_oct_dot = node1_precise_dot
        ~m_of_state:(fun state -> match state with
          | State.Real_oct_dot.R {m; point_shift=_} -> Ok m
          | State.Real_oct_dot.Malformations mals -> Error mals
        )
        ~under_advance:(fun state view t -> advance (State_real_oct_dot state) view t)
        ~digit_cps:oct_cps
        ~digit_advance:(fun state ({pcursor; _} as view) t ->
          let digit = nat_of_cp (Source.Cursor.rget pcursor) in
          advance (State_real_oct_dot (state |> State.Real_oct_dot.accum_digit digit)) view t
        )
        ~ident_advance:(fun state (View.{pcursor; cursor; _} as view) t ->
          let mal = invalid_numerical pcursor cursor in
          advance (State_real_oct_dot (state |> State.Real_oct_dot.accum_mal mal)) view t
        )
    let node1_hex_dot = node1_precise_dot
        ~m_of_state:(fun state -> match state with
          | State.Real_hex_dot.R {m; point_shift=_} -> Ok m
          | State.Real_hex_dot.Malformations mals -> Error mals
        )
        ~under_advance:(fun state view t -> advance (State_real_hex_dot state) view t)
        ~digit_cps:hex_cps
        ~digit_advance:(fun state ({pcursor; _} as view) t ->
          let digit = nat_of_cp (Source.Cursor.rget pcursor) in
          advance (State_real_hex_dot (state |> State.Real_hex_dot.accum_digit digit)) view t
        )
        ~ident_advance:(fun state (View.{pcursor; cursor; _} as view) t ->
          let mal = invalid_numerical pcursor cursor in
          advance (State_real_hex_dot (state |> State.Real_hex_dot.accum_mal mal)) view t
        )

    let node1_dec_dot =
      let open State.Real_dec_dot in
      {
        edges1=map_of_cpsets_alist [
          (cpset_of_cps "_", fun state view t -> advance (State_real_dec_dot state) view t);
          (cpset_of_cps dec_cps, fun state ({pcursor; _} as view) t ->
              let digit = real_of_cp (Source.Cursor.rget pcursor) in
              advance (State_real_dec_dot (state |> State.Real_dec_dot.accum_digit digit)) view t
          );
          (cpset_of_cps "e", fun state view t ->
              let state' = match state with
                | R {m; ds=_} -> State.Real_e.init ~m
                | Malformations mals -> State.Real_e.mals ~mals
              in
              advance (State_real_e state') view t
          );
          (cpset_of_cps "r", fun state view t ->
              let state' = match state with
                | R {m; ds=_} -> State.Real_approx_r.init ~r:m
                | Malformations mals -> State.Real_approx_r.mals ~mals
              in
              advance (State_real_approx_r state') view t
          );
          (Set.diff (cpset_of_cps ident_cps)
              (Set.union (cpset_of_cps dec_cps) (cpset_of_cps "_er")),
            fun state (View.{pcursor; cursor; _} as view) t ->
              let mal = invalid_numerical pcursor cursor in
              advance (State_real_dec_dot (state |> State.Real_dec_dot.accum_mal mal)) view t
          );
        ];
        default1=(fun state view t ->
          match state with
          | R {m; ds=_} -> accept_approx_excl Subtype_r64 ~r:m view t
          | Malformations mals -> accept_mals_excl mals view t
        );
        eoi1=(fun state view t ->
          match state with
          | R {m; ds=_} -> accept_approx_incl Subtype_r64 ~r:m view t
          | Malformations mals -> accept_mals_incl mals view t
        );
      }

    let node1_p =
      let open State.Real_p in
      {
        edges1=map_of_cpsets_alist [
          (cpset_of_cps "_", fun state view t -> advance (State_real_p state) view t);
          (cpset_of_cps dec_cps, fun state (View.{pcursor; _} as view) t ->
              let state' = match state with
                | R {m} -> begin
                    let exp = nat_of_cp (Source.Cursor.rget pcursor) in
                    State.Real_p_exp.init ~m ~exp_sign:Pos ~exp
                  end
                | Malformations mals -> State.Real_p_exp.mals ~mals
              in
              advance (State_real_p_exp state') view t
          );
          (cpset_of_cps "-", fun state view t ->
              let state' = match state with
                | R {m} -> State.Real_p_sign.init ~m ~exp_sign:Neg
                | Malformations mals -> State.Real_p_sign.mals ~mals
              in
              advance (State_real_p_sign state') view t
          );
          (cpset_of_cps "+", fun state view t ->
              let state' = match state with
                | R {m} -> State.Real_p_sign.init ~m ~exp_sign:Pos
                | Malformations mals -> State.Real_p_sign.mals ~mals
              in
              advance (State_real_p_sign state') view t
          );
          (cpset_of_cps "r", fun state (View.{pcursor; cursor; _} as view) t ->
              let mal = invalid_numerical pcursor cursor in
              let mals = match state with
                | R _ -> [mal]
                | Malformations mals -> mal :: mals
              in
              advance (State_real_precise_r (State.Real_precise_r.mals ~mals)) view t
          );
          (Set.diff (cpset_of_cps ident_cps)
              (Set.union (cpset_of_cps dec_cps) (cpset_of_cps "_-+r")),
            fun state (View.{pcursor; cursor; _} as view) t ->
              let mal = invalid_numerical pcursor cursor in
              advance (State_real_p (state |> State.Real_p.accum_mal mal)) view t
          );
        ];
        default1=(fun state (View.{pcursor; cursor; _} as view) t ->
          let mal = invalid_numerical pcursor cursor in
          let mals = match state with
            | R _ -> [mal]
            | Malformations mals -> mal :: mals
          in
          accept_mals_excl mals view t
        );
        eoi1=(fun state (View.{cursor; _} as view) t ->
          let mal = invalid_numerical cursor cursor in
          let mals = match state with
            | R _ -> [mal]
            | Malformations mals -> mal :: mals
          in
          accept_mals_incl mals view t
        );
      }

    let node1_e =
      let open State.Real_e in
      {
        edges1=map_of_cpsets_alist [
          (cpset_of_cps "_", fun state view t -> advance (State_real_e state) view t);
          (cpset_of_cps dec_cps, fun state (View.{pcursor; _} as view) t ->
              let state' = match state with
                | R {m} -> begin
                    let exp = nat_of_cp (Source.Cursor.rget pcursor) in
                    State.Real_e_exp.init ~m ~exp_sign:Pos ~exp
                  end
                | Malformations mals -> State.Real_e_exp.mals ~mals
              in
              advance (State_real_e_exp state') view t
          );
          (cpset_of_cps "-", fun state view t ->
              let state' = match state with
                | R {m} -> State.Real_e_sign.init ~m ~exp_sign:Neg
                | Malformations mals -> State.Real_e_sign.mals ~mals
              in
              advance (State_real_e_sign state') view t
          );
          (cpset_of_cps "+", fun state view t ->
              let state' = match state with
                | R {m} -> State.Real_e_sign.init ~m ~exp_sign:Pos
                | Malformations mals -> State.Real_e_sign.mals ~mals
              in
              advance (State_real_e_sign state') view t
          );
          (cpset_of_cps "r", fun state (View.{pcursor; cursor; _} as view) t ->
              let mal = invalid_numerical pcursor cursor in
              let mals = match state with
                | R _ -> [mal]
                | Malformations mals -> mal :: mals
              in
              advance (State_real_approx_r (State.Real_approx_r.mals ~mals)) view t
          );
          (Set.diff (cpset_of_cps ident_cps)
              (Set.union (cpset_of_cps dec_cps) (cpset_of_cps "_-+r")),
            fun state (View.{pcursor; cursor; _} as view) t ->
              let mal = invalid_numerical pcursor cursor in
              advance (State_real_e (state |> State.Real_e.accum_mal mal)) view t
          );
        ];
        default1=(fun state (View.{pcursor; cursor; _} as view) t ->
          let mal = invalid_numerical pcursor cursor in
          let mals = match state with
            | R _ -> [mal]
            | Malformations mals -> mal :: mals
          in
          accept_mals_excl mals view t
        );
        eoi1=(fun state (View.{cursor; _} as view) t ->
          let mal = invalid_numerical cursor cursor in
          let mals = match state with
            | R _ -> [mal]
            | Malformations mals -> mal :: mals
          in
          accept_mals_incl mals view t
        );
      }

    let node1_p_sign =
      let open State.Real_p_sign in
      {
        edges1=map_of_cpsets_alist [
          (cpset_of_cps "_", fun state view t -> advance (State_real_p_sign state) view t);
          (cpset_of_cps dec_cps, fun state (View.{pcursor; _} as view) t ->
              let state' = match state with
                | R {m; exp_sign} -> begin
                    let exp = nat_of_cp (Source.Cursor.rget pcursor) in
                    State.Real_p_exp.init ~m ~exp_sign ~exp
                  end
                | Malformations mals -> State.Real_p_exp.mals ~mals
              in
              advance (State_real_p_exp state') view t
          );
          (cpset_of_cps "r", fun state (View.{pcursor; cursor; _} as view) t ->
              let mal = invalid_numerical pcursor cursor in
              let mals = match state with
                | R _ -> [mal]
                | Malformations mals -> mal :: mals
              in
              advance (State_real_precise_r (State.Real_precise_r.mals ~mals)) view t
          );
          (Set.union (Set.diff (cpset_of_cps ident_cps)
              (Set.union (cpset_of_cps dec_cps) (cpset_of_cps "_r"))) (cpset_of_cps "-+"),
            fun state (View.{pcursor; cursor; _} as view) t ->
              let mal = invalid_numerical pcursor cursor in
              advance (State_real_p_sign (state |> State.Real_p_sign.accum_mal mal)) view t
          );
        ];
        default1=(fun state (View.{pcursor; cursor; _} as view) t ->
          let mal = invalid_numerical pcursor cursor in
          let mals = match state with
            | R _ -> [mal]
            | Malformations mals -> mal :: mals
          in
          accept_mals_excl mals view t
        );
        eoi1=(fun state (View.{cursor; _} as view) t ->
          let mal = invalid_numerical cursor cursor in
          let mals = match state with
            | R _ -> [mal]
            | Malformations mals -> mal :: mals
          in
          accept_mals_incl mals view t
        );
      }

    let node1_e_sign =
      let open State.Real_e_sign in
      {
        edges1=map_of_cpsets_alist [
          (cpset_of_cps "_", fun state view t -> advance (State_real_e_sign state) view t);
          (cpset_of_cps dec_cps, fun state (View.{pcursor; _} as view) t ->
              let state' = match state with
                | R {m; exp_sign} -> begin
                    let exp = nat_of_cp (Source.Cursor.rget pcursor) in
                    State.Real_e_exp.init ~m ~exp_sign ~exp
                  end
                | Malformations mals -> State.Real_e_exp.mals ~mals
              in
              advance (State_real_e_exp state') view t
          );
          (cpset_of_cps "r", fun state (View.{pcursor; cursor; _} as view) t ->
              let mal = invalid_numerical pcursor cursor in
              let mals = match state with
                | R _ -> [mal]
                | Malformations mals -> mal :: mals
              in
              advance (State_real_approx_r (State.Real_approx_r.mals ~mals)) view t
          );
          (Set.union (Set.diff (cpset_of_cps ident_cps)
              (Set.union (cpset_of_cps dec_cps) (cpset_of_cps "_r"))) (cpset_of_cps "-+"),
            fun state (View.{pcursor; cursor; _} as view) t ->
              let mal = invalid_numerical pcursor cursor in
              advance (State_real_e_sign (state |> State.Real_e_sign.accum_mal mal)) view t
          );
        ];
        default1=(fun state (View.{pcursor; cursor; _} as view) t ->
          let mal = invalid_numerical pcursor cursor in
          let mals = match state with
            | R _ -> [mal]
            | Malformations mals -> mal :: mals
          in
          accept_mals_excl mals view t
        );
        eoi1=(fun state (View.{cursor; _} as view) t ->
          let mal = invalid_numerical cursor cursor in
          let mals = match state with
            | R _ -> [mal]
            | Malformations mals -> mal :: mals
          in
          accept_mals_incl mals view t
        );
      }

    let node1_p_exp =
      let open State.Real_p_exp in
      {
        edges1=map_of_cpsets_alist [
          (cpset_of_cps "_", fun state view t -> advance (State_real_p_exp state) view t);
          (cpset_of_cps dec_cps, fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_real_p_exp (state |> State.Real_p_exp.accum_digit digit)) view t
          );
          (cpset_of_cps "r", fun state view t ->
              let state' = match to_r state with
                | Ok r -> State.Real_precise_r.init ~r
                | Error mals -> State.Real_precise_r.mals ~mals
              in
              advance (State_real_precise_r state') view t
          );
          (Set.diff (cpset_of_cps ident_cps)
              (Set.union (cpset_of_cps dec_cps) (cpset_of_cps "_r")),
            fun state (View.{pcursor; cursor; _} as view) t ->
              let mal = invalid_numerical pcursor cursor in
              advance (State_real_p_exp (state |> State.Real_p_exp.accum_mal mal)) view t
          );
        ];
        default1=(fun state view t ->
          match to_r state with
          | Ok r -> accept_precise_excl Subtype_r64 ~r view t
          | Error mals -> accept_mals_excl mals view t
        );
        eoi1=(fun state view t ->
          match to_r state with
          | Ok r -> accept_precise_incl Subtype_r64 ~r view t
          | Error mals -> accept_mals_incl mals view t
        );
      }

    let node1_e_exp =
      let open State.Real_e_exp in
      {
        edges1=map_of_cpsets_alist [
          (cpset_of_cps "_", fun state view t -> advance (State_real_e_exp state) view t);
          (cpset_of_cps dec_cps, fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_real_e_exp (state |> State.Real_e_exp.accum_digit digit)) view t
          );
          (cpset_of_cps "r", fun state view t ->
              let state' = match to_r state with
                | Ok r -> State.Real_approx_r.init ~r
                | Error mals -> State.Real_approx_r.mals ~mals
              in
              advance (State_real_approx_r state') view t
          );
          (Set.diff (cpset_of_cps ident_cps)
              (Set.union (cpset_of_cps dec_cps) (cpset_of_cps "_r")),
            fun state (View.{pcursor; cursor; _} as view) t ->
              let mal = invalid_numerical pcursor cursor in
              advance (State_real_e_exp (state |> State.Real_e_exp.accum_mal mal)) view t
          );
        ];
        default1=(fun state view t ->
          match to_r state with
          | Ok r -> accept_approx_excl Subtype_r64 ~r view t
          | Error mals -> accept_mals_excl mals view t
        );
        eoi1=(fun state view t ->
          match to_r state with
          | Ok r -> accept_approx_incl Subtype_r64 ~r view t
          | Error mals -> accept_mals_incl mals view t
        );
      }

    let node1_precise_r =
      let open State.Real_precise_r in
      {
        edges1=map_of_cpsets_alist [
          ((cpset_of_cps dec_lead_cps), fun state (View.{pcursor; _} as view) t ->
              let state' = match state with
                | R {r} -> begin
                    let digit = nat_of_cp (Source.Cursor.rget pcursor) in
                    State.Real_precise_r_bitwidth.init ~r ~bitwidth:digit
                  end
                | Malformations mals -> State.Real_precise_r_bitwidth.mals ~mals
              in
              advance (State_real_precise_r_bitwidth state') view t
          );
          (cpset_of_cps "0",
            fun state (View.{pcursor; cursor; _} as view) t ->
              let mal = unsupported_bitwidth pcursor cursor in
              advance (State_real_precise_r (state |> State.Real_precise_r.accum_mal mal)) view t
          );
          (Set.diff (cpset_of_cps ident_cps) (cpset_of_cps dec_cps),
            fun state (View.{pcursor; cursor; _} as view) t ->
              let mal = invalid_numerical pcursor cursor in
              advance (State_real_precise_r (state |> State.Real_precise_r.accum_mal mal)) view t
          );
        ];
        default1=(fun state view t ->
          match state with
          | R {r} -> accept_precise_excl Subtype_r64 ~r view t
          | Malformations mals -> accept_mals_excl mals view t
        );
        eoi1=(fun state view t ->
          match state with
          | R {r} -> accept_precise_incl Subtype_r64 ~r view t
          | Malformations mals -> accept_mals_incl mals view t
        );
      }

    let node1_approx_r =
      let open State.Real_approx_r in
      {
        edges1=map_of_cpsets_alist [
          ((cpset_of_cps dec_lead_cps), fun state (View.{pcursor; _} as view) t ->
              let state' = match state with
                | R {r} -> begin
                    let digit = nat_of_cp (Source.Cursor.rget pcursor) in
                    State.Real_approx_r_bitwidth.init ~r ~bitwidth:digit
                  end
                | Malformations mals -> State.Real_approx_r_bitwidth.mals ~mals
              in
              advance (State_real_approx_r_bitwidth state') view t
          );
          (cpset_of_cps "0",
            fun state (View.{pcursor; cursor; _} as view) t ->
              let mal = unsupported_bitwidth pcursor cursor in
              advance (State_real_approx_r (state |> State.Real_approx_r.accum_mal mal)) view t
          );
          (Set.diff (cpset_of_cps ident_cps) (cpset_of_cps dec_cps),
            fun state (View.{pcursor; cursor; _} as view) t ->
              let mal = invalid_numerical pcursor cursor in
              advance (State_real_approx_r (state |> State.Real_approx_r.accum_mal mal)) view t
          );
        ];
        default1=(fun state view t ->
          match state with
          | R {r} -> accept_approx_excl Subtype_r64 ~r view t
          | Malformations mals -> accept_mals_excl mals view t
        );
        eoi1=(fun state view t ->
          match state with
          | R {r} -> accept_approx_incl Subtype_r64 ~r view t
          | Malformations mals -> accept_mals_incl mals view t
        );
      }

    let node1_precise_r_bitwidth =
      let open State.Real_precise_r_bitwidth in
      {
        edges1=map_of_cpsets_alist [
          ((cpset_of_cps dec_cps), fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_real_precise_r_bitwidth
                  (state |> State.Real_precise_r_bitwidth.accum_digit digit)) view t
          );
          (Set.diff (cpset_of_cps ident_cps) (cpset_of_cps dec_cps),
            fun state (View.{pcursor; cursor; _} as view) t ->
              let mal = invalid_numerical pcursor cursor in
              advance (State_real_precise_r_bitwidth
                  (state |> State.Real_precise_r_bitwidth.accum_mal mal)) view t
          );
        ];
        default1=(fun state (View.{cursor; _} as view) t ->
          match state with
          | R {r; bitwidth} -> begin
              match subtype_of_bitwidth bitwidth with
              | Some subtype -> accept_precise_excl subtype ~r view t
              | None -> begin
                  let mal = unsupported_bitwidth t.tok_base cursor in
                  accept_mals_excl [mal] view t
                end
            end
          | Malformations mals -> accept_mals_excl mals view t
        );
        eoi1=(fun state (View.{cursor; _} as view) t ->
          match state with
          | R {r; bitwidth} -> begin
              match subtype_of_bitwidth bitwidth with
              | Some subtype -> accept_precise_incl subtype ~r view t
              | None -> begin
                  let mal = unsupported_bitwidth t.tok_base cursor in
                  accept_mals_incl [mal] view t
                end
            end
          | Malformations mals -> accept_mals_incl mals view t
        );
      }

    let node1_approx_r_bitwidth =
      let open State.Real_approx_r_bitwidth in
      {
        edges1=map_of_cpsets_alist [
          ((cpset_of_cps dec_cps), fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_real_approx_r_bitwidth
                  (state |> State.Real_approx_r_bitwidth.accum_digit digit)) view t
          );
          (Set.diff (cpset_of_cps ident_cps) (cpset_of_cps dec_cps),
            fun state (View.{pcursor; cursor; _} as view) t ->
              let mal = invalid_numerical pcursor cursor in
              advance (State_real_approx_r_bitwidth
                  (state |> State.Real_approx_r_bitwidth.accum_mal mal)) view t
          );
        ];
        default1=(fun state (View.{cursor; _} as view) t ->
          match state with
          | R {r; bitwidth} -> begin
              match subtype_of_bitwidth bitwidth with
              | Some subtype -> accept_approx_excl subtype ~r view t
              | None -> begin
                  let mal = unsupported_bitwidth t.tok_base cursor in
                  accept_mals_excl [mal] view t
                end
            end
          | Malformations mals -> accept_mals_excl mals view t
        );
        eoi1=(fun state (View.{cursor; _} as view) t ->
          match state with
          | R {r; bitwidth} -> begin
              match subtype_of_bitwidth bitwidth with
              | Some subtype -> accept_approx_incl subtype ~r view t
              | None -> begin
                  let mal = unsupported_bitwidth t.tok_base cursor in
                  accept_mals_incl [mal] view t
                end
            end
          | Malformations mals -> accept_mals_incl mals view t
        );
      }
  end

  module Integer = struct
    type signedness =
      | Unsigned
      | Signed

    (* Full enumeration of integer types is a bit unwieldy, but it's more robust and doesn't
     * actually cause much code bloat. *)
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
      match subtype with
      | Subtype_u8 -> Some U8.(extend_to_nat max_value)
      | Subtype_i8 -> Some Sint.(widen_to_nat_hlt (abs (I8.(extend_to_sint min_value))))
      | Subtype_u16 -> Some U16.(extend_to_nat max_value)
      | Subtype_i16 -> Some Sint.(widen_to_nat_hlt (abs (I16.(extend_to_sint min_value))))
      | Subtype_u32 -> Some U32.(extend_to_nat max_value)
      | Subtype_i32 -> Some Sint.(widen_to_nat_hlt (abs (I32.(extend_to_sint min_value))))
      | Subtype_u64 -> Some U64.(extend_to_nat max_value)
      | Subtype_i64 -> Some (Nat.like_of_zint_hlt (Zint.abs I64.(extend_to_zint min_value)))
      | Subtype_u128 -> Some U128.(extend_to_nat max_value)
      | Subtype_i128 -> Some (Nat.like_of_zint_hlt (Zint.abs I128.(extend_to_zint min_value)))
      | Subtype_u256 -> Some U256.(extend_to_nat max_value)
      | Subtype_i256 -> Some (Nat.like_of_zint_hlt (Zint.abs I256.(extend_to_zint min_value)))
      | Subtype_u512 -> Some U512.(extend_to_nat max_value)
      | Subtype_i512 -> Some (Nat.like_of_zint_hlt (Zint.abs I512.(extend_to_zint min_value)))
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
      let atok = match limit with
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
            (* Use truncation for signed types rather than narrowing, in order to support min_value
             * for signed types. Unsigned types could use `narrow_of_nat_hlt`, but range checking
             * was already done so there's no need to validate again. *)
            match subtype with
            | Subtype_u8 -> Tok_u8 (Constant (U8.trunc_of_nat n))
            | Subtype_i8 -> Tok_i8 (Constant (I8.trunc_of_nat n))
            | Subtype_u16 -> Tok_u16 (Constant (U16.trunc_of_nat n))
            | Subtype_i16 -> Tok_i16 (Constant (I16.trunc_of_nat n))
            | Subtype_u32 -> Tok_u32 (Constant (U32.trunc_of_nat n))
            | Subtype_i32 -> Tok_i32 (Constant (I32.trunc_of_nat n))
            | Subtype_u64 -> Tok_u64 (Constant (U64.trunc_of_nat n))
            | Subtype_i64 -> Tok_i64 (Constant (I64.trunc_of_nat n))
            | Subtype_u128 -> Tok_u128 (Constant (U128.trunc_of_nat n))
            | Subtype_i128 -> Tok_i128 (Constant (I128.trunc_of_nat n))
            | Subtype_u256 -> Tok_u256 (Constant (U256.trunc_of_nat n))
            | Subtype_i256 -> Tok_i256 (Constant (I256.trunc_of_nat n))
            | Subtype_u512 -> Tok_u512 (Constant (U512.trunc_of_nat n))
            | Subtype_i512 -> Tok_i512 (Constant (I512.trunc_of_nat n))
            | Subtype_nat -> Tok_nat (Constant n)
            | Subtype_zint -> Tok_zint (Constant (Nat.bits_to_zint n))
          end
      in
      accept atok cursor t

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

    let accept_mal cursor t =
      let mal = malformed (invalid_numerical t.tok_base cursor) in
      accept (Tok_u64 mal) cursor t

    let accept_mal_incl View.{cursor; _} t =
      accept_mal cursor t

    let accept_mal_excl View.{pcursor; _} t =
      accept_mal pcursor t

    let node0_0 = {
      edges0=map_of_cpsets_alist [
        (cpset_of_cps "_", advance (State_integer_dec (State.Integer_dec.init ~n:Nat.k_0)));
        (cpset_of_cps dec_cps, fun (View.{pcursor; _} as view) t ->
            let digit = nat_of_cp (Source.Cursor.rget pcursor) in
            advance (State_integer_dec (State.Integer_dec.init ~n:digit)) view t;
        );
        (cpset_of_cps "b", advance State_integer_0b);
        (cpset_of_cps "o", advance State_integer_0o);
        (cpset_of_cps "x", advance State_integer_0x);
        (cpset_of_cps "u", advance (State_integer_u (State.Integer_u.init ~n:Nat.k_0 ~radix:Dec)));
        (cpset_of_cps "i", advance (State_integer_i (State.Integer_i.init ~n:Nat.k_0 ~radix:Dec)));
        (cpset_of_cps "n", advance (State_integer_n (State.Integer_n.init ~n:Nat.k_0 ~radix:Dec)));
        (cpset_of_cps "z", advance (State_integer_z (State.Integer_z.init ~n:Nat.k_0 ~radix:Dec)));
        (cpset_of_cps "r", advance (State_real_approx_r (State.Real_approx_r.init ~r:0.)));
        (cpset_of_cps ".", advance State_integer_0_dot);
        (cpset_of_cps "e", advance (State_real_e (State.Real_e.init ~m:0.)));
        (Set.diff (cpset_of_cps ident_cps)
            (Set.union (cpset_of_cps dec_cps) (cpset_of_cps "_beinoruxz")),
          advance State_integer_mal_ident);
      ];
      default0=accept_zero_excl;
      eoi0=accept_zero_incl;
    }

    let node0_0_dot = {
      edges0=map_of_cpsets_alist [
        (cpset_of_cps operator_cps, accept_zero_pexcl);
        (cpset_of_cps "e", advance (State_real_e (State.Real_e.init ~m:0.)));
        (cpset_of_cps dec_cps, fun (View.{pcursor; _} as view) t ->
            let digit = real_of_cp (Source.Cursor.rget pcursor) in
            advance (State_real_dec_dot (State.Real_dec_dot.(init ~m:0. |> accum_digit digit))) view
              t
        );
        (cpset_of_cps "_", advance (State_real_dec_dot (State.Real_dec_dot.init ~m:0.)));
        (cpset_of_cps "r", advance (State_real_approx_r (State.Real_approx_r.init ~r:0.)));
        (Set.diff (cpset_of_cps ident_cps) (Set.union (cpset_of_cps dec_cps) (cpset_of_cps "er_")),
          fun (View.{cursor; _} as view) t ->
            let mal = invalid_numerical t.tok_base cursor in
            advance (State_real_dec_dot (State.Real_dec_dot.mals ~mals:[mal])) view t
        );
      ];
      default0=accept_excl (AbstractToken.Tok_r64 (Constant 0.));
      eoi0=accept_incl (AbstractToken.Tok_r64 (Constant 0.));
    }

    let node0_0box ~base_cps ~state ~state_base_init =
      {
        edges0=map_of_cpsets_alist [
          (cpset_of_cps "_", advance state);
          (cpset_of_cps base_cps, fun (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (state_base_init digit) view t
          );
          (Set.diff (cpset_of_cps ident_cps) (Set.union (cpset_of_cps "_") (cpset_of_cps base_cps)),
            advance State_integer_mal_ident);
          (cpset_of_cps ".", accept_mal_incl);
        ];
        default0=accept_mal_excl;
        eoi0=accept_mal_incl;
      }
    let node0_0b = node0_0box ~base_cps:bin_cps ~state:State_integer_0b
        ~state_base_init:(fun n -> State_integer_bin (State.Integer_bin.init ~n))
    let node0_0o = node0_0box ~base_cps:oct_cps ~state:State_integer_0o
        ~state_base_init:(fun n -> State_integer_oct (State.Integer_oct.init ~n))
    let node0_0x = node0_0box ~base_cps:hex_cps ~state:State_integer_0x
        ~state_base_init:(fun n -> State_integer_hex (State.Integer_hex.init ~n))

    let node1_base ~radix ~base_cps ~state_init ~ep_cp ~ep_advance ~n_of_state ~accum_digit
        ~state_dot_init ~r_advance =
      {
        edges1=map_of_cpsets_alist [
          (cpset_of_cps "_", fun state view t -> advance (state_init state) view t);
          (cpset_of_cps base_cps, fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (state_init (state |> accum_digit digit)) view t);
          (cpset_of_cps ".", fun state view t -> advance (state_dot_init state) view t);
          (cpset_of_cps ep_cp, ep_advance);
          (cpset_of_cps "u", fun state view t ->
              let n = n_of_state state in
              advance (State_integer_u (State.Integer_u.init ~n ~radix)) view t
          );
          (cpset_of_cps "i", fun state view t ->
              let n = n_of_state state in
              advance (State_integer_i (State.Integer_i.init ~n ~radix)) view t
          );
          (cpset_of_cps "n", fun state view t ->
              let n = n_of_state state in
              advance (State_integer_n (State.Integer_n.init ~n ~radix)) view t
          );
          (cpset_of_cps "z", fun state view t ->
              let n = n_of_state state in
              advance (State_integer_z (State.Integer_z.init ~n ~radix)) view t
          );
          (cpset_of_cps "r", r_advance);
          (Set.diff (cpset_of_cps ident_cps)
              (Set.union (cpset_of_cps base_cps) (cpset_of_cps (String.join ["_inruz"; ep_cp]))),
            fun _state view t -> advance State_integer_mal_ident view t);
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
        ~ep_cp:"p"
        ~ep_advance:(fun State.Integer_bin.{n} view t ->
          let m = realer_of_nat n in
          advance (State_real_p (State.Real_p.init ~m)) view t
        )
        ~accum_digit:State.Integer_bin.accum_digit
        ~state_dot_init:(fun state -> State.State_integer_bin_dot state)
        ~r_advance:(fun State.Integer_bin.{n} view t ->
          let r = realer_of_nat n in
          advance (State_real_precise_r (State.Real_precise_r.init ~r)) view t
        )
    let node1_oct =
      node1_base ~radix:Oct ~base_cps:oct_cps
        ~state_init:(fun state -> State.State_integer_oct state)
        ~n_of_state:(fun State.Integer_oct.{n} -> n)
        ~ep_cp:"p"
        ~ep_advance:(fun State.Integer_oct.{n} view t ->
          let m = realer_of_nat n in
          advance (State_real_p (State.Real_p.init ~m)) view t
        )
        ~accum_digit:State.Integer_oct.accum_digit
        ~state_dot_init:(fun state -> State.State_integer_oct_dot state)
        ~r_advance:(fun State.Integer_oct.{n} view t ->
          let r = realer_of_nat n in
          advance (State_real_precise_r (State.Real_precise_r.init ~r)) view t
        )
    let node1_dec =
      node1_base ~radix:Dec ~base_cps:dec_cps
        ~state_init:(fun state -> State.State_integer_dec state)
        ~n_of_state:(fun State.Integer_dec.{n} -> n)
        ~ep_cp:"e"
        ~ep_advance:(fun State.Integer_dec.{n} view t ->
          let m = Nat.to_real n in
          advance (State_real_e (State.Real_e.init ~m)) view t
        )
        ~accum_digit:State.Integer_dec.accum_digit
        ~state_dot_init:(fun state -> State.State_integer_dec_dot state)
        ~r_advance:(fun State.Integer_dec.{n} view t ->
          let r = Nat.to_real n in
          advance (State_real_approx_r (State.Real_approx_r.init ~r)) view t
        )
    let node1_hex =
      node1_base ~radix:Hex ~base_cps:hex_cps
        ~state_init:(fun state -> State.State_integer_hex state)
        ~n_of_state:(fun State.Integer_hex.{n} -> n)
        ~ep_cp:"p"
        ~ep_advance:(fun State.Integer_hex.{n} view t ->
          let m = realer_of_nat n in
          advance (State_real_p (State.Real_p.init ~m)) view t
        )
        ~accum_digit:State.Integer_hex.accum_digit
        ~state_dot_init:(fun state -> State.State_integer_hex_dot state)
        ~r_advance:(fun State.Integer_hex.{n} view t ->
          let r = realer_of_nat n in
          advance (State_real_precise_r (State.Real_precise_r.init ~r)) view t
        )

    let node1_base_dot ~radix ~n_of_state ~real_dot_retry =
      {
        edges1=map_of_cps_alist [
          (".", fun state view t ->
              let n = n_of_state state in
              accept_integer_pexcl n radix view t
          );
        ];
        default1=real_dot_retry;
        eoi1=real_dot_retry;
      }
    let node1_bin_dot =
      node1_base_dot ~radix:Bin
        ~n_of_state:(fun State.Integer_bin_dot.{n} -> n)
        ~real_dot_retry:(fun State.Integer_bin_dot.{n} _view t ->
          let m = realer_of_nat n in
          retry (State_real_bin_dot (State.Real_bin_dot.init ~m)) t
        )
    let node1_oct_dot =
      node1_base_dot ~radix:Oct
        ~n_of_state:(fun State.Integer_oct_dot.{n} -> n)
        ~real_dot_retry:(fun State.Integer_oct_dot.{n} _view t ->
          let m = realer_of_nat n in
          retry (State_real_oct_dot (State.Real_oct_dot.init ~m)) t
        )
    let node1_dec_dot =
      node1_base_dot ~radix:Dec
        ~n_of_state:(fun State.Integer_dec_dot.{n} -> n)
        ~real_dot_retry:(fun State.Integer_bin_dot.{n} _view t ->
          let m = Nat.to_real n in
          retry (State_real_dec_dot (State.Real_dec_dot.init ~m)) t
        )
    let node1_hex_dot =
      node1_base_dot ~radix:Hex
        ~n_of_state:(fun State.Integer_hex_dot.{n} -> n)
        ~real_dot_retry:(fun State.Integer_hex_dot.{n} _view t ->
          let m = realer_of_nat n in
          retry (State_real_hex_dot (State.Real_hex_dot.init ~m)) t
        )

    let node1_u =
      let open State.Integer_u in
      {
        edges1=map_of_cps_alist [
          (dec_cps, fun {n; radix} (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_integer_u_bitwidth (State.Integer_u_bitwidth.init ~n ~radix
                  ~bitwidth:digit)) view t
          );
        ];
        default1=(fun {n; radix} view t -> accept_integer_excl ~subtype:Subtype_u64 n radix view t);
        eoi1=(fun {n; radix} view t -> accept_integer_incl ~subtype:Subtype_u64 n radix view t);
      }

    let node1_i =
      let open State.Integer_i in
      {
        edges1=map_of_cps_alist [
          (dec_cps, fun {n; radix} (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_integer_i_bitwidth (State.Integer_i_bitwidth.init ~n ~radix
                  ~bitwidth:digit)) view t
          );
        ];
        default1=(fun {n; radix} view t -> accept_integer_excl ~subtype:Subtype_i64 n radix view t);
        eoi1=(fun {n; radix} view t -> accept_integer_incl ~subtype:Subtype_i64 n radix view t);
      }

    let node1_n =
      let open State.Integer_n in
      {
        edges1=map_of_cps_alist [
          (dec_cps, fun _state view t -> advance State_integer_mal_ident view t);
        ];
        default1=(fun {n; radix} view t -> accept_integer_excl ~subtype:Subtype_nat n radix view t);
        eoi1=(fun {n; radix} view t -> accept_integer_incl ~subtype:Subtype_nat n radix view t);
      }

    let node1_z =
      let open State.Integer_z in
      {
        edges1=map_of_cps_alist [
          (dec_cps, fun _state view t -> advance State_integer_mal_ident view t);
        ];
        default1=(fun {n; radix} view t ->
          accept_integer_excl ~subtype:Subtype_zint n radix view t);
        eoi1=(fun {n; radix} view t -> accept_integer_incl ~subtype:Subtype_zint n radix view t);
      }

    let node1_u_bitwidth =
      let open State.Integer_u_bitwidth in
      {
        edges1=map_of_cps_alist [
          (dec_cps, fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_integer_u_bitwidth (state |> accum_bitwidth digit)) view t
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
          (dec_cps, fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_integer_i_bitwidth (state |> accum_bitwidth digit)) view t
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
      default0=accept_mal_excl;
      eoi0=accept_mal_incl;
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
      let atok = match Map.get uident_str keyword_map with
        | Some atok -> atok
        | None -> Tok_uident (Constant uident_str)
      in
      accept atok cursor t

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
        ("..", Tok_dot_dot);
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
      | Some atok -> accept atok cursor t

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
        ("*", fun state view t -> advance (State_paren_comment_star state) view t);
        ("(", fun state view t -> advance (State_paren_comment_lparen state) view t);
      ];
      default1;
      eoi1;
    }

    let node1_lparen = {
      edges1=map_of_cps_alist [
        ("*", fun State.Paren_comment_lparen.{nesting} view t ->
            advance (State_paren_comment_body (State.Paren_comment_body.init ~nesting:(succ
                nesting))) view t
        );
        ("(", fun state view t -> advance (State_paren_comment_lparen state) view t);
      ];
      default1;
      eoi1;
    }

    let node1_star = {
      edges1=map_of_cps_alist [
        ("*", fun state view t -> advance (State_paren_comment_star state) view t);
        ("(", fun state view t -> advance (State_paren_comment_lparen state) view t);
        (")", fun {nesting} view t ->
            match nesting with
            | 1L -> accept_incl (Tok_paren_comment (Constant ())) view t
            | _ ->
              advance (State_paren_comment_body (State.Paren_comment_body.init
                  ~nesting:(pred nesting))) view t
        );
      ];
      default1;
      eoi1;
    }
  end

  module SrcDirective = struct
    let max_abs_i64 = Nat.like_of_zint_hlt (Zint.abs I64.(extend_to_zint min_value))

    let render_source_directive ~mals ~path ~line ~indent ~omit =
      match mals with
      | [] -> begin
          let path = match path with
            | None -> None
            | Some cps -> Some (Path.of_string (String.of_list_rev cps))
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
        (dec_lead_cps, fun ({pcursor; _} as view) t ->
            let digit = nat_of_cp (Source.Cursor.rget pcursor) in
            advance (State_src_directive_line (State.Src_directive_line.init ~mals:[] ~path:None
              ~line_cursor:pcursor ~line:(Some digit))) view t);
        ("]", fun view t ->
            let atok =
              render_source_directive ~mals:[] ~path:None ~line:None ~indent:None ~omit:None in
            accept_source_directive atok view t
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
          ("%", fun state ({pcursor; cursor; _} as view) t ->
              let mal = missing_backslash pcursor cursor in
              advance (State_src_directive_path (state |> accum_mals mal)) view t
          );
          ("\"", fun state view t -> advance (State_src_directive_rditto state) view t);
          ("\\", fun {mals; path} ({pcursor; _} as view) t ->
              advance (State_src_directive_path_bslash {mals; path; bslash_cursor=pcursor}) view t
          );
        ];
        default1=(fun state ({pcursor; _} as view) t ->
          advance (State_src_directive_path (state |> accum_path (Source.Cursor.rget pcursor))) view
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
          ("u", fun state view t -> advance (State_src_directive_path_bslash_u state) view t);
          ("t", fun state view t ->
              advance (State_src_directive_path (state |> accum_path Codepoint.ht)) view t);
          ("n", fun state view t ->
              advance (State_src_directive_path (state |> accum_path Codepoint.nl)) view t);
          ("r", fun state view t ->
              advance (State_src_directive_path (state |> accum_path Codepoint.cr)) view t);
          ("\"\\%", fun state ({pcursor; _} as view) t ->
              advance (State_src_directive_path (state |> accum_path (Source.Cursor.rget pcursor)))
                view t
          );
        ];
        default1=(fun ({bslash_cursor; _} as state) {cursor; _} t ->
          let mal = illegal_backslash bslash_cursor cursor in
          retry (State_src_directive_path (state |> accum_mals mal)) t
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
          ("{", fun {mals; path; bslash_cursor} view t ->
              advance (State_src_directive_path_bslash_u_lcurly
                  (State.Src_directive_path_bslash_u_lcurly.init ~mals ~path ~bslash_cursor)) view t
          );
          ("\"", fun ({bslash_cursor; _} as state) ({cursor; _} as view) t ->
              let mal = invalid_unicode_escape bslash_cursor cursor in
              advance (State_src_directive_rditto (state |> accum_mals mal)) view t
          );
          ("]", fun {mals; _} {pcursor; cursor; _} t ->
              let mal = unexpected_codepoint_source_directive pcursor cursor in
              accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
          );
        ];
        default1=(fun ({bslash_cursor; _} as state) ({cursor; _} as view) t ->
          let mal = invalid_unicode_escape bslash_cursor cursor in
          advance (State_src_directive_path (state |> accum_mals mal)) view t
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
          ("_", fun state view t ->
              advance (State_src_directive_path_bslash_u_lcurly state) view t);
          (hex_cps, fun {mals; path; bslash_cursor} ({pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_src_directive_path_bslash_u_lcurly_hex
                  (State.Src_directive_path_bslash_u_lcurly_hex.init ~mals ~path ~bslash_cursor
                      ~hex:digit)) view t
          );
          ("\"", fun ({bslash_cursor; _} as state) ({cursor; _} as view) t ->
              let mal = invalid_unicode_escape bslash_cursor cursor in
              advance (State_src_directive_rditto (state |> accum_mals mal)) view t
          );
          ("]", fun {mals; _} {pcursor; cursor; _} t ->
              let mal = unexpected_codepoint_source_directive pcursor cursor in
              accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
          );
        ];
        default1=(fun ({bslash_cursor; _} as state) ({cursor; _} as view) t ->
          let mal = invalid_unicode_escape bslash_cursor cursor in
          advance (State_src_directive_path (state |> accum_mals mal)) view t
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
          ("_", fun state view t ->
              advance (State_src_directive_path_bslash_u_lcurly_hex state) view t);
          (hex_cps, fun state ({pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_src_directive_path_bslash_u_lcurly_hex (state |> accum_hex digit)) view
                t
          );
          ("}", fun ({bslash_cursor; hex; _} as state) ({cursor; _} as view) t ->
              advance (State_src_directive_path (
                Option.value_map (Nat.to_uns_opt hex)
                  ~f:(fun u -> Codepoint.narrow_of_uns_opt u)
                  ~default:None
                |> Option.value_map
                  ~f:(fun cp -> (state |> accum_path cp))
                  ~default:(state |> accum_mals (invalid_unicode_escape bslash_cursor cursor))
              )) view t
          );
          ("\"", fun ({bslash_cursor; _} as state) ({cursor; _} as view) t ->
              let mal = invalid_unicode_escape bslash_cursor cursor in
              advance (State_src_directive_rditto (state |> accum_mals mal)) view t
          );
          ("]", fun {mals; _} {pcursor; cursor; _} t ->
              let mal = unexpected_codepoint_source_directive pcursor cursor in
              accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
          );
        ];
        default1=(fun ({bslash_cursor; _} as state) ({cursor; _} as view) t ->
          let mal = invalid_unicode_escape bslash_cursor cursor in
          advance (State_src_directive_path (state |> accum_mals mal)) view t
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
          (":", fun state view t -> advance (State_src_directive_path_colon state) view t);
          ("]", fun {mals; path} view t ->
              let atok = render_source_directive ~mals ~path ~line:None ~indent:None ~omit:None in
              accept_source_directive atok view t
          );
        ];
        default1=(fun state ({pcursor; cursor; _} as view) t ->
          let mal = unexpected_codepoint_source_directive pcursor cursor in
          advance (State_src_directive_rditto (state |> accum_mals mal)) view t
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
          ("_", fun state view t -> advance (State_src_directive_path_colon state) view t);
          (dec_lead_cps, fun {mals; path} ({pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_src_directive_line (State.Src_directive_line.init ~mals ~path
                  ~line_cursor:pcursor ~line:(Some digit))) view t);
          ("]", fun {mals; _} {pcursor; cursor; _} t ->
              let mal = unexpected_codepoint_source_directive pcursor cursor in
              accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
          );
        ];
        default1=(fun state ({pcursor; cursor; _} as view) t ->
          let mal = unexpected_codepoint_source_directive pcursor cursor in
          advance (State_src_directive_rditto (state |> accum_mals mal)) view t
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
                  |> Nat.fmt ~alt:true max_abs_i64
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
          ("_", fun state view t -> advance (State_src_directive_line state) view t);
          (dec_cps, fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_src_directive_line (state |> accum_line digit)) view t
          );
          (":", fun ({path; _} as state) view t ->
              let mals, line = validate_line state view in
              advance (State_src_directive_line_colon (State.Src_directive_line_colon.init ~mals
                  ~path ~line)) view t
          );
          ("]", fun ({path; _} as state) view t ->
              let mals, line = validate_line state view in
              let atok = render_source_directive ~mals ~path ~line ~indent:None ~omit:None in
              accept_source_directive atok view t
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
          ("_", fun state view t -> advance (State_src_directive_line_colon state) view t);
          ("0", fun {mals; path; line} view t ->
              advance (State_src_directive_indent_0 (State.Src_directive_indent_0.init ~mals ~path
                  ~line)) view t);
          (dec_lead_cps, fun {mals; path; line} (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_src_directive_indent (State.Src_directive_indent.init ~mals ~path ~line
                  ~indent_cursor:pcursor ~indent:digit)) view t);
          ("]", fun {mals; _} {pcursor; cursor; _} t ->
              let mal = unexpected_codepoint_source_directive pcursor cursor in
              accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
          );
        ];
        default1=(fun state ({pcursor; cursor; _} as view) t ->
          let mal = unexpected_codepoint_source_directive pcursor cursor in
          advance (State_src_directive_line_colon (state |> accum_mals mal)) view t
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
          ("_", fun state view t -> advance (State_src_directive_indent state) view t);
          (dec_cps, fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_src_directive_indent (state |> accum_indent digit)) view t
          );
          ("+", fun {mals; path; line; indent_cursor; indent} ({pcursor; _} as view) t ->
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
                      |> Nat.fmt ~alt:true max_abs_i64
                      |> Fmt.to_string
                    in
                    let mal = malformation ~base:indent_cursor ~past:pcursor description in
                    (mal :: mals), None
                  end
              in
              advance (State_src_directive_indent_plus (State.Src_directive_indent_plus.init ~mals
                  ~path ~line ~indent)) view t
          );
          ("]", fun {mals; _} {pcursor; cursor; _} t ->
              let mal = unexpected_codepoint_source_directive pcursor cursor in
              accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
          );
        ];
        default1=(fun state ({pcursor; cursor; _} as view) t ->
          let mal = unexpected_codepoint_source_directive pcursor cursor in
          advance (State_src_directive_indent (state |> accum_mals mal)) view t
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
          ("_", fun state view t -> advance (State_src_directive_indent_0 state) view t);
          ("+", fun {mals; path; line} view t ->
              advance (State_src_directive_indent_plus (State.Src_directive_indent_plus.init ~mals
                  ~path ~line ~indent:(Some Nat.k_0))) view t
          );
          ("]", fun {mals; _} {pcursor; cursor; _} t ->
              let mal = unexpected_codepoint_source_directive pcursor cursor in
              accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
          );
        ];
        default1=(fun state ({pcursor; cursor; _} as view) t ->
          let mal = unexpected_codepoint_source_directive pcursor cursor in
          advance (State_src_directive_indent_0 (state |> accum_mals mal)) view t
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
          ("_", fun state view t -> advance (State_src_directive_indent_plus state) view t);
          ("0", fun {mals; path; line; indent} view t ->
              advance (State_src_directive_omit_0 (State.Src_directive_omit_0.init ~mals ~path
                  ~line ~indent)) view t);
          (dec_lead_cps, fun {mals; path; line; indent} (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_src_directive_omit (State.Src_directive_omit.init ~mals ~path ~line
                  ~indent ~omit_cursor:pcursor ~omit:digit)) view t);
          ("]", fun {mals; _} {pcursor; cursor; _} t ->
              let mal = unexpected_codepoint_source_directive pcursor cursor in
              accept (Tok_source_directive (AbstractToken.Rendition.of_mals (mal :: mals))) cursor t
          );
        ];
        default1=(fun state ({pcursor; cursor; _} as view) t ->
          let mal = unexpected_codepoint_source_directive pcursor cursor in
          advance (State_src_directive_indent_plus (state |> accum_mals mal)) view t
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
          ("_", fun state view t -> advance (State_src_directive_omit state) view t);
          (dec_cps, fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_src_directive_omit (state |> accum_omit digit)) view t
          );
          ("]", fun {mals; path; line; indent; omit_cursor; omit} ({pcursor; _} as view) t ->
              let mals, omit = match Nat.(omit > max_abs_i64) with
                | false -> mals, Some omit
                | true -> begin
                    let description =
                      String.Fmt.empty
                      |> Fmt.fmt "Omitted columns exceeds "
                      |> Nat.fmt ~alt:true max_abs_i64
                      |> Fmt.to_string
                    in
                    let mal = malformation ~base:omit_cursor ~past:pcursor description in
                    (mal :: mals), None
                  end
              in
              let atok = render_source_directive ~mals ~path ~line ~indent ~omit in
              accept_source_directive atok view t
          );
        ];
        default1=(fun state ({pcursor; cursor; _} as view) t ->
          let mal = unexpected_codepoint_source_directive pcursor cursor in
          advance (State_src_directive_omit (state |> accum_mals mal)) view t
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
          ("_", fun state view t -> advance (State_src_directive_omit_0 state) view t);
          ("]", fun {mals; path; line; indent} view t ->
              let atok = render_source_directive ~mals ~path ~line ~indent ~omit:(Some Nat.k_0) in
              accept_source_directive atok view t
          );
        ];
        default1=(fun state ({pcursor; cursor; _} as view) t ->
          let mal = unexpected_codepoint_source_directive pcursor cursor in
          advance (State_src_directive_omit_0 (state |> accum_mals mal)) view t
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
      (* Compute level an alignement such that the misaligned cases rounded to the nearest level. *)
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
        ("\n", fun view t ->
            match t.line_state with
            | Line_begin -> accept_incl Tok_whitespace view t
            | Line_whitespace
            | Line_start_col _ -> accept_line_break_incl Tok_whitespace view t
            | Line_body -> not_reached ()
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
        ("*", fun (View.{ppcursor; _} as view) t ->
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
    let accept_rendition rendition cursor t =
      match in_fstring t with
      | true -> accept_fstring_trans Fstring_spec_pad_seen (Tok_fstring_pad rendition) cursor t
      | false -> accept (Tok_codepoint rendition) cursor t

    let accept_rendition_incl rendition View.{cursor; _} t =
      accept_rendition rendition cursor t

    let accept_rendition_pexcl rendition View.{ppcursor; _} t =
      accept_rendition rendition ppcursor t

    let eoi0 View.{cursor; _} t =
      let mal = unterminated_codepoint t.tok_base cursor in
      let rendition = AbstractToken.Rendition.of_mals [mal] in
      accept_rendition rendition cursor t

    let eoi1 _state view t =
      eoi0 view t

    let node0_tick = {
      edges0=map_of_cps_alist [
        ("\\", fun (View.{pcursor=bslash_cursor; _} as view) t ->
            advance (State_codepoint_bslash (State.Codepoint_bslash.init ~bslash_cursor)) view t);
        ("'", fun ({cursor; _} as view) t ->
            let mal = empty_codepoint t.tok_base cursor in
            accept_rendition_incl (AbstractToken.Rendition.of_mals [mal]) view t
        );
        ("\t\n\r", fun ({pcursor; cursor; _} as view) t ->
            let mal = invalid_codepoint pcursor cursor in
            advance (State.State_codepoint_mal (State.Codepoint_mal.init ~mal)) view t
        );
        ("�", fun (View.{pcursor; cursor; _} as view) t ->
            match Source.Cursor.rvalid pcursor with
            | false -> begin
                let mal = invalid_utf8 pcursor cursor in
                advance (State.State_codepoint_mal (State.Codepoint_mal.init ~mal)) view t
              end
            | true -> begin
                let cp = Source.Cursor.rget pcursor in
                advance (State.State_codepoint_raw_cp (State.Codepoint_raw_cp.init ~cp)) view t
              end
        );
      ];
      default0=(fun (View.{pcursor; _} as view) t ->
        let cp = Source.Cursor.rget pcursor in
        advance (State.State_codepoint_raw_cp (State.Codepoint_raw_cp.init ~cp)) view t
      );
      eoi0;
    }

    let node1_bslash =
      let open State.Codepoint_bslash in
      {
        edges1=map_of_cps_alist [
          ("u", fun state view t -> advance (State_codepoint_bslash_u state) view t);
          ("t", fun _state view t ->
              advance (State_codepoint_interpolated_cp (State.Codepoint_interpolated_cp.init
                  ~cp:Codepoint.ht)) view t);
          ("n", fun _state view t ->
              advance (State_codepoint_interpolated_cp (State.Codepoint_interpolated_cp.init
                  ~cp:Codepoint.nl)) view t);
          ("r", fun _state view t ->
              advance (State_codepoint_interpolated_cp (State.Codepoint_interpolated_cp.init
                  ~cp:Codepoint.cr)) view t);
          ("'\\", fun _state ({pcursor; _} as view) t ->
              let cp = Source.Cursor.rget pcursor in
              advance (State_codepoint_interpolated_cp (State.Codepoint_interpolated_cp.init ~cp))
                view t
          );
        ];
        default1=(fun {bslash_cursor} (View.{cursor; _} as view) t ->
          let mal = illegal_backslash bslash_cursor cursor in
          advance (State.State_codepoint_mal (State.Codepoint_mal.init ~mal)) view t
        );
        eoi1;
      }

    let node1_bslash_u = {
      edges1=map_of_cps_alist [
        ("{", fun state view t -> advance (State_codepoint_bslash_u_lcurly state) view t);
      ];
      default1=(fun {bslash_cursor} (View.{cursor; _} as view) t ->
        let mal = invalid_unicode_escape bslash_cursor cursor in
        advance (State.State_codepoint_mal (State.Codepoint_mal.init ~mal)) view t
      );
      eoi1;
    }

    let node1_bslash_u_lcurly = {
      edges1=map_of_cps_alist [
        ("_", fun state view t -> advance (State_codepoint_bslash_u_lcurly state) view t);
        (hex_cps, fun {bslash_cursor} (View.{pcursor; _} as view) t ->
            let digit = nat_of_cp (Source.Cursor.rget pcursor) in
            advance (State_codepoint_bslash_u_lcurly_hex (State.Codepoint_bslash_u_lcurly_hex.init
                ~bslash_cursor ~hex:digit)) view t
        );
      ];
      default1=(fun {bslash_cursor} (View.{cursor; _} as view) t ->
        let mal = invalid_unicode_escape bslash_cursor cursor in
        advance (State.State_codepoint_mal (State.Codepoint_mal.init ~mal)) view t
      );
      eoi1;
    }

    let node1_bslash_u_lcurly_hex =
      let open State.Codepoint_bslash_u_lcurly_hex in
      {
        edges1=map_of_cps_alist [
          ("_", fun state view t -> advance (State_codepoint_bslash_u_lcurly_hex state) view t);
          (hex_cps, fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_codepoint_bslash_u_lcurly_hex (state |> accum_hex digit)) view t
          );
          ("}", fun {bslash_cursor; hex} ({cursor; _} as view) t ->
              advance (
                Option.value_map (Nat.to_uns_opt hex)
                  ~f:(fun u -> Codepoint.narrow_of_uns_opt u)
                  ~default:None
                |> Option.value_map
                  ~f:(fun cp -> State.State_codepoint_interpolated_cp
                      (State.Codepoint_interpolated_cp.init ~cp))
                  ~default:(State.State_codepoint_mal (State.Codepoint_mal.init
                      ~mal:(invalid_unicode_escape bslash_cursor cursor)))
              ) view t
          );
        ];
        default1=(fun {bslash_cursor; _} (View.{cursor; _} as view) t ->
          let mal = invalid_unicode_escape bslash_cursor cursor in
          advance (State.State_codepoint_mal (State.Codepoint_mal.init ~mal)) view t
        );
        eoi1;
      }

    let node1_interpolated_cp =
      let open State.Codepoint_interpolated_cp in
      {
        edges1=map_of_cps_alist [
          ("'", fun {cp} view t -> accept_rendition_incl (Constant cp) view t);
        ];
        default1=(fun _state (View.{pcursor; cursor; _} as view) t ->
          let mal = excess_codepoint pcursor cursor in
          advance (State.State_codepoint_mal (State.Codepoint_mal.init ~mal)) view t
        );
        eoi1;
      }

    let node1_raw_cp =
      let open State.Codepoint_raw_cp in
      {
        edges1=map_of_cps_alist [
          ("'", fun {cp} view t -> accept_rendition_incl (Constant cp) view t);
        ];
        default1=(fun _state (View.{cursor; _} as view) t ->
          match in_fstring t with
          | true -> begin
              let mal = unterminated_codepoint t.tok_base cursor in
              accept_rendition_pexcl (AbstractToken.Rendition.of_mals [mal]) view t
            end
          | false -> accept_pexcl Tok_tick view t);
        eoi1;
      }

    let node1_mal =
      let open State.Codepoint_mal in
      {
        edges1=map_of_cps_alist [
          ("'", fun {mal} view t ->
              accept_rendition_incl (AbstractToken.Rendition.of_mals [mal]) view t);
        ];
        default1=(fun state view t -> advance (State.State_codepoint_mal state) view t);
        eoi1=(fun {mal} (View.{cursor; _} as view) t ->
          let mal2 = unterminated_codepoint t.tok_base cursor in
          accept_rendition_incl (AbstractToken.Rendition.of_mals (mal2 :: [mal])) view t
        );
      }
  end

  module Rstring = struct
    let accept_mals mals cursor t =
      let open AbstractToken in
      let malformed = Rendition.of_mals mals in
      accept (Tok_rstring malformed) cursor t

    let accept_mals_incl mals View.{cursor; _} t =
      accept_mals mals cursor t

    let node1_ltag =
      let open State.Rstring_ltag in
      {
        edges1=map_of_cpsets_alist [
          (cpset_of_cps ident_cps, fun state view t -> advance (State_rstring_ltag state) view t);
          (cpset_of_cps "`", fun {mals; ltag_base} (View.{pcursor; cursor; _} as view) t ->
              let ltag = Source.Slice.of_cursors ~base:ltag_base ~past:pcursor in
              let state' = State.Rstring_body.init ~mals ~ltag ~body_base:cursor in
              advance (State_rstring_body state') view t
          );
        ];
        default1=(fun state (View.{pcursor; cursor; _} as view) t ->
          let mal = invalid_tag pcursor cursor in
          advance (State_rstring_ltag (state |> accum_mal ~mal)) view t
        );
        eoi1=(fun state (View.{cursor; _} as view) t ->
          let mal = unterminated_string t.tok_base cursor in
          let state' = accum_mal ~mal state in
          accept_mals_incl state'.mals view t
        );
      }

    let node1_body =
      let open State.Rstring_body in
      {
        edges1=map_of_cps_alist [
          ("`", fun {mals; ltag; body_base} (View.{pcursor; _} as view) t ->
              let body = Source.Slice.of_cursors ~base:body_base ~past:pcursor in
              let state' = State.Rstring_rtag.init ~mals ~ltag ~body in
              advance (State_rstring_rtag state') view t
          );
          ("�", fun state (View.{pcursor; cursor; _} as view) t ->
              let state' = match Source.Cursor.rvalid pcursor with
                | false -> begin
                    let mal = invalid_utf8 pcursor cursor in
                    state |> accum_mal ~mal
                  end
                | true -> state
              in
              advance (State_rstring_body state') view t
          );
        ];
        default1=(fun state view t -> advance (State_rstring_body state) view t);
        eoi1=(fun state (View.{cursor; _} as view) t ->
          let mal = unterminated_string t.tok_base cursor in
          let state' = accum_mal ~mal state in
          accept_mals_incl state'.mals view t
        );
      }

    let node1_rtag =
      let open State.Rstring_rtag in
      {
        edges1=map_of_cps_alist [
          ("`", fun {mals; ltag; body; ltag_cursor} (View.{pcursor; cursor; _} as view) t ->
              let ltag_past = Source.Slice.past ltag in
              match Source.Cursor.(ltag_cursor = ltag_past) with
              | false -> begin
                  let body' =
                    Source.Slice.of_cursors ~base:(Source.Slice.base body) ~past:pcursor in
                  let state' = State.Rstring_rtag.init ~mals ~ltag ~body:body' in
                  advance (State_rstring_rtag state') view t
                end
              | true -> begin
                  match mals with
                  | _ :: _ -> accept_mals_incl mals view t
                  | [] -> begin
                      let open AbstractToken in
                      accept (Tok_rstring (Constant (Source.Slice.to_string body))) cursor t
                    end
                end
          );
        ];
        default1=(fun ({mals; ltag; body; ltag_cursor} as state) (View.{pcursor; _} as view)
          t ->
          let lcp = Source.Cursor.rget ltag_cursor in
          let rcp = Source.Cursor.rget pcursor in
          match Codepoint.(lcp = rcp) with
          | false -> begin
              let state' =
                State.Rstring_body.init ~mals ~ltag ~body_base:(Source.Slice.base body) in
              advance (State_rstring_body state') view t
            end
          | true -> advance (State_rstring_rtag (State.Rstring_rtag.next state)) view t
        );
        eoi1=(fun state (View.{cursor; _} as view) t ->
          let mal = unterminated_string t.tok_base cursor in
          let state' = accum_mal ~mal state in
          accept_mals_incl state'.mals view t
        );
      }
  end

  (* Istring states are also used for fstrings. Source directives, on the other hand, have their own
   * istring scanning implementation. *)
  module Istring = struct
    let accept_istring accum cursor t =
      accept (State.CodepointAccum.to_atok_istring accum) cursor t

    let accept_istring_incl accum View.{cursor; _} t =
      accept_istring accum cursor t

    let accept_mal accum cursor t =
      let open State.CodepointAccum in
      assert (match accum with Codepoints _ -> false | Malformations _ -> true);
      match in_fstring t with
      | true -> accept (to_atok_fstring_interpolated accum) cursor t
      | false -> accept_istring accum cursor t

    let accept_mal_incl accum View.{cursor; _} t =
      accept_mal accum cursor t

    let accept_eoi accum cursor t =
      let open State.CodepointAccum in
      match in_fstring t with
      | true -> begin
          match accum with
          | Codepoints [] -> retry_fstring_eoi t
          | _ -> accept_fstring_pop (to_atok_fstring_interpolated accum) cursor t
        end
      | false -> begin
          let mal = unterminated_string cursor cursor in
          let accum' = accum |> accum_mal mal in
          accept_istring accum' cursor t
        end

    let accept_eoi_incl accum View.{cursor; _} t =
      accept_eoi accum cursor t

    let node1_start =
      let open State.Istring_body in
      {
        edges1=map_of_cps_alist [
          ("%", fun {accum} (View.{pcursor; cursor; _} as view) t ->
              match in_fstring t with
              | false -> begin
                  let interpolated_base = Source.Cursor.succ t.tok_base in
                  let lditto_ctok = ConcreteToken.ctok_at ~base:t.tok_base ~past:interpolated_base
                      Tok_fstring_lditto in
                  let pct_ctok = ConcreteToken.ctok_at ~base:pcursor ~past:cursor Tok_fstring_pct in
                  let fstring_state = match Source.Cursor.(interpolated_base = pcursor) with
                    | true -> Fstring_spec_pct_seen [pct_ctok]
                    | false -> begin
                        let interpolated_ctok = ConcreteToken.ctok_at ~base:interpolated_base
                            ~past:pcursor (State.CodepointAccum.to_atok_fstring_interpolated accum)
                        in
                        Fstring_spec_pct_seen [interpolated_ctok; pct_ctok]
                      end
                  in
                  accept_fstring_push_ctok_incl fstring_state lditto_ctok view t
                end
              | true -> begin
                  let pct_ctok = ConcreteToken.ctok_at ~base:pcursor ~past:cursor Tok_fstring_pct in
                  let fstring_state, ctok = match Source.Cursor.(t.tok_base = pcursor) with
                    | true -> Fstring_spec_pct_seen [], pct_ctok
                    | false -> begin
                        let interpolated_ctok = ConcreteToken.ctok_at ~base:t.tok_base
                            ~past:pcursor (State.CodepointAccum.to_atok_fstring_interpolated accum)
                        in
                        Fstring_spec_pct_seen [pct_ctok], interpolated_ctok
                      end
                  in
                  accept_fstring_trans_ctok_incl fstring_state ctok view t
                end
          );
          ("\"", fun {accum} (View.{pcursor; cursor; _} as view) t ->
              match in_fstring t with
              | true -> begin
                  let rditto_ctok = ConcreteToken.ctok_at ~base:pcursor ~past:cursor
                      Tok_fstring_rditto in
                  let fstring_states' = List.tl t.fstring_states in
                  match Source.Cursor.(t.tok_base = pcursor) with
                  | true ->
                    {t with tok_base=cursor; fstring_states=fstring_states'}, Accept rditto_ctok
                  | false -> begin
                      let interpolated_ctok = ConcreteToken.ctok_at ~base:t.tok_base
                          ~past:pcursor (State.CodepointAccum.to_atok_fstring_interpolated accum) in
                      accept_fstring_trans_ctok_incl (Fstring_rditto_seen rditto_ctok)
                        interpolated_ctok view t
                    end
                end
              | false -> accept_istring_incl accum view t
          );
          ("\\", fun {accum} ({pcursor; _} as view) t ->
              advance (State_istring_bslash (State.Istring_bslash.init ~accum
                  ~bslash_cursor:pcursor)) view t
          );
          ("\t\r", fun state (View.{pcursor; cursor; _} as view) t ->
              let mal = invalid_codepoint pcursor cursor in
              advance (State_istring_body (state |> accum_mal mal)) view t
          );
        ];
        default1=(fun state (View.{pcursor; _} as view) t ->
          let cp = Source.Cursor.rget pcursor in
          advance (State_istring_body (state |> accum_cp cp)) view t
        );
        eoi1=(fun {accum} view t -> accept_eoi_incl accum view t);
      }

    let node1_bslash =
      let open State.Istring_bslash in
      let accum_interpolated cp state view t = begin
        let state' = state |> accum_cp cp in
        advance (State_istring_body (State.Istring_body.init ~accum:state'.accum)) view t
      end in
      let accum_raw state (View.{pcursor; _} as view) t = begin
        let cp = Source.Cursor.rget pcursor in
        accum_interpolated cp state view t
      end in
      {
        edges1=map_of_cps_alist [
          ("u", fun {accum; bslash_cursor} view t ->
              advance (State_istring_bslash_u (State.Istring_bslash.init ~accum ~bslash_cursor))
                view t
          );
          ("t", accum_interpolated Codepoint.ht);
          ("n", accum_interpolated Codepoint.nl);
          ("r", accum_interpolated Codepoint.cr);
          ("\"\\%", accum_raw);
          ("\n", fun {accum; _} view t ->
              advance (State_istring_body (State.Istring_body.init ~accum)) view t);
        ];
        default1=(fun {accum; bslash_cursor} {cursor; _} t ->
          let mal = illegal_backslash bslash_cursor cursor in
          let accum' = State.CodepointAccum.accum_mal mal accum in
          t, Retry (State_istring_body (State.Istring_body.init ~accum:accum'))
        );
        eoi1=(fun {accum; _} view t -> accept_eoi_incl accum view t);
      }

    let node1_bslash_u =
      let open State.Istring_bslash_u in
      {
        edges1=map_of_cps_alist [
          ("{", fun {accum; bslash_cursor} view t ->
              advance (State_istring_bslash_u_lcurly {accum; bslash_cursor; u=Nat.k_0}) view t
          );
          ("\"", fun {accum; bslash_cursor} ({cursor; _} as view) t ->
              let mal = illegal_backslash bslash_cursor cursor in
              accept_mal_incl (State.CodepointAccum.accum_mal mal accum) view t
          );
        ];
        default1=(fun ({bslash_cursor; _} as state) View.{pcursor; _} t ->
          let mal = illegal_backslash bslash_cursor pcursor in
          let state' = state |> accum_mal mal in
          t, Retry (State_istring_body (State.Istring_body.init ~accum:state'.accum))
        );
        eoi1=(fun {accum; _} view t -> accept_eoi_incl accum view t);
      }

    let node1_bslash_u_lcurly =
      let open State.Istring_bslash_u_lcurly in
      {
        edges1=map_of_cps_alist [
          ("_", fun state view t -> advance (State_istring_bslash_u_lcurly state) view t);
          (hex_cps, fun state ({pcursor; _} as view) t ->
              let cp = Source.Cursor.rget pcursor in
              advance (State_istring_bslash_u_lcurly (state |> accum_hex cp)) view t
          );
          ("}", fun ({bslash_cursor; u; _} as state) ({cursor; _} as view) t ->
              Option.value_map (Nat.to_uns_opt u)
                ~f:(fun u -> Codepoint.narrow_of_uns_opt u)
                ~default:None
              |> Option.value_map
                ~f:(fun cp ->
                  let state' = state |> accum_cp cp in
                  advance (State_istring_body (State.Istring_body.init ~accum:state'.accum)) view t
                )
                ~default:(
                  let mal = invalid_unicode bslash_cursor cursor in
                  let state' = state |> accum_mal mal in
                  t, Retry (State_istring_body (State.Istring_body.init ~accum:state'.accum))
                )
          );
          ("\"", fun ({bslash_cursor; _} as state) ({cursor; _} as view) t ->
              let mal = invalid_unicode_escape bslash_cursor cursor in
              let state' = state |> accum_mal mal in
              accept_mal_incl state'.accum view t
          );
        ];
        default1=(fun {accum; bslash_cursor; _} View.{pcursor; _} t ->
          let mal = invalid_unicode_escape bslash_cursor pcursor in
          t, Retry (State_istring_body {accum=State.CodepointAccum.accum_mal mal accum})
        );
        eoi1=(fun {accum; _} view t -> accept_eoi_incl accum view t);
      }
  end

  module Fstring = struct
    let accept_mal fstring_state description atok_of_mal cursor t =
      let mal = malformation ~base:t.tok_base ~past:cursor description in
      accept_fstring_trans fstring_state (atok_of_mal mal) cursor t

    let accept_mal_incl fstring_state description atok_of_mal View.{cursor; _} t =
      accept_mal fstring_state description atok_of_mal cursor t

    let accept_mal_excl fstring_state description atok_of_mal View.{pcursor; _} t =
      accept_mal fstring_state description atok_of_mal pcursor t

    let eoi0 _view t =
      retry_fstring_eoi t

    let edges0_pad = map_of_cps_alist [
      ("'", advance State_codepoint_tick);
    ]

    let edges0_just = map_of_cps_alist [
      ("<", accept_fstring_trans_incl Fstring_spec_just_seen (Tok_fstring_just Fmt.Left));
      ("^", accept_fstring_trans_incl Fstring_spec_just_seen (Tok_fstring_just Fmt.Center));
      (">", accept_fstring_trans_incl Fstring_spec_just_seen (Tok_fstring_just Fmt.Right));
    ]

    let edges0_sign = map_of_cps_alist [
      ("+", accept_fstring_trans_incl Fstring_spec_sign_seen (Tok_fstring_sign Fmt.Explicit));
      ("_", accept_fstring_trans_incl Fstring_spec_sign_seen (Tok_fstring_sign Fmt.Space));
    ]

    let edges0_alt = map_of_cps_alist [
      ("#", accept_fstring_trans_incl Fstring_spec_alt_seen Tok_fstring_alt);
    ]

    let edges0_zpad = map_of_cps_alist [
      ("0", accept_fstring_trans_incl Fstring_spec_zpad_seen Tok_fstring_zpad);
    ]

    let edges0_width = map_of_cps_alist [
      (dec_lead_cps, fun (View.{pcursor; _} as view) t ->
          let digit = nat_of_cp (Source.Cursor.rget pcursor) in
          advance (State_fstring_width (State.Fstring_width.init ~n:digit)) view t
      );
      ("*", accept_fstring_trans_incl Fstring_spec_width_star_seen Tok_fstring_width_star);
    ]

    let edges0_precision = map_of_cps_alist [
      (".", advance State_fstring_dot);
    ]

    let edges0_radix = map_of_cps_alist [
      ("b", advance State_fstring_b);
      ("o", accept_fstring_trans_incl Fstring_spec_radix_seen (Tok_fstring_radix Radix.Oct));
      ("d", accept_fstring_trans_incl Fstring_spec_radix_seen (Tok_fstring_radix Radix.Dec));
      ("x", accept_fstring_trans_incl Fstring_spec_radix_seen (Tok_fstring_radix Radix.Hex));
    ]

    let edges0_notation = map_of_cps_alist [
      ("m", accept_fstring_trans_incl Fstring_spec_notation_seen (Tok_fstring_notation
            Fmt.Normalized));
      ("a", accept_fstring_trans_incl Fstring_spec_notation_seen (Tok_fstring_notation
            Fmt.RadixPoint));
      ("c", advance State_fstring_c);
    ]

    let edges0_pretty = map_of_cps_alist [
      ("p", accept_fstring_trans_incl Fstring_spec_pretty_seen Tok_fstring_pretty);
    ]

    (* Sans [bc], which require lookahead. *)
    let edges0_fmt = map_of_cps_alist [
      ("u", advance State_fstring_fmt_u);
      ("i", advance State_fstring_fmt_i);
      ("n", accept_fstring_trans_incl Fstring_spec_fmt_seen (Tok_fstring_fmt (Constant Fmt_n)));
      ("z", accept_fstring_trans_incl Fstring_spec_fmt_seen (Tok_fstring_fmt (Constant Fmt_z)));
      ("r", advance State_fstring_fmt_r);
      ("s", accept_fstring_trans_incl Fstring_spec_fmt_seen (Tok_fstring_fmt (Constant Fmt_s)));
      ("f", accept_fstring_trans_incl Fstring_spec_fmt_f_seen (Tok_fstring_fmt (Constant Fmt_f)));
    ]

    (* Sans c, which requires lookahead. *)
    let edges0_fmt_b = Map.insert edges0_fmt
        ~k:(Codepoint.of_char 'b')
        ~v:(accept_fstring_trans_incl Fstring_spec_fmt_seen (Tok_fstring_fmt (Constant Fmt_b)))

    (* No lookahead required. *)
    let edges0_fmt_bc = Map.insert edges0_fmt
        ~k:(Codepoint.of_char 'c')
        ~v:(accept_fstring_trans_incl Fstring_spec_fmt_seen (Tok_fstring_fmt (Constant Fmt_c)))

    let edges0_of_maps maps =
      List.fold maps ~init:(Map.empty (module Codepoint))
        ~f:(fun accum m -> Map.union m accum ~f:(fun _k _v0 _v1 -> not_reached ()))

    let node0_pct_seen_start =
      let fstring_state = Fstring_spec_fmt_seen in
      let description =
        ("Percent must be followed by pad/just/sign/alt/zpad/width/precision/radix/notation/pretty/"
          ^ "fmt parameter")
      in
      let atok_of_mal mal = begin
        AbstractToken.Tok_fstring_fmt (Malformed [mal])
      end in
      {
        edges0=edges0_of_maps [edges0_pad; edges0_just; edges0_sign; edges0_alt; edges0_zpad;
          edges0_width; edges0_precision; edges0_radix; edges0_notation; edges0_pretty; edges0_fmt];
        default0=accept_mal_excl fstring_state description atok_of_mal;
        eoi0;
      }

    let node0_pad_seen_start =
      let fstring_state = Fstring_spec_fmt_seen in
      let description =
        "Pad parameter must be followed by just/sign/alt/zpad/width/precision/radix/notation/"
        ^ "pretty/fmt parameter"
      in
      let atok_of_mal mal = begin
        AbstractToken.Tok_fstring_fmt (Malformed [mal])
      end in
      {
        edges0=edges0_of_maps [edges0_just; edges0_sign; edges0_alt; edges0_zpad; edges0_width;
          edges0_precision; edges0_radix; edges0_notation; edges0_pretty; edges0_fmt];
        default0=accept_mal_excl fstring_state description atok_of_mal;
        eoi0;
      }

    let node0_just_seen_start =
      let fstring_state = Fstring_spec_fmt_seen in
      let description =
        "Justification parameter must be followed by sign/alt/zpad/width/precision/radix/notation/"
        ^ "pretty/fmt parameter"
      in
      let atok_of_mal mal = begin
        AbstractToken.Tok_fstring_fmt (Malformed [mal])
      end in
      {
        edges0=edges0_of_maps [edges0_sign; edges0_alt; edges0_zpad; edges0_width; edges0_precision;
          edges0_radix; edges0_notation; edges0_pretty; edges0_fmt];
        default0=accept_mal_excl fstring_state description atok_of_mal;
        eoi0;
      }

    let node0_sign_seen_start =
      let fstring_state = Fstring_spec_fmt_seen in
      let description =
        "Sign parameter must be followed by alt/zpad/width/precision/radix/notation/pretty/fmt"
        ^ " parameter"
      in
      let atok_of_mal mal = begin
        AbstractToken.Tok_fstring_fmt (Malformed [mal])
      end in
      {
        edges0=edges0_of_maps [edges0_alt; edges0_zpad; edges0_width; edges0_precision;
          edges0_radix; edges0_notation; edges0_pretty; edges0_fmt];
        default0=accept_mal_excl fstring_state description atok_of_mal;
        eoi0;
      }

    let node0_alt_seen_start =
      let fstring_state = Fstring_spec_fmt_seen in
      let description =
        "Alt parameter must be followed by zpad/width/precision/radix/notation/pretty/fmt parameter"
      in
      let atok_of_mal mal = begin
        AbstractToken.Tok_fstring_fmt (Malformed [mal])
      end in
      {
        edges0=edges0_of_maps [edges0_zpad; edges0_width; edges0_precision; edges0_radix;
          edges0_notation; edges0_pretty; edges0_fmt];
        default0=accept_mal_excl fstring_state description atok_of_mal;
        eoi0;
      }

    let node0_zpad_seen_start =
      let fstring_state = Fstring_spec_fmt_seen in
      let description =
        "Zpad parameter must be followed by width/precision/radix/notation/pretty/fmt parameter" in
      let atok_of_mal mal = begin
        AbstractToken.Tok_fstring_fmt (Malformed [mal])
      end in
      {
        edges0=edges0_of_maps [edges0_width; edges0_precision; edges0_radix; edges0_notation;
          edges0_pretty; edges0_fmt];
        default0=accept_mal_excl fstring_state description atok_of_mal;
        eoi0;
      }

    let node1_width =
      let open State.Fstring_width in
      let accept_width {n} cursor t = begin
        let open AbstractToken in
        let atok = match Nat.to_uns_opt n with
          | None -> begin
              let mal = out_of_range_int Radix.Dec Uns.(extend_to_nat max_value) t.tok_base
                cursor in
              Tok_fstring_width (Rendition.of_mals [mal])
            end
          | Some width -> Tok_fstring_width (Constant width)
        in
        accept_fstring_trans Fstring_spec_width_seen atok cursor t
      end in
      let accept_width_incl state View.{cursor; _} t = begin
        accept_width state cursor t
      end in
      let accept_width_excl state View.{pcursor; _} t = begin
        accept_width state pcursor t
      end in
      {
        edges1=map_of_cps_alist [
          (dec_cps, fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_fstring_width (state |> accum_digit digit)) view t
          );
        ];
        default1=accept_width_excl;
        eoi1=accept_width_incl;
      }

    let node0_width_star_seen_start =
      let fstring_state = Fstring_spec_width_seen in
      let description = "Parametric width must be of the form `*(^ ... ^)`" in
      let atok_of_mal mal = begin
        AbstractToken.Tok_fstring_width (Malformed [mal])
      end in
      {
        edges0=map_of_cps_alist [
          ("(", advance State_fstring_lparen);
        ];
        default0=accept_mal_excl fstring_state description atok_of_mal;
        eoi0=accept_mal_incl fstring_state description atok_of_mal;
      }

    let node0_width_seen_start =
      let fstring_state = Fstring_spec_fmt_seen in
      let description =
        "Width parameter must be followed by precision/radix/notation/pretty/fmt parameter" in
      let atok_of_mal mal = begin
        AbstractToken.Tok_fstring_fmt (Malformed [mal])
      end in
      {
        edges0=edges0_of_maps [edges0_precision; edges0_radix; edges0_notation;
          edges0_pretty; edges0_fmt];
        default0=accept_mal_excl fstring_state description atok_of_mal;
        eoi0;
      }

    let node0_dot =
      let fstring_state = Fstring_spec_precision_seen in
      let description = "Dot must be followed by precision mode or precision" in
      let atok_of_mal mal = begin
        AbstractToken.Tok_fstring_precision (Malformed [mal])
      end in
      {
        edges0=map_of_cpsets_alist [
          (cpset_of_cps "=",
            accept_fstring_trans_incl Fstring_spec_pmode_seen (Tok_fstring_pmode Fmt.Fixed));
          (Set.union (cpset_of_cps dec_cps) (cpset_of_cps "*"),
            accept_fstring_trans_excl Fstring_spec_pmode_seen (Tok_fstring_pmode Fmt.Limited));
        ];
        default0=accept_mal_excl fstring_state description atok_of_mal;
        eoi0=accept_mal_incl fstring_state description atok_of_mal;
      }

    let node0_pmode_seen_start =
      let fstring_state = Fstring_spec_precision_seen in
      let description = "Precision mode parameter must be followed by precision parameter" in
      let atok_of_mal mal = begin
        AbstractToken.Tok_fstring_precision (Malformed [mal])
      end in
      {
        edges0=map_of_cps_alist [
          (dec_lead_cps, fun (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_fstring_precision (State.Fstring_precision.init ~n:digit)) view t
          );
          ("*",
            accept_fstring_trans_incl Fstring_spec_precision_star_seen Tok_fstring_precision_star);
        ];
        default0=accept_mal_excl fstring_state description atok_of_mal;
        eoi0;
      }

    let node1_precision =
      let open State.Fstring_precision in
      let accept_precision {n} cursor t = begin
        let open AbstractToken in
        let atok = match Nat.to_uns_opt n with
          | None -> begin
              let mal = out_of_range_int Radix.Dec Uns.(extend_to_nat max_value) t.tok_base
                cursor in
              Tok_fstring_precision (Rendition.of_mals [mal])
            end
          | Some precision -> Tok_fstring_precision (Constant precision)
        in
        accept_fstring_trans Fstring_spec_precision_seen atok cursor t
      end in
      let accept_precision_incl state View.{cursor; _} t = begin
        accept_precision state cursor t
      end in
      let accept_precision_excl state View.{pcursor; _} t = begin
        accept_precision state pcursor t
      end in
      {
        edges1=map_of_cps_alist [
          (dec_cps, fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_fstring_precision (state |> accum_digit digit)) view t
          );
        ];
        default1=accept_precision_excl;
        eoi1=accept_precision_incl;
      }

    let node0_precision_star_seen_start =
      let fstring_state = Fstring_spec_precision_seen in
      let description = "Parametric precision must be of the form `*(^ ... ^)`" in
      let atok_of_mal mal = begin
        AbstractToken.Tok_fstring_precision (Malformed [mal])
      end in
      {
        edges0=map_of_cps_alist [
          ("(", advance State_fstring_lparen);
        ];
        default0=accept_mal_excl fstring_state description atok_of_mal;
        eoi0=accept_mal_incl fstring_state description atok_of_mal;
      }

    let node0_precision_seen_start =
      let fstring_state = Fstring_spec_fmt_seen in
      let description =
        "Precision parameter must be followed by radix/notation/pretty/fmt parameter" in
      let atok_of_mal mal = begin
        AbstractToken.Tok_fstring_fmt (Malformed [mal])
      end in
      {
        edges0=edges0_of_maps [edges0_radix; edges0_notation; edges0_pretty; edges0_fmt];
        default0=accept_mal_excl fstring_state description atok_of_mal;
        eoi0;
      }

    let node0_b = {
      edges0=map_of_cpsets_alist [
        (cpset_of_cps "macpbuinzrcsf",
          accept_fstring_trans_excl Fstring_spec_radix_seen (Tok_fstring_radix Radix.Bin)
        );
      ];
      default0=accept_fstring_trans_excl Fstring_spec_fmt_seen (Tok_fstring_fmt (Constant Fmt_b));
      eoi0=accept_fstring_trans_incl Fstring_spec_fmt_seen (Tok_fstring_fmt (Constant Fmt_b));
    }

    let node0_radix_seen_start =
      let fstring_state = Fstring_spec_fmt_seen in
      let description = "Radix parameter must be followed by notation/pretty/fmt parameter" in
      let atok_of_mal mal = begin
        AbstractToken.Tok_fstring_fmt (Malformed [mal])
      end in
      {
        edges0=edges0_of_maps [edges0_notation; edges0_pretty; edges0_fmt_b];
        default0=accept_mal_excl fstring_state description atok_of_mal;
        eoi0;
      }

    let node0_c = {
      edges0=map_of_cpsets_alist [
        (cpset_of_cps "pbuinzrcsf",
          accept_fstring_trans_excl Fstring_spec_notation_seen (Tok_fstring_notation Fmt.Compact)
        );
      ];
      default0=accept_fstring_trans_excl Fstring_spec_fmt_seen (Tok_fstring_fmt (Constant Fmt_c));
      eoi0=accept_fstring_trans_incl Fstring_spec_fmt_seen (Tok_fstring_fmt (Constant Fmt_c));
    }

    let node0_notation_seen_start =
      let fstring_state = Fstring_spec_fmt_seen in
      let description = "Notation parameter must be followed by pretty/fmt parameter" in
      let atok_of_mal mal = begin
        AbstractToken.Tok_fstring_fmt (Malformed [mal])
      end in
      {
        edges0=edges0_of_maps [edges0_pretty; edges0_fmt_bc];
        default0=accept_mal_excl fstring_state description atok_of_mal;
        eoi0;
      }

    let node0_pretty_seen_start =
      let fstring_state = Fstring_spec_fmt_seen in
      let description = "Pretty parameter must be followed by fmt parameter" in
      let atok_of_mal mal = begin
        AbstractToken.Tok_fstring_fmt (Malformed [mal])
      end in
      {
        edges0=edges0_fmt_bc;
        default0=accept_mal_excl fstring_state description atok_of_mal;
        eoi0;
      }

    let node0_fmt_u = {
      edges0=map_of_cps_alist [
        (dec_lead_cps, fun (View.{ppcursor; pcursor; _} as view) t ->
            let digit = nat_of_cp (Source.Cursor.rget pcursor) in
            advance (State_fstring_fmt_u_bitwidth
                State.Fstring_fmt_u_bitwidth.(init ~fmt_cursor:ppcursor |> accum_bitwidth digit))
              view t
        );
        ("0", fun (View.{ppcursor; _} as view) t ->
            advance (State_fstring_fmt_u_bitwidth
                State.Fstring_fmt_u_bitwidth.(init ~fmt_cursor:ppcursor |> accum_invalid)) view t
        );
      ];
      default0=accept_fstring_trans_excl Fstring_spec_fmt_seen (Tok_fstring_fmt (Constant Fmt_u));
      eoi0=accept_fstring_trans_incl Fstring_spec_fmt_seen (Tok_fstring_fmt (Constant Fmt_u));
    }

    let node1_fmt_u_bitwidth =
      let open State.Fstring_fmt_u_bitwidth in
      let subtype_of_state {fmt_cursor; invalid; bitwidth} View.{pcursor; _} = begin
        let open AbstractToken in
        match invalid, bitwidth with
        | false, bitwidth when Nat.(bitwidth = of_string "8") -> Rendition.Constant Fmt_u8
        | false, bitwidth when Nat.(bitwidth = of_string "16") -> Rendition.Constant Fmt_u16
        | false, bitwidth when Nat.(bitwidth = of_string "32") -> Rendition.Constant Fmt_u32
        | false, bitwidth when Nat.(bitwidth = of_string "64") -> Rendition.Constant Fmt_u64
        | false, bitwidth when Nat.(bitwidth = of_string "128") -> Rendition.Constant Fmt_u128
        | false, bitwidth when Nat.(bitwidth = of_string "256") -> Rendition.Constant Fmt_u256
        | false, bitwidth when Nat.(bitwidth = of_string "512") -> Rendition.Constant Fmt_u512
        | _, _ -> Rendition.of_mals [unsupported_bitwidth fmt_cursor pcursor]
      end in
      {
        edges1=map_of_cps_alist [
          (dec_cps, fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_fstring_fmt_u_bitwidth (state |> accum_bitwidth digit)) view t
          );
        ];
        default1=(fun state view t ->
          let subtype = subtype_of_state state view in
          accept_fstring_trans_excl Fstring_spec_fmt_seen (Tok_fstring_fmt subtype) view t
        );
        eoi1=(fun state view t ->
          let subtype = subtype_of_state state view in
          accept_fstring_trans_excl Fstring_spec_fmt_seen (Tok_fstring_fmt subtype) view t
        );
      }

    let node0_fmt_i = {
      edges0=map_of_cps_alist [
        (dec_lead_cps, fun (View.{ppcursor; pcursor; _} as view) t ->
            let digit = nat_of_cp (Source.Cursor.rget pcursor) in
            advance (State_fstring_fmt_i_bitwidth
                State.Fstring_fmt_i_bitwidth.(init ~fmt_cursor:ppcursor |> accum_bitwidth digit))
              view t
        );
        ("0", fun (View.{ppcursor; _} as view) t ->
            advance (State_fstring_fmt_i_bitwidth
                State.Fstring_fmt_i_bitwidth.(init ~fmt_cursor:ppcursor |> accum_invalid)) view t
        );
      ];
      default0=accept_fstring_trans_excl Fstring_spec_fmt_seen (Tok_fstring_fmt (Constant Fmt_i));
      eoi0=accept_fstring_trans_incl Fstring_spec_fmt_seen (Tok_fstring_fmt (Constant Fmt_i));
    }

    let node1_fmt_i_bitwidth =
      let open State.Fstring_fmt_i_bitwidth in
      let subtype_of_state {fmt_cursor; invalid; bitwidth} View.{pcursor; _} = begin
        let open AbstractToken in
        match invalid, bitwidth with
        | false, bitwidth when Nat.(bitwidth = of_string "8") -> Rendition.Constant Fmt_i8
        | false, bitwidth when Nat.(bitwidth = of_string "16") -> Rendition.Constant Fmt_i16
        | false, bitwidth when Nat.(bitwidth = of_string "32") -> Rendition.Constant Fmt_i32
        | false, bitwidth when Nat.(bitwidth = of_string "64") -> Rendition.Constant Fmt_i64
        | false, bitwidth when Nat.(bitwidth = of_string "128") -> Rendition.Constant Fmt_i128
        | false, bitwidth when Nat.(bitwidth = of_string "256") -> Rendition.Constant Fmt_i256
        | false, bitwidth when Nat.(bitwidth = of_string "512") -> Rendition.Constant Fmt_i512
        | _, _ -> Rendition.of_mals [unsupported_bitwidth fmt_cursor pcursor]
      end in
      {
        edges1=map_of_cps_alist [
          (dec_cps, fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_fstring_fmt_i_bitwidth (state |> accum_bitwidth digit)) view t
          );
        ];
        default1=(fun state view t ->
          let subtype = subtype_of_state state view in
          accept_fstring_trans_excl Fstring_spec_fmt_seen (Tok_fstring_fmt subtype) view t
        );
        eoi1=(fun state view t ->
          let subtype = subtype_of_state state view in
          accept_fstring_trans_excl Fstring_spec_fmt_seen (Tok_fstring_fmt subtype) view t
        );
      }

    let node0_fmt_r = {
      edges0=map_of_cps_alist [
        (dec_lead_cps, fun (View.{ppcursor; pcursor; _} as view) t ->
            let digit = nat_of_cp (Source.Cursor.rget pcursor) in
            advance (State_fstring_fmt_r_bitwidth
                State.Fstring_fmt_r_bitwidth.(init ~fmt_cursor:ppcursor |> accum_bitwidth digit))
              view t
        );
        ("0", fun (View.{ppcursor; _} as view) t ->
            advance (State_fstring_fmt_r_bitwidth
                State.Fstring_fmt_r_bitwidth.(init ~fmt_cursor:ppcursor |> accum_invalid)) view t
        );
      ];
      default0=accept_fstring_trans_excl Fstring_spec_fmt_seen (Tok_fstring_fmt (Constant Fmt_r));
      eoi0=accept_fstring_trans_incl Fstring_spec_fmt_seen (Tok_fstring_fmt (Constant Fmt_r));
    }

    let node1_fmt_r_bitwidth =
      let open State.Fstring_fmt_r_bitwidth in
      let subtype_of_state {fmt_cursor; invalid; bitwidth} View.{pcursor; _} = begin
        let open AbstractToken in
        match invalid, bitwidth with
        | false, bitwidth when Nat.(bitwidth = of_string "32") -> Rendition.Constant Fmt_r32
        | false, bitwidth when Nat.(bitwidth = of_string "64") -> Rendition.Constant Fmt_r64
        | _, _ -> Rendition.of_mals [unsupported_bitwidth fmt_cursor pcursor]
      end in
      {
        edges1=map_of_cps_alist [
          (dec_cps, fun state (View.{pcursor; _} as view) t ->
              let digit = nat_of_cp (Source.Cursor.rget pcursor) in
              advance (State_fstring_fmt_r_bitwidth (state |> accum_bitwidth digit)) view t
          );
        ];
        default1=(fun state view t ->
          let subtype = subtype_of_state state view in
          accept_fstring_trans_excl Fstring_spec_fmt_seen (Tok_fstring_fmt subtype) view t
        );
        eoi1=(fun state view t ->
          let subtype = subtype_of_state state view in
          accept_fstring_trans_excl Fstring_spec_fmt_seen (Tok_fstring_fmt subtype) view t
        );
      }

    let node0_fmt_f_seen_start =
      let fstring_state = Fstring_spec_fmt_seen in
      let description = "`f` fmt parameter must be of the form `f(^ ... ^)`" in
      let atok_of_mal mal = begin
        AbstractToken.Tok_error [mal]
      end in
      {
        edges0=map_of_cps_alist [
          ("(", advance State_fstring_lparen);
        ];
        default0=accept_mal_excl fstring_state description atok_of_mal;
        eoi0=accept_mal_incl fstring_state description atok_of_mal;
      }

    let node0_fmt_seen_start =
      let fstring_state = Fstring_spec_sep_seen in
      let description = "Fmt parameter must be followed by separator/value parameter" in
      let atok_of_mal mal = begin
        AbstractToken.Tok_fstring_sep (Malformed [mal])
      end in
      {
        edges0=map_of_cps_alist [
          (" ", advance State_fstring_sep_space);
          (operator_infix_lead_cps, advance State_fstring_sep_op);
          ("(", advance State_fstring_lparen);
        ];
        default0=accept_mal_incl fstring_state description atok_of_mal;
        eoi0;
      }

    let node0_sep_space =
      let fstring_state = Fstring_spec_sep_seen in
      let description = "Separator parameter must contain an infix operator" in
      let atok_of_mal mal = begin
        AbstractToken.Tok_fstring_sep (Malformed [mal])
      end in
      {
        edges0=map_of_cps_alist [
          (" ", advance State_fstring_sep_space);
          (operator_infix_lead_cps, advance State_fstring_sep_op);
        ];
        default0=accept_mal_excl fstring_state description atok_of_mal;
        eoi0=accept_mal_incl fstring_state description atok_of_mal;
      }

    let node0_sep_op = {
      edges0=map_of_cps_alist [
        (operator_cps, advance State_fstring_sep_op);
        (" ", advance State_fstring_sep_op_space);
      ];
      default0=(fun (View.{pcursor; _} as view) t ->
        let sep = str_of_cursor pcursor t in
        accept_fstring_trans_excl Fstring_spec_sep_seen (Tok_fstring_sep (Constant sep)) view t
      );
      eoi0=(fun (View.{cursor; _} as view) t ->
        let sep = str_of_cursor cursor t in
        accept_fstring_trans_incl Fstring_spec_sep_seen (Tok_fstring_sep (Constant sep)) view t
      );
    }

    let node0_sep_op_space = {
      edges0=map_of_cps_alist [
        (operator_cps, advance State_fstring_sep_op);
        (" ", advance State_fstring_sep_op_space);
      ];
      default0=(fun (View.{pcursor; _} as view) t ->
        let sep = str_of_cursor pcursor t in
        accept_fstring_trans_excl Fstring_spec_sep_seen (Tok_fstring_sep (Constant sep)) view t
      );
      eoi0=(fun (View.{cursor; _} as view) t ->
        let sep = str_of_cursor cursor t in
        accept_fstring_trans_incl Fstring_spec_sep_seen (Tok_fstring_sep (Constant sep)) view t
      );
    }

    let node0_sep_seen_start =
      let fstring_state = Fstring_body in
      let description = "Separator parameter must be followed by value parameter" in
      let atok_of_mal mal = begin
        AbstractToken.Tok_error [mal]
      end in
      {
        edges0=map_of_cps_alist [
          ("(", advance State_fstring_lparen);
        ];
        default0=accept_mal_excl fstring_state description atok_of_mal;
        eoi0;
      }

    let node0_spec_lparen =
      let fstring_state = Fstring_body in
      let description = "Value parameter must be of the form `(^ ... ^)`" in
      let atok_of_mal mal = begin
        AbstractToken.Tok_error [mal]
      end in
      {
        edges0=map_of_cps_alist [
          ("^", fun (View.{cursor; _} as view) t ->
              match t.fstring_states with
              | Fstring_spec_width_star_seen :: _ ->
                accept_fstring_trans_incl Fstring_expr_width Tok_fstring_lparen_caret view t
              | Fstring_spec_precision_star_seen :: _ ->
                accept_fstring_trans_incl Fstring_expr_precision Tok_fstring_lparen_caret view t
              | Fstring_spec_fmt_f_seen :: _ ->
                accept_fstring_trans_incl Fstring_expr_fmt Tok_fstring_lparen_caret view t
              | Fstring_spec_fmt_seen :: _ ->
                accept_fstring_trans_incl (Fstring_expr_value None) Tok_fstring_lparen_caret view t
              | Fstring_spec_sep_seen :: _ ->
                accept_fstring_trans_incl (Fstring_expr_value (Some cursor))
                  Tok_fstring_lparen_caret view t
              | _ :: _
              | [] -> not_reached ()
          );
        ];
        default0=accept_mal_incl fstring_state description atok_of_mal;
        eoi0=accept_mal_incl fstring_state description atok_of_mal;
      }

    let node0_caret = {
      edges0=map_of_cps_alist [
        (")", fun (View.{ppcursor; _} as view) t ->
            match t.fstring_states with
            | [] -> accept_excl Tok_caret view t
            | Fstring_expr_width :: _ ->
              accept_fstring_trans_incl Fstring_spec_width_seen Tok_fstring_caret_rparen view t
            | Fstring_expr_precision :: _ ->
              accept_fstring_trans_incl Fstring_spec_precision_seen Tok_fstring_caret_rparen view t
            | Fstring_expr_fmt :: _ ->
              accept_fstring_trans_incl Fstring_spec_fmt_seen Tok_fstring_caret_rparen view t
            | (Fstring_expr_value (Some label_base)) :: _ -> begin
                let label_source = Source.Slice.of_cursors ~base:label_base ~past:ppcursor in
                let label_string = Source.Slice.to_string label_source in
                let label_ctok = ConcreteToken.init (Tok_fstring_label label_string) label_source in
                accept_fstring_trans_incl (Fstring_value_seen label_ctok) Tok_fstring_caret_rparen
                  view t
              end
            | (Fstring_expr_value None) :: _ ->
              accept_fstring_trans_incl Fstring_body Tok_fstring_caret_rparen view t
            | _ :: _ -> not_reached ()
        );
        (operator_cps, advance State_operator_caret);
      ];
      default0=accept_excl Tok_caret;
      eoi0=accept_incl Tok_caret;
    }
  end

  let transition_of_state trace state view t =
    match state with
    | State.State_start -> act0 trace node0_start view t
    | State_lparen -> act0 trace node0_lparen view t
    | State_lbrack -> act0 trace node0_lbrack view t
    | State_tilde -> act0 trace node0_tilde view t
    | State_qmark -> act0 trace node0_qmark view t
    | State_star -> act0 trace node0_star view t
    | State_caret -> act0 trace Fstring.node0_caret view t
    | State_bar -> act0 trace node0_bar view t
    | State_uscore -> act0 trace node0_uscore view t
    | State_tick -> act0 trace node0_tick view t
    | State_tick_lookahead -> act0 trace node0_tick_lookahead view t
    | State_real_bin_dot v -> act1 trace Real.node1_bin_dot v view t
    | State_real_oct_dot v -> act1 trace Real.node1_oct_dot v view t
    | State_real_hex_dot v -> act1 trace Real.node1_hex_dot v view t
    | State_real_dec_dot v -> act1 trace Real.node1_dec_dot v view t
    | State_real_p v -> act1 trace Real.node1_p v view t
    | State_real_e v -> act1 trace Real.node1_e v view t
    | State_real_p_sign v -> act1 trace Real.node1_p_sign v view t
    | State_real_e_sign v -> act1 trace Real.node1_e_sign v view t
    | State_real_p_exp v -> act1 trace Real.node1_p_exp v view t
    | State_real_e_exp v -> act1 trace Real.node1_e_exp v view t
    | State_real_precise_r v -> act1 trace Real.node1_precise_r v view t
    | State_real_approx_r v -> act1 trace Real.node1_approx_r v view t
    | State_real_precise_r_bitwidth v -> act1 trace Real.node1_precise_r_bitwidth v view t
    | State_real_approx_r_bitwidth v -> act1 trace Real.node1_approx_r_bitwidth v view t
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
    | State_codepoint_interpolated_cp v -> act1 trace Codepoint_.node1_interpolated_cp v view t
    | State_codepoint_raw_cp v -> act1 trace Codepoint_.node1_raw_cp v view t
    | State_codepoint_mal v -> act1 trace Codepoint_.node1_mal v view t
    | State_rstring_ltag v -> act1 trace Rstring.node1_ltag v view t
    | State_rstring_body v -> act1 trace Rstring.node1_body v view t
    | State_rstring_rtag v -> act1 trace Rstring.node1_rtag v view t
    | State_istring_body v -> act1 trace Istring.node1_start v view t
    | State_istring_bslash v -> act1 trace Istring.node1_bslash v view t
    | State_istring_bslash_u v -> act1 trace Istring.node1_bslash_u v view t
    | State_istring_bslash_u_lcurly v -> act1 trace Istring.node1_bslash_u_lcurly v view t
    | State_fstring_pct_seen_start -> act0 trace Fstring.node0_pct_seen_start view t
    | State_fstring_pad_seen_start -> act0 trace Fstring.node0_pad_seen_start view t
    | State_fstring_just_seen_start -> act0 trace Fstring.node0_just_seen_start view t
    | State_fstring_sign_seen_start -> act0 trace Fstring.node0_sign_seen_start view t
    | State_fstring_alt_seen_start -> act0 trace Fstring.node0_alt_seen_start view t
    | State_fstring_zpad_seen_start -> act0 trace Fstring.node0_zpad_seen_start view t
    | State_fstring_width_star_seen_start -> act0 trace Fstring.node0_width_star_seen_start view t
    | State_fstring_width v -> act1 trace Fstring.node1_width v view t
    | State_fstring_width_seen_start -> act0 trace Fstring.node0_width_seen_start view t
    | State_fstring_dot -> act0 trace Fstring.node0_dot view t
    | State_fstring_pmode_seen_start -> act0 trace Fstring.node0_pmode_seen_start view t
    | State_fstring_precision_star_seen_start ->
      act0 trace Fstring.node0_precision_star_seen_start view t
    | State_fstring_precision v -> act1 trace Fstring.node1_precision v view t
    | State_fstring_precision_seen_start -> act0 trace Fstring.node0_precision_seen_start view t
    | State_fstring_b -> act0 trace Fstring.node0_b view t
    | State_fstring_radix_seen_start -> act0 trace Fstring.node0_radix_seen_start view t
    | State_fstring_notation_seen_start -> act0 trace Fstring.node0_notation_seen_start view t
    | State_fstring_pretty_seen_start -> act0 trace Fstring.node0_pretty_seen_start view t
    | State_fstring_c -> act0 trace Fstring.node0_c view t
    | State_fstring_fmt_u -> act0 trace Fstring.node0_fmt_u view t
    | State_fstring_fmt_u_bitwidth v -> act1 trace Fstring.node1_fmt_u_bitwidth v view t
    | State_fstring_fmt_i -> act0 trace Fstring.node0_fmt_i view t
    | State_fstring_fmt_i_bitwidth v -> act1 trace Fstring.node1_fmt_i_bitwidth v view t
    | State_fstring_fmt_r -> act0 trace Fstring.node0_fmt_r view t
    | State_fstring_fmt_r_bitwidth v -> act1 trace Fstring.node1_fmt_r_bitwidth v view t
    | State_fstring_fmt_f_seen_start -> act0 trace Fstring.node0_fmt_f_seen_start view t
    | State_fstring_fmt_seen_start -> act0 trace Fstring.node0_fmt_seen_start view t
    | State_fstring_sep_space -> act0 trace Fstring.node0_sep_space view t
    | State_fstring_sep_op -> act0 trace Fstring.node0_sep_op view t
    | State_fstring_sep_op_space -> act0 trace Fstring.node0_sep_op_space view t
    | State_fstring_sep_seen_start -> act0 trace Fstring.node0_sep_seen_start view t
    | State_fstring_lparen -> act0 trace Fstring.node0_spec_lparen view t

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
    | t', Accept ctok -> begin
        if trace then
          File.Fmt.stdout |> Fmt.fmt " -> Accept (" |> ConcreteToken.pp ctok |> Fmt.fmt "), "
          |> pp t' |> Fmt.fmt "\n" |> ignore;
        t', Accept ctok
      end

  let next ?(trace=false) state t =
    if trace then
      File.Fmt.stdout |> Fmt.fmt "Scan: start " |> State.pp state |> Fmt.fmt ", " |> pp t
      |> Fmt.fmt "\n" |> ignore;
    match transition trace state (view_of_t t) t with
    | _, Advance _ -> not_reached ()
    | _, Retry _ -> not_reached ()
    | t', Accept ctok -> t', ctok
end

let next t =
  let trace = None in
  let _trace = Some true in
  match State.start_of_t t with
  | Some start -> Dfa.next ?trace start t
  | None -> begin
      match List.hd t.fstring_states with
      | Fstring_spec_pct_seen (ctok :: ctoks') ->
        Dfa.fstring_trans (Fstring_spec_pct_seen ctoks') t, ctok
      | Fstring_value_seen ctok -> Dfa.fstring_trans Fstring_body t, ctok
      | Fstring_rditto_seen ctok -> Dfa.fstring_pop t, ctok
      | _ -> not_reached ()
    end
