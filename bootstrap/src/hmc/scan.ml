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
        let line = Uns.of_sint (Sint.((Uns.to_sint (Text.Pos.line text_pos)) +
            t.source.bias)) in
        Text.Pos.init ~line ~col:(Text.Pos.col text_pos)

      let seek_rev offset t =
        match offset > index t with
        | true -> halt "Out of bounds"
        | false ->
          {t with text_cursor=Text.Cursor.seek (Sint.neg (Uns.to_sint offset))
              t.text_cursor}

      let seek_fwd offset t =
        match (index t) + offset > (Text.Cursor.index t.source.past) with
        | true -> halt "Out of bounds"
        | false ->
          {t with text_cursor=Text.Cursor.seek (Uns.to_sint offset)
              t.text_cursor}

      let seek offset t =
        match Sint.(offset < kv 0) with
        | true -> seek_rev (Uns.of_sint (Sint.neg offset)) t
        | false -> seek_fwd (Uns.of_sint offset) t
    end
    include T
    include Cmpable.Make(T)
  end

  module Slice = struct
    include Slice.MakeMono(Cursor)
  end

  let line_context t =
    let rec bol text_cursor = begin
      match Text.Cursor.index text_cursor with
      | 0 -> text_cursor
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

  let pp_loc ppf t =
    Format.fprintf ppf "%s[%a..%a)"
      (match t.path with
        | None -> ""
        | Some path -> Format.asprintf "%s:" path
      )
      Text.Pos.pp (Cursor.(pos (hd t)))
      Text.Pos.pp (Cursor.(pos (tl t)))

  let pp ppf t =
    Format.fprintf ppf "%s"
      (Text.Slice.(to_string (of_cursors ~base:t.base ~past:t.past)))
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

      let pp ppf t =
        Format.fprintf ppf "\"%a: %s\"" Source.pp_loc t.source t.description
    end

    type 'a t =
      | Constant of 'a
      | Malformed of Malformation.t list

    let pp pp_a ppf = function
      | Constant a -> Format.fprintf ppf "Constant %a" pp_a a
      | Malformed malformations -> Format.fprintf ppf "Malformed %a"
          (List.pp Malformation.pp) malformations
  end
  type t =
    (* Keywords. *)
    | Tok_and
    | Tok_also
    | Tok_as
    | Tok_assert
    | Tok_conceal
    | Tok_do
    | Tok_downto
    | Tok_effect
    | Tok_else
    | Tok_expose
    | Tok_external
    | Tok_false
    | Tok_for
    | Tok_fun
    | Tok_function
    | Tok_functor
    | Tok_if
    | Tok_import
    | Tok_in
    | Tok_include
    | Tok_lazy
    | Tok_let
    | Tok_match
    | Tok_module
    | Tok_of
    | Tok_open
    | Tok_or
    | Tok_rec
    | Tok_sig
    | Tok_struct
    | Tok_then
    | Tok_to
    | Tok_true
    | Tok_type
    | Tok_val
    | Tok_when
    | Tok_while
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
    | Tok_lt_op of string
    | Tok_eq_op of string
    | Tok_gt_op of string
    | Tok_bar_op of string

    (* Punctuation. *)
    | Tok_tilde
    | Tok_qmark
    | Tok_gt
    | Tok_comma
    | Tok_dot
    | Tok_semi
    | Tok_colon
    | Tok_cons
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
    | Tok_bslash
    | Tok_tick
    | Tok_caret
    | Tok_amp
    | Tok_earrow
    | Tok_arrow
    | Tok_parrow

    | Tok_indent
    | Tok_line_delim
    | Tok_dedent
    | Tok_whitespace
    | Tok_hash_comment
    | Tok_paren_comment of unit Rendition.t
    | Tok_uident of string
    | Tok_cident of string
    | Tok_codepoint of codepoint Rendition.t
    | Tok_istring of string Rendition.t
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
    | Tok_indent_absent
    | Tok_dedent_absent
    | Tok_misaligned
    | Tok_error

  let to_string t =
    let open Format in
    match t with
    (* Keywords. *)
    | Tok_and -> "<Tok_and>"
    | Tok_also -> "<Tok_also>"
    | Tok_as -> "<Tok_as>"
    | Tok_assert -> "<Tok_assert>"
    | Tok_conceal -> "<Tok_conceal>"
    | Tok_do -> "<Tok_do>"
    | Tok_downto -> "<Tok_downto>"
    | Tok_effect -> "<Tok_effect>"
    | Tok_else -> "<Tok_else>"
    | Tok_expose -> "<Tok_expose>"
    | Tok_external -> "<Tok_external>"
    | Tok_false -> "<Tok_false>"
    | Tok_for -> "<Tok_for>"
    | Tok_fun -> "<Tok_fun>"
    | Tok_function -> "<Tok_function>"
    | Tok_functor -> "<Tok_functor>"
    | Tok_if -> "<Tok_if>"
    | Tok_import -> "<Tok_import>"
    | Tok_in -> "<Tok_in>"
    | Tok_include -> "<Tok_include>"
    | Tok_lazy -> "<Tok_lazy>"
    | Tok_let -> "<Tok_let>"
    | Tok_match -> "<Tok_match>"
    | Tok_module -> "<Tok_module>"
    | Tok_of -> "<Tok_of>"
    | Tok_open -> "<Tok_open>"
    | Tok_or -> "<Tok_or>"
    | Tok_rec -> "<Tok_rec>"
    | Tok_sig -> "<Tok_sig>"
    | Tok_struct -> "<Tok_struct>"
    | Tok_then -> "<Tok_then>"
    | Tok_to -> "<Tok_to>"
    | Tok_true -> "<Tok_true>"
    | Tok_type -> "<Tok_type>"
    | Tok_val -> "<Tok_val>"
    | Tok_when -> "<Tok_when>"
    | Tok_while -> "<Tok_while>"
    | Tok_with -> "<Tok_with>"

    (* Operators. *)
    | Tok_tilde_op op -> asprintf "@[<h><Tok_tilde_op=%a>@]" String.pp op
    | Tok_qmark_op op -> asprintf "@[<h><Tok_qmark_op=%a>@]" String.pp op
    | Tok_star_star_op op ->
      asprintf "@[<h><Tok_star_star_op=%a>@]" String.pp op
    | Tok_star_op op -> asprintf "@[<h><Tok_star_op=%a>@]" String.pp op
    | Tok_slash_op op -> asprintf "@[<h><Tok_slash_op=%a>@]" String.pp op
    | Tok_pct_op op -> asprintf "@[<h><Tok_pct_op=%a>@]" String.pp op
    | Tok_plus_op op -> asprintf "@[<h><Tok_plus_op=%a>@]" String.pp op
    | Tok_minus_op op -> asprintf "@[<h><Tok_minus_op=%a>@]" String.pp op
    | Tok_at_op op -> asprintf "@[<h><Tok_at_op=%a>@]" String.pp op
    | Tok_lt_op op -> asprintf "@[<h><Tok_lt_op=%a>@]" String.pp op
    | Tok_eq_op op -> asprintf "@[<h><Tok_eq_op=%a>@]" String.pp op
    | Tok_gt_op op -> asprintf "@[<h><Tok_gt_op=%a>@]" String.pp op
    | Tok_bar_op op -> asprintf "@[<h><Tok_bar_op=%a>@]" String.pp op

    (* Punctuation. *)
    | Tok_tilde -> "<Tok_tilde>"
    | Tok_qmark -> "<Tok_qmark>"
    | Tok_gt -> "<Tok_gt>"
    | Tok_comma -> "<Tok_comma>"
    | Tok_dot -> "<Tok_dot>"
    | Tok_semi -> "<Tok_semi>"
    | Tok_colon -> "<Tok_colon>"
    | Tok_cons -> "<Tok_cons>"
    | Tok_colon_eq -> "<Tok_colon_eq>"
    | Tok_lparen -> "<Tok_lparen>"
    | Tok_rparen -> "<Tok_rparen>"
    | Tok_lbrack -> "<Tok_lbrack>"
    | Tok_rbrack -> "<Tok_rbrack>"
    | Tok_lcurly -> "<Tok_lcurly>"
    | Tok_rcurly -> "<Tok_rcurly>"
    | Tok_bar -> "<Tok_bar>"
    | Tok_larray -> "<Tok_larray>"
    | Tok_rarray -> "<Tok_rarray>"
    | Tok_bslash -> "<Tok_bslash>"
    | Tok_tick -> "<Tok_tick>"
    | Tok_caret -> "<Tok_caret>"
    | Tok_amp -> "<Tok_amp>"
    | Tok_earrow -> "<Tok_earrow>"
    | Tok_arrow -> "<Tok_arrow>"
    | Tok_parrow -> "<Tok_parrow>"

    | Tok_indent -> "<Tok_indent>"
    | Tok_line_delim -> "<Tok_line_delim>"
    | Tok_dedent -> "<Tok_dedent>"
    | Tok_whitespace -> "<Tok_whitespace>"
    | Tok_hash_comment -> "<Tok_hash_comment>"
    | Tok_paren_comment rendition -> begin
        match rendition with
        | Constant _ -> "<Tok_paren_comment>"
        | Malformed _ ->
          asprintf "@[<h><Tok_paren_comment=%a>@]"
            (Rendition.pp Unit.pp) rendition
      end
    | Tok_uident uident -> asprintf "@[<h><Tok_uident=%a>@]" String.pp uident
    | Tok_cident cident -> asprintf "@[<h><Tok_cident=%a>@]" String.pp cident
    | Tok_codepoint rendition ->
      asprintf "@[<h><Tok_codepoint=%a>@]" (Rendition.pp Codepoint.pp) rendition
    | Tok_istring rendition ->
      asprintf "@[<h><Tok_istring=%a>@]" (Rendition.pp String.pp) rendition
    | Tok_rstring rendition ->
      asprintf "@[<h><Tok_rstring=%a>@]" (Rendition.pp String.pp) rendition
    | Tok_bstring rendition ->
      asprintf "@[<h><Tok_bstring=%a>@]" (Rendition.pp String.pp) rendition
    | Tok_r32 rendition ->
      asprintf "@[<h><Tok_r32=%a>@]" (Rendition.pp Real.pp) rendition
    | Tok_r64 rendition ->
      asprintf "@[<h><Tok_r64=%a>@]" (Rendition.pp Real.pp) rendition
    | Tok_u8 rendition ->
      asprintf "@[<h><Tok_u8=%a>@]" (Rendition.pp U8.pp) rendition
    | Tok_i8 rendition ->
      asprintf "@[<h><Tok_i8=%a>@]" (Rendition.pp I8.pp) rendition
    | Tok_u16 rendition ->
      asprintf "@[<h><Tok_u16=%a>@]" (Rendition.pp U16.pp) rendition
    | Tok_i16 rendition ->
      asprintf "@[<h><Tok_i16=%a>@]" (Rendition.pp I16.pp) rendition
    | Tok_u32 rendition ->
      asprintf "@[<h><Tok_u32=%a>@]" (Rendition.pp U32.pp) rendition
    | Tok_i32 rendition ->
      asprintf "@[<h><Tok_i32=%a>@]" (Rendition.pp I32.pp) rendition
    | Tok_u64 rendition ->
      asprintf "@[<h><Tok_u64=%a>@]" (Rendition.pp U64.pp) rendition
    | Tok_i64 rendition ->
      asprintf "@[<h><Tok_i64=%a>@]" (Rendition.pp I64.pp) rendition
    | Tok_u128 rendition ->
      asprintf "@[<h><Tok_u128=%a>@]" (Rendition.pp U128.pp) rendition
    | Tok_i128 rendition ->
      asprintf "@[<h><Tok_i128=%a>@]" (Rendition.pp I128.pp) rendition
    | Tok_u256 rendition ->
      asprintf "@[<h><Tok_u256=%a>@]" (Rendition.pp U256.pp) rendition
    | Tok_i256 rendition ->
      asprintf "@[<h><Tok_i256=%a>@]" (Rendition.pp I256.pp) rendition
    | Tok_u512 rendition ->
      asprintf "@[<h><Tok_u512=%a>@]" (Rendition.pp U512.pp) rendition
    | Tok_i512 rendition ->
      asprintf "@[<h><Tok_i512=%a>@]" (Rendition.pp I512.pp) rendition
    | Tok_end_of_input -> "<Tok_end_of_input>"
    | Tok_indent_absent -> "<Tok_indent_absent>"
    | Tok_dedent_absent -> "<Tok_dedent_absent>"
    | Tok_misaligned -> "<Tok_misaligned>"
    | Tok_error -> "<Tok_error>"

  let keyword_map = Map.of_alist (module String) [
    ("and", Tok_and);
    ("also", Tok_also);
    ("as", Tok_as);
    ("assert", Tok_assert);
    ("conceal", Tok_conceal);
    ("do", Tok_do);
    ("downto", Tok_downto);
    ("effect", Tok_effect);
    ("else", Tok_else);
    ("expose", Tok_expose);
    ("external", Tok_external);
    ("false", Tok_false);
    ("for", Tok_for);
    ("fun", Tok_fun);
    ("function", Tok_function);
    ("functor", Tok_functor);
    ("if", Tok_if);
    ("import", Tok_import);
    ("in", Tok_in);
    ("include", Tok_include);
    ("lazy", Tok_lazy);
    ("let", Tok_let);
    ("match", Tok_match);
    ("module", Tok_module);
    ("of", Tok_of);
    ("open", Tok_open);
    ("or", Tok_or);
    ("rec", Tok_rec);
    ("sig", Tok_sig);
    ("struct", Tok_struct);
    ("then", Tok_then);
    ("to", Tok_to);
    ("true", Tok_true);
    ("type", Tok_type);
    ("val", Tok_val);
    ("when", Tok_when);
    ("while", Tok_while);
    ("with", Tok_with);
  ]

  let of_uident_str uident_str =
    match Map.get uident_str keyword_map with
    | Some t -> t
    | None -> Tok_uident uident_str
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
end

type line_state =
  | LineDentation
  | LineDelim
  | LineBody

type t = {
  path: string option;
  bias: sint;
  cursor: Text.Cursor.t;
  line_state: line_state;
  level: uns;
}

let init text =
  {
    path=Text.path text;
    bias=Sint.zero;
    cursor=Text.Cursor.hd text;
    line_state=LineDentation;
    level=0;
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
  let slice = Text.Slice.of_cursors ~base:t.cursor ~past:cursor in
  Text.Slice.to_string slice

let accept atoken cursor t =
  let source = Source.init t.path t.bias t.cursor cursor in
  {t with cursor}, (ConcreteToken.init atoken source)

let accept_incl atoken _pcursor cursor t =
  accept atoken cursor t

let accept_excl atoken pcursor _cursor t =
  accept atoken pcursor t

let accept_delim atoken cursor t =
  let source = Source.init t.path t.bias t.cursor cursor in
  {t with cursor; line_state=LineDelim}, (ConcreteToken.init atoken source)

let accept_delim_incl atoken _pcursor cursor t =
  accept_delim atoken cursor t

(*******************************************************************************
 * Convenience routines for reporting malformations. *)

let malformation ~base ~past description t =
  AbstractToken.Rendition.Malformation.init
    t.path t.bias ~base ~past description

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
  let description = Format.asprintf "@[<h>Numerical constant exceeds %a@]" (
    let open Radix in
    let open Nat in
    match radix with
    | Bin -> pp_b
    | Oct -> pp_o
    | Dec -> pp
    | Hex -> pp_x
  ) limit in
  malformation ~base ~past description t

let out_of_range_real base past t =
  malformation ~base ~past "Numerical constant cannot be precisely represented"
    t

(******************************************************************************)

let accept_dentation atoken cursor t =
  accept atoken cursor {t with line_state=LineBody}

let whitespace _pcursor cursor t =
  let rec fn cursor t = begin
    match Text.Cursor.next_opt cursor with
    | None -> accept Tok_whitespace cursor t
    | Some (cp, cursor') -> begin
        match cp with
        | cp when Codepoint.(cp = of_char ' ') -> fn cursor' t
        | cp when Codepoint.(cp = of_char '\\') -> fn_bslash cursor cursor' t
        | cp when Codepoint.(cp = nl) -> accept_delim Tok_whitespace cursor' t
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

let hash_comment _pcursor cursor t =
  let accept_hash_comment cursor t = begin
    accept_delim Tok_hash_comment cursor t
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

let paren_comment _pcursor cursor t =
  let accept_paren_comment cursor t = begin
    let open AbstractToken.Rendition in
    accept (Tok_paren_comment (Constant ())) cursor t
  end in

  let rec fn_wrapper ~f nesting cursor t = begin
    match Text.Cursor.next_opt cursor with
    | None -> begin
        let open AbstractToken.Rendition in
        accept (Tok_paren_comment (Malformed [unterminated_comment t.cursor
            cursor t])) cursor t
      end
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
          | 1 -> accept_paren_comment cursor t
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
  fn 1 cursor t

let operator_cps = "-+*/%@!$<=>|:.~?"
let operator_set = set_of_cps operator_cps

let operator_map = (
  let open AbstractToken in
  Map.of_alist (module String) [
    ("|", Tok_bar);
    ("~>", Tok_earrow);
    ("->", Tok_arrow);
    (">", Tok_gt);
    (">->", Tok_parrow);
  ])

let operator fop _pcursor cursor t =
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

let uident _pcursor cursor t =
  ident ~f_accept:(fun cursor t ->
    let uident_str = str_of_cursor cursor t in
    accept (AbstractToken.of_uident_str uident_str) cursor t
  ) cursor t

let cident _pcursor cursor t =
  ident ~f_accept:(fun cursor t ->
    let cident_str = str_of_cursor cursor t in
    accept (Tok_cident cident_str) cursor t
  ) cursor t

let accum_cp_of_nat ~accum_cp ~accum_mal nat accum base past t =
  let nat_to_cp_opt nat = begin
    match Nat.to_uns_opt nat with
    | None -> None
    | Some u -> Codepoint.of_uns_opt u
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
  val codepoint: Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
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
  let codepoint _pcursor cursor t =
    let accept_codepoint accum cursor t = begin
      let open AbstractToken.Rendition in
      match accum with
      | Empty -> not_reached ()
      | Cp cp -> accept (Tok_codepoint (Constant cp)) cursor t
      | Malformations mals ->
        accept (Tok_codepoint (Malformed (List.rev mals))) cursor t
    end in

    (* The callers of fn_wrapper have varying scanner state they're carrying as
     * call parameters, so in most cases they have to allocate a closure. This
     * isn't ideal performance-wise, but it reduces boilerplate. *)
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
      | Some (cp, cursor') when Codepoint.(cp = of_char '\'') ->
        accept_codepoint accum cursor' t
      | Some (_, _)
      | None -> accept Tok_tick pcursor t
    end in
    let rec fn_bslash_u_lcurly nat bslash_cursor accum cursor t = begin
      fn_wrapper ~f:(fun cp cursor' accum cursor t ->
        match Map.get cp u_map with
        | Some UMapUscore ->
          fn_bslash_u_lcurly nat bslash_cursor accum cursor' t
        | Some UMapDigit -> begin
            fn_bslash_u_lcurly Radix.(nat_accum (nat_of_cp cp) nat Hex)
              bslash_cursor accum cursor' t
          end
        | Some UMapRcurly -> begin
            let accum' = accum_cp_of_nat ~accum_cp ~accum_mal nat accum
                bslash_cursor cursor' t in
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
  val istring: Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
  val bstring: Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
  val rstring: Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
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

  (* Interpolated string: "..." *)
  let istring _pcursor cursor t =
    let accept_istring accum cursor t = begin
      match accum with
      | Codepoints cps ->
        accept (Tok_istring (Constant (String.of_list_rev cps))) cursor t
      | Malformations mals ->
        accept (Tok_istring (Malformed (List.rev mals))) cursor t
    end in

    let rec fn_wrapper ~f accum cursor t = begin
      match Text.Cursor.nextv_opt cursor with
      | None -> begin
          let mal = unterminated_string t.cursor cursor t in
          accept_istring (accum_mal mal accum) cursor t
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
        | Some UMapUscore ->
          fn_bslash_u_lcurly nat bslash_cursor accum cursor' t
        | Some UMapDigit -> begin
            fn_bslash_u_lcurly Radix.(nat_accum (nat_of_cp cp) nat Hex)
              bslash_cursor accum cursor' t
          end
        | Some UMapRcurly -> begin
            fn (accum_cp_of_nat ~accum_cp ~accum_mal nat accum bslash_cursor
                cursor t) cursor' t
          end
        | Some UMapDitto -> begin
            let mal = partial_unicode bslash_cursor cursor t in
            accept_istring (accum_mal mal accum) cursor' t
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
            accept_istring (accum_mal mal accum) cursor' t
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
        | cp when Codepoint.(cp = of_char 'u') ->
          fn_bslash_u bslash_cursor accum cursor' t
        | cp when Codepoint.(cp = of_char 't') ->
          fn (accum_cp Codepoint.ht accum) cursor' t
        | cp when Codepoint.(cp = of_char 'n') ->
          fn (accum_cp Codepoint.nl accum) cursor' t
        | cp when Codepoint.(cp = of_char 'r') ->
          fn (accum_cp Codepoint.cr accum) cursor' t
        | cp when Codepoint.(cp = of_char '"') ->
          fn (accum_cp cp accum) cursor' t
        | cp when Codepoint.(cp = of_char '\\') ->
          fn (accum_cp cp accum) cursor' t
        | _ -> begin
            let mal = illegal_backslash bslash_cursor cursor' t in
            fn (accum_mal mal accum) cursor' t
          end
      )
    end
    and fn accum cursor t = begin
      fn_wrapper accum cursor t ~f:(fun accum cursor cp cursor' t ->
        match cp with
        | cp when Codepoint.(cp = of_char '"') -> accept_istring accum cursor' t
        | cp when Codepoint.(cp = of_char '\\') ->
          fn_bslash cursor accum cursor' t
        | _ -> fn (accum_cp cp accum) cursor' t
      )
    end in
    fn (Codepoints []) cursor t

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
    accept (Tok_rstring (Malformed [unterminated_string t.cursor cursor t]))
      cursor t

  (* Raw string: ``...`` *)
  let rstring pcursor _cursor t =
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
          let mals = List.(rev_concat ltag.mals (rev_concat body_mals
              (rev rtag.mals))) in
          accept (Tok_rstring (Malformed mals)) cursor t
        end
    end in

    let rec fn_rtag rtag_accum ltag_cursor body_accum saved_body_accum ltag
        cursor t = begin
      match Text.Cursor.nextv_opt cursor with
      | None -> begin
          let mal = unterminated_string t.cursor cursor t in
          let rtag = tag_of_accum (tag_accum_mal mal rtag_accum) in
          accept_rstring rtag saved_body_accum ltag cursor t
        end
      | Some (_, false, cursor') -> begin
          let mal = invalid_utf8 cursor cursor' t in
          match String.Cursor.(ltag_cursor = (tl (container ltag_cursor))) with
          | true -> fn (accum_mal mal body_accum) ltag cursor' t
          | false -> begin
              let ltag_cp, ltag_cursor' = String.Cursor.next ltag_cursor in
              match Codepoint.(replacement = ltag_cp) with
              | true ->
                fn_rtag (tag_accum_cp Codepoint.replacement rtag_accum)
                  ltag_cursor' (accum_mal mal body_accum)
                  saved_body_accum ltag cursor' t
              | false -> fn (accum_mal mal body_accum) ltag cursor' t
            end
        end
      | Some (cp, true, cursor') -> begin
          match cp, String.Cursor.(ltag_cursor = (tl (container ltag_cursor)))
          with
          | cp, true when Codepoint.(cp = of_char '`') ->
            accept_rstring (tag_of_accum rtag_accum) saved_body_accum ltag
              cursor' t
          | cp, false when Codepoint.(cp = of_char '`') ->
            fn_rtag tag_accum_empty (String.Cursor.hd ltag.tag)
              (accum_cp cp body_accum) body_accum ltag cursor' t
          | _, true -> fn (accum_cp cp body_accum) ltag cursor' t
          | _, false -> begin
              let ltag_cp, ltag_cursor' = String.Cursor.next ltag_cursor in
              match Codepoint.(cp = ltag_cp) with
              | true ->
                fn_rtag (tag_accum_cp cp rtag_accum) ltag_cursor'
                  (accum_cp cp body_accum) saved_body_accum ltag cursor' t
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
            fn_rtag tag_accum_empty (String.Cursor.hd ltag.tag)
              (accum_cp cp body_accum) body_accum ltag cursor' t
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
  let bstring _pcursor cursor t =
    let accept_bstring accum cursor t = begin
      match accum with
      | Codepoints (_ :: cps) ->
        accept (Tok_bstring (Constant (String.of_list_rev cps))) cursor t
      | Codepoints [] -> not_reached () (* There's always a '\n'. *)
      | Malformations mals ->
        accept (Tok_bstring (Malformed (List.rev mals))) cursor t
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
          | cp when Codepoint.(cp = of_char ' ') ->
            fn_lpad c0_cursor accum lmargin cursor' t
          | cp when Codepoint.(cp = of_char '|') -> begin
              match Text.(Pos.col (Cursor.pos cursor')) = lmargin with
              | true -> fn accum lmargin cursor' t
              | false -> begin
                  let mal = invalid_bar_indent c0_cursor cursor' t in
                  fn (accum_mal mal accum) lmargin cursor' t
                end
            end
          | cp when Codepoint.(cp = of_char '`') ->
            accept_bstring accum cursor' t
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
          | cp when Codepoint.(cp = nl) ->
            fn_lpad cursor' accum' lmargin cursor' t
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
  val of_whole: Nat.t -> Radix.t -> t
  val of_mals: AbstractToken.Rendition.Malformation.t list -> t
  val r_suffix: t -> Text.Cursor.t -> Radix.t -> Text.Cursor.t -> outer
    -> outer * ConcreteToken.t
  val exp: t -> Text.Cursor.t -> Radix.t -> Text.Cursor.t -> outer
    -> outer * ConcreteToken.t
  val dot: t -> Text.Cursor.t -> Radix.t -> Text.Cursor.t -> outer
    -> outer * ConcreteToken.t
  val zero_r_suffix: Text.Cursor.t -> Text.Cursor.t -> outer
    -> outer * ConcreteToken.t
  val zero_frac: Text.Cursor.t -> Text.Cursor.t -> outer
    -> outer * ConcreteToken.t
  val zero_exp: Text.Cursor.t -> Text.Cursor.t -> outer
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
    (* exp is always non-negative; exp_sign tracks the exponent's sign because
     * the sign is scanned before there is a non-zero magnitude in which to
     * incorporate the sign. *)
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
    | R (frac, point_shift, _exp_sign, exp) ->
      R (frac, point_shift, exp_sign, exp)
    | R_dec -> R_dec
    | Malformations _ as mals -> mals

  let realer_of_nat nat =
    let sig_bits = (Nat.bit_length nat) - (Nat.bit_clz nat) in
    let exponent = match sig_bits with
      | 0 -> Zint.zero
      | _ -> Zint.of_uns (pred sig_bits)
    in
    Realer.create ~sign:Realer.Pos ~exponent ~mantissa:nat

  let accum_frac_digit cp radix = function
    | R (frac, point_shift, exp_sign, exp) -> begin
        let cp_nat = nat_of_cp cp in
        let open Radix in
        let bits_per_digit = match radix with
          | Bin -> Sint.kv 1
          | Oct -> Sint.kv 3
          | Dec -> not_reached ()
          | Hex -> Sint.kv 4
        in
        let sig_bits = (Nat.bit_length cp_nat) - (Nat.bit_clz cp_nat) in
        let nonfrac_shift =
          Sint.(bits_per_digit - (pred (Uns.to_sint sig_bits))) in
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
                | None ->
                  (Rendition.Malformed [out_of_range_real t.cursor cursor t])
              )
            end
          | Subtype_r64 -> begin
              Tok_r64 (
                match Realer.to_r64_opt realer with
                | Some r -> (Constant r)
                | None ->
                  (Rendition.Malformed [out_of_range_real t.cursor cursor t])
              )
            end
        in
        accept tok cursor t
      end
    | R _, Dec -> not_reached ()
    | R_dec, Dec -> begin
        let r = Real.of_string Text.Slice.(to_string (of_cursors ~base:t.cursor
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
            | 32 -> Some Subtype_r32
            | 64 -> Some Subtype_r64
            | 0 -> begin
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
    ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnoqstuvwxyz23456789'",
      FMapMalIdent);
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
        | Some FMapUscore ->
          next_frac accum mantissa_cursor frac_map radix cursor' t
        | Some FMapDigit -> begin
            let accum' = accum_frac_digit cp radix accum in
            next_frac accum' mantissa_cursor frac_map radix cursor' t
          end
        | Some FMapExp -> first_exp accum cursor radix cursor' t
        | Some FMapRealSuffix ->
          next_suffix Nat.k_0 cursor accum radix cursor' t
        | Some FMapMalIdent -> begin
            let mal = invalid_numerical cursor cursor' t in
            let accum' = accum_mal mal accum in
            next_frac accum' mantissa_cursor frac_map radix cursor' t
          end
        | None -> accept None cursor accum radix cursor t
      end

  (* Entry point functions follow. *)

  let of_whole whole radix =
    let open Radix in
    match radix with
    | Bin | Oct | Hex -> R (realer_of_nat whole, Sint.kv 0, ExpPos, Zint.zero)
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

  let zero_r_suffix pcursor cursor t =
    r_suffix R_dec pcursor Dec cursor t

  let zero_frac _pcursor cursor t =
    next_frac R_dec t.cursor dec_frac_map Dec cursor t

  let zero_exp pcursor cursor t =
    first_exp R_dec pcursor Dec cursor t
end

module Integer : sig
  val zero: AbstractToken.t
  val bin: Nat.t -> Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
  val oct: Nat.t -> Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
  val dec: Nat.t -> Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
  val hex: Nat.t -> Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
  val mal_ident: Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
  val zero_u_suffix: Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
  val zero_i_suffix: Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
end = struct
  type signedness =
    | Unsigned
    | Signed

  (* Full enumeration of integer types is a bit unwieldy, but it's more robust
   * and doesn't actually cause much code bloat. *)
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

  (* Prefix signs are scanned as separate tokens, and there is no way to
   * distinguish them from infix operators until parsing is complete. Therefore
   * the scanner accepts min_value (e.g. 0x80i8) regardless of whether it's
   * negative. Thanks to 2s complement representation, both 0x80i8 and -0x80i8
   * encode min_value, and no correctness issues arise from allowing 0x80i8.
   *
   * It would be possible to disallow this edge case during an AST optimization
   * pass which combines constants and their prefix signs; if the compiler does
   * so, it should take care to emit error messages formatted the same as those
   * emitted by the scanner. Although it would be possible to push the limit
   * checking into the post-parsing optimization, doing so would make the
   * optimization mandatory, as well as making it more likely for the programmer
   * to not see such errors unless there are no parse errors which prevent code
   * generation. *)
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
            | Unsigned, 8 -> Some Subtype_u8
            | Signed, 8 -> Some Subtype_i8
            | Unsigned, 16 -> Some Subtype_u16
            | Signed, 16 -> Some Subtype_i16
            | Unsigned, 32 -> Some Subtype_u32
            | Signed, 32 -> Some Subtype_i32
            | Unsigned, 64 -> Some Subtype_u64
            | Signed, 64 -> Some Subtype_i64
            | Unsigned, 128 -> Some Subtype_u128
            | Signed, 128 -> Some Subtype_i128
            | Unsigned, 256 -> Some Subtype_u256
            | Signed, 256 -> Some Subtype_i256
            | Unsigned, 512 -> Some Subtype_u512
            | Signed, 512 -> Some Subtype_i512
            | Unsigned, 0 -> begin
                let _cp, digits_cursor = Text.Cursor.next suffix_cursor in
                match Text.Cursor.(digits_cursor = cursor) with
                | true -> Some Subtype_u64 (* "u" suffix. *)
                | false -> None
              end
            | Signed, 0 -> begin
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
    | None ->
      accept_subtype bitwidth signedness suffix_cursor accum radix cursor t
    | Some (cp, cursor') -> begin
        match Map.get cp num_suffix_map with
        | Some NSMapDigit -> begin
            let digit = nat_of_cp cp in
            match Nat.(bitwidth = zero && digit = zero) with
            | true -> begin
                let mal = invalid_type_suffix_leading_zero cursor cursor' t in
                let accum' = accum_mal mal accum in
                next_suffix bitwidth signedness suffix_cursor accum' radix
                  cursor' t
              end
            | false -> begin
                let bitwidth' = Radix.(nat_accum digit bitwidth Dec) in
                next_suffix bitwidth' signedness suffix_cursor accum radix
                  cursor' t
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

  let rec next_whole accum whole_cursor whole_map radix cursor t =
    match Text.Cursor.next_opt cursor with
    | None -> accept None accum radix cursor t
    | Some (cp, cursor') -> begin
        match Map.get cp whole_map with
        | Some WMapUscore ->
          next_whole accum whole_cursor whole_map radix cursor' t
        | Some WMapDigit -> begin
            let accum' = accum_whole_digit cp radix accum in
            next_whole accum' whole_cursor whole_map radix cursor' t
          end
        | Some WMapDot ->
          Real.dot (real_accum_of_accum radix accum) whole_cursor radix cursor'
            t
        | Some WMapExp ->
          Real.exp (real_accum_of_accum radix accum) cursor radix cursor' t
        | Some WMapUnsSuffix ->
          next_suffix Nat.k_0 Unsigned cursor accum radix cursor' t
        | Some WMapIntSuffix ->
          next_suffix Nat.k_0 Signed cursor accum radix cursor' t
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

  let bin n _pcursor cursor t =
    let accum = N n in
    next_whole accum cursor bin_whole_map Bin cursor t

  let oct n _pcursor cursor t =
    let accum = N n in
    next_whole accum cursor oct_whole_map Oct cursor t

  let dec n _pcursor cursor t =
    let accum = N n in
    next_whole accum t.cursor dec_whole_map Dec cursor t

  let hex n _pcursor cursor t =
    let accum = N n in
    next_whole accum cursor hex_whole_map Hex cursor t

  let mal_ident pcursor cursor t =
    let mal = invalid_numerical pcursor cursor t in
    let accum = Malformations [mal] in
    next_whole accum t.cursor dec_whole_map Dec cursor t

  let zero_u_suffix pcursor cursor t =
    let accum = N Nat.k_0 in
    next_suffix Nat.k_0 Unsigned pcursor accum Dec cursor t

  let zero_i_suffix pcursor cursor t =
    let accum = N Nat.k_0 in
    next_suffix Nat.k_0 Signed pcursor accum Dec cursor t
end

let end_of_input cursor t =
  match t.line_state, t.level with
  | LineDelim, 0 -> accept_dentation Tok_line_delim cursor t
  | _, 0 -> accept Tok_end_of_input cursor t
  | _ -> accept_dentation Tok_dedent cursor {t with level=Uns.pred t.level}

module Dag = struct
  (* The scanner's directed acyclic subgraph is expressed as a DAG of states
   * with a unified state transition driver. *)
  type action = Text.Cursor.t -> Text.Cursor.t -> t -> t * ConcreteToken.t
  type eoi_action = Text.Cursor.t -> t -> t * ConcreteToken.t
  type state = {
    edges: (codepoint, action, Codepoint.cmper_witness) Map.t;
    eoi: eoi_action;
    default: action;
  }

  let act state _pcursor cursor t =
    match Text.Cursor.next_opt cursor with
    | None -> state.eoi cursor t
    | Some (cp, cursor') -> begin
        match Map.get cp state.edges with
        | Some action' -> action' cursor cursor' t
        | None -> state.default cursor cursor' t
      end

  let start_state = {
    edges=(map_of_cps_alist [
      (",", (accept_incl Tok_comma));
      (".", (accept_incl Tok_dot));
      (":", (act {
          edges=(map_of_cps_alist [
            (":", (accept_incl Tok_cons));
            ("=", (accept_incl Tok_colon_eq));
          ]);
          eoi=(accept Tok_colon);
          default=(accept_excl Tok_colon);
        }));
      (";", (accept_incl Tok_semi));
      ("(", (act {
          edges=Map.singleton (module Codepoint)
            ~k:(Codepoint.of_char '*') ~v:paren_comment;
          eoi=(accept Tok_lparen);
          default=(accept_excl Tok_lparen);
        }));
      (")", (accept_incl Tok_rparen));
      ("[", (act {
          edges=Map.singleton (module Codepoint)
            ~k:(Codepoint.of_char '|') ~v:(act {
              edges=Map.empty (module Codepoint);
              eoi=(accept Tok_larray);
              default=(accept_excl Tok_larray);
            });
          eoi=(accept Tok_lbrack);
          default=(accept_excl Tok_lbrack);
        }));
      ("]", (act {
          edges=Map.empty (module Codepoint);
          eoi=(accept Tok_rbrack);
          default=(accept_excl Tok_rbrack);
        }));
      ("{", (accept_incl Tok_lcurly));
      ("}", (accept_incl Tok_rcurly));
      ("\\", (act {
          edges=(map_of_cps_alist [
            ("\n", whitespace);
          ]);
          eoi=(accept Tok_bslash);
          default=(accept_excl Tok_bslash);
        }));
      ("^", (accept_incl Tok_caret));
      ("&", (accept_incl Tok_amp));
      ("\n", (accept_delim_incl Tok_whitespace));
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
            ("-+/%@!$<=>|:.~?", (operator (fun s -> Tok_star_op s)));
          ]);
          eoi=(accept (Tok_star_op "*"));
          default=(accept_excl (Tok_star_op "*"));
        }));
      ("/", (operator (fun s -> Tok_slash_op s)));
      ("%", (operator (fun s -> Tok_pct_op s)));
      ("+", (operator (fun s -> Tok_plus_op s)));
      ("-", (operator (fun s -> Tok_minus_op s)));
      ("@", (operator (fun s -> Tok_at_op s)));
      ("<", (operator (fun s -> Tok_lt_op s)));
      ("=", (operator (fun s -> Tok_eq_op s)));
      (">", (operator (fun s -> Tok_gt_op s)));
      ("|", (act {
          edges=(map_of_cps_alist [
            ("]", (act {
                edges=Map.empty (module Codepoint);
                eoi=(accept Tok_rarray);
                default=(accept_excl Tok_rarray);
              }));
            (operator_cps, (operator (fun s -> Tok_bar_op s)));
          ]);
          eoi=(accept Tok_bar);
          default=(accept_excl Tok_bar);
        }));
      (" ", whitespace);
      ("#", hash_comment);
      ("abcdefghijklmnopqrstuvwxyz_", uident);
      ("ABCDEFGHIJKLMNOPQRSTUVWXYZ", cident);
      ("'", Codepoint_.codepoint);
      ("\"", String_.istring);
      ("`", (act {
          edges=Map.singleton (module Codepoint)
            ~k:(Codepoint.of_char '|') ~v:String_.bstring;
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
            (".", Real.zero_frac);
            ("e", Real.zero_exp);
            ("ABCDEFGHIJKLMNOPQRSTUVWXYZacdfghjklmnpqstvwyz'",
              Integer.mal_ident);
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
    act start_state t.cursor t.cursor t
end

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
        let bias = Sint.(ni - (Uns.to_sint (Text.(Pos.line (Cursor.pos
            cursor))))) in
        {t with path; bias; cursor}, None
      end
    | None -> accept_error cursor t

  let path_finish path n cursor t =
    match Text.Cursor.next_opt cursor with
    | None -> accept_error cursor t
    | Some (cp, cursor') when Codepoint.(cp = nl) ->
      accept_directive ~path n cursor' t
    | Some (_, cursor') -> error cursor' t

  let path_start n cursor t =
    match Text.Cursor.next_opt cursor with
    | None -> accept_error cursor t
    | Some (cp, cursor') when Codepoint.(cp = of_char '"') -> begin
        let t', path_tok = String_.istring cursor cursor' t in
        match ConcreteToken.atoken path_tok with
        | Tok_istring (Constant path) -> path_finish path n t'.cursor t
        | Tok_istring (Malformed _) -> error cursor' t
        | _ -> not_reached ()
      end
    | Some (_, cursor') -> error cursor' t

  let rec n_cont n cursor t =
    match Text.Cursor.next_opt cursor with
    | None -> accept_error cursor t
    | Some (cp, cursor') -> begin
        match Map.get cp trailing_digit_map  with
        | Some LDMapDigit ->n_cont Nat.(n * k_a + (nat_of_cp cp)) cursor' t
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

  let rec next cursor t =
    match Text.Cursor.next_opt cursor with
    | None -> begin
        match t.line_state with
        | LineDentation -> accept Tok_whitespace cursor t
        | LineDelim -> accept_dentation Tok_line_delim cursor t
        | LineBody -> not_reached ()
      end
    | Some (cp, cursor') -> begin
        match cp with
        | cp when Codepoint.(cp = of_char ' ') -> next cursor' t
        | cp when Codepoint.(cp = nl) -> accept_delim Tok_whitespace cursor' t
        | _ -> begin
            let col = Text.(Pos.col (Cursor.pos cursor)) in
            let level = col / 4 in
            let rem = col % 4 in
            (* The following patterns incrementally handle all
             * dentation/alignment cases. Tokens are synthesized in error cases
             * such that the cursor does not advance, but the level is
             * incrementally adjusted to converge. The overall result is that
             * Tok_indent[_absent]/Tok_dedent[_absent] nesting is always well
             * formed, with the expectation that the parser converts the
             * Tok_{in,de}dent_absent tokens to their non-error forms when
             * feeding the parser, but notes them as errors for post-parse error
             * reporting. *)
            match rem, t.level, level with
            (* New expression at same level. *)
            | 0, t_level, level when t_level = level -> begin
                match t.line_state with
                | LineDentation -> Dag.start {t with line_state=LineBody}
                | LineDelim -> accept_dentation Tok_line_delim cursor t
                | LineBody -> not_reached ()
              end

            (* Continuation of expression at current level. *)
            | 2, t_level, level when t_level = level ->
              accept_dentation Tok_whitespace cursor t

            (* New expression at higher level. *)
            | 0, t_level, level when succ t_level = level ->
              accept_dentation Tok_indent cursor {t with level}

            (* Continuation of expression at lower level. *)
            | 2, t_level, level when t_level > succ level ->
              accept Tok_dedent t.cursor {t with level=pred t_level}
            | 2, t_level, level when t_level = succ level ->
              accept_dentation Tok_dedent cursor {t with level}

            (* New expression at lower level. *)
            | 0, t_level, level when t_level > succ level ->
              accept Tok_dedent t.cursor {t with level=pred t_level}
            | 0, t_level, level when t_level = succ level ->
              accept_dentation Tok_dedent cursor {t with level}

            (* Off by one column at lower level. *)
            | 3, t_level, level when t_level > succ level ->
              accept Tok_dedent_absent t.cursor {t with level=pred t_level}
            | 1, t_level, level when t_level > level ->
              accept Tok_dedent_absent t.cursor {t with level=pred t_level}

            (* Off by one column at current level. *)
            | 3, t_level, level when t_level = succ level ->
              accept_dentation Tok_misaligned cursor t
            | 1, t_level, level when t_level = level ->
              accept_dentation Tok_misaligned cursor t

            (* Excess aligned indentation. *)
            | 0, t_level, level when succ t_level < level ->
              accept Tok_indent_absent t.cursor {t with level=succ t_level}

            (* Off by one column at higher level. *)
            | 3, t_level, level when t_level < succ level ->
              accept Tok_indent_absent t.cursor {t with level=succ t_level}
            | 1, t_level, level when t_level < level ->
              accept Tok_indent_absent t.cursor {t with level=succ t_level}

            (* Continuation of expression at higher level. *)
            | 2, t_level, level when t_level < level ->
              accept Tok_indent_absent t.cursor {t with level=succ t_level}

            | _ -> not_reached ()
          end
      end

  (* Lines comprising only whitespace and/or comments are ignored with regard to
   * indentation. Leading paren comments are problematic in that we must look
   * ahead far enough to determine whether the line contains an expression.
   * While this could require looking ahead an arbitrary number of tokens, in
   * the overwhelmingly common case the leading paren comment is immediately
   * followed by line-delimiting whitespace. In the LineNoop case the leading
   * paren comment only gets scanned once, and therefore the line-delimiting
   * whitespace token is the only token to be scanned twice in the common case.
  *)
  let paren_comment_lookahead pcursor cursor t =
    let rec fn t = begin
      let t', ctoken = Dag.start t in
      match ctoken.atoken, t'.line_state with
      | Tok_end_of_input, _
      | Tok_hash_comment, _
      | Tok_whitespace, LineDelim -> true
      | Tok_paren_comment _, _
      | Tok_whitespace, LineDentation -> fn t'
      | Tok_whitespace, LineBody -> not_reached ()
      | _ -> false
    end in
    match paren_comment pcursor cursor t with
    | t', ctoken -> begin
        match fn {t' with line_state=LineDentation} with
        | false -> LineExpr
        | true -> LineNoop ({t' with line_state=LineBody}, ctoken)
      end

  let other cursor t =
    match t.level with
    | 0 -> begin
        match t.line_state with
        | LineDentation -> Dag.start {t with line_state=LineBody}
        | LineDelim -> accept_dentation Tok_line_delim cursor t
        | LineBody -> not_reached ()
      end
    | 1 -> accept_dentation Tok_dedent cursor {t with level=0}
    | _ -> accept Tok_dedent cursor {t with level=pred t.level}

  let rec start cursor t =
    match Text.Cursor.next_opt cursor with
    | None -> end_of_input cursor t
    | Some (cp, cursor') -> begin
        match cp with
        | cp when Codepoint.(cp = of_char ' ') -> next cursor' t
        | cp when Codepoint.(cp = nl) -> accept_delim Tok_whitespace cursor' t
        | cp when Codepoint.(cp = of_char '#') -> hash_comment cursor cursor' t
        | cp when Codepoint.(cp = of_char '(') -> begin
            match Text.Cursor.next_opt cursor' with
            | None -> other cursor t
            | Some (cp, cursor'') -> begin
                match cp with
                | cp when Codepoint.(cp = of_char '*') -> begin
                    match paren_comment_lookahead cursor' cursor'' t with
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
  | LineDentation
  | LineDelim -> Dentation.start t.cursor t
  | LineBody -> Dag.start t

(******************************************************************************)
(* Begin tests. *)

let scan_str s =
  let open Format in
  let rec fn t = begin
    let t', ctoken = next t in
    let atoken = ConcreteToken.atoken ctoken in
    let source = ConcreteToken.source ctoken in
    printf "  %a : %s\n"
      Source.pp_loc source
      (AbstractToken.to_string atoken)
    ;
    match atoken with
    | Tok_end_of_input -> ()
    | _ -> fn t'
  end in
  printf "{|%s|}\n" s;
  let t = init (Text.of_string_slice (String.Slice.of_string s)) in
  fn t

let%expect_test "comment" =
  let open Format in
  printf "@[<h>";
  scan_str "#";
  scan_str "#...";
  scan_str "#...\n#...";
  scan_str "#...\n    #...";
  scan_str "(**)";
  scan_str "(***)";
  scan_str "(*(*(#*#)*)*)";
  scan_str "(**";
  scan_str "(*)";
  printf "@]";

  [%expect{xxx|
    {|#|}
      [1:0..1:1) : <Tok_hash_comment>
      [1:1..1:1) : <Tok_line_delim>
      [1:1..1:1) : <Tok_end_of_input>
    {|#...|}
      [1:0..1:4) : <Tok_hash_comment>
      [1:4..1:4) : <Tok_line_delim>
      [1:4..1:4) : <Tok_end_of_input>
    {|#...
    #...|}
      [1:0..2:0) : <Tok_hash_comment>
      [2:0..2:4) : <Tok_hash_comment>
      [2:4..2:4) : <Tok_line_delim>
      [2:4..2:4) : <Tok_end_of_input>
    {|#...
        #...|}
      [1:0..2:0) : <Tok_hash_comment>
      [2:0..2:4) : <Tok_indent>
      [2:4..2:8) : <Tok_hash_comment>
      [2:8..2:8) : <Tok_dedent>
      [2:8..2:8) : <Tok_end_of_input>
    {|(**)|}
      [1:0..1:4) : <Tok_paren_comment>
      [1:4..1:4) : <Tok_end_of_input>
    {|(***)|}
      [1:0..1:5) : <Tok_paren_comment>
      [1:5..1:5) : <Tok_end_of_input>
    {|(*(*(#*#)*)*)|}
      [1:0..1:13) : <Tok_paren_comment>
      [1:13..1:13) : <Tok_end_of_input>
    {|(**|}
      [1:0..1:3) : <Tok_paren_comment=Malformed ["[1:0..1:3): Unterminated comment"]>
      [1:3..1:3) : <Tok_end_of_input>
    {|(*)|}
      [1:0..1:3) : <Tok_paren_comment=Malformed ["[1:0..1:3): Unterminated comment"]>
      [1:3..1:3) : <Tok_end_of_input>
   |xxx}]

let%expect_test "dentation" =
  let open Format in
  printf "@[<h>";
  scan_str "";
  scan_str "a";
  scan_str "\n";
  scan_str "    a";
  scan_str {|    a
|};
  scan_str {|    a
b
|};

  scan_str {|a
b
c
    d
    e
    f
g
h
i
|};

  scan_str {|a
    b
      c
    d
e
|};

  scan_str {|
a
b
    c
d
|};

  scan_str {|
a
    b
  c
    d
|};
  scan_str {|a
    b
    c

    d
        e
        f|};
  scan_str {|\
    a\
    b \
    c
    d|};
  scan_str "\t|";
  scan_str "  ";
  scan_str "\n  \n";
  scan_str "\\\n";
  scan_str "\\\na";
  scan_str "\\\n\\\na";
  scan_str {|# a
b
    c
    # d
# e
    f
g
|};
  scan_str "a\n  \n  \nb";
  scan_str {|a
    b
(* Ignore. *)
    c
|};
  scan_str {|a
    b
(* Ignore. *) (* ... *) # ...
    c
|};
  scan_str {|a
    b
(* Ignore. *) (*
 ... *) \
# ...
    c
|};
  scan_str {|a
    b
(* Don't ignore. *) true
    c
|};
  scan_str {|a
    b
(* Don't ignore. *) (* ...
 *) \
 true
    c
|};
  scan_str {|a
    b
(* Don't ignore. *) (*
 ... *) true
    c
|};
  scan_str {|a
    b
        c
            d
    e
|};
  scan_str {|a
    b
        c
            d
      e
    f
|};
  scan_str {|a
    b
    c
  d
    e
        f
      g
        h
    i
|};
  scan_str {|a
  b
  c
d
|};
  scan_str {|a
        b
|};
  scan_str {|a
    b
        c
d
|};

  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
a2
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
 x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
  a'
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
   x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
    b2
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
     x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
      b'
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
       x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
        c2
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
         x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
          c'
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
           x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
            d2
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
             x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
              d'
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
               x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
                e
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
                 x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
                  x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
                   x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
                    x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
                     x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
                      x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
                       x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
                        x
|};

  printf "@]";

  [%expect{xxx|
    {||}
      [1:0..1:0) : <Tok_end_of_input>
    {|a|}
      [1:0..1:1) : <Tok_uident="a">
      [1:1..1:1) : <Tok_end_of_input>
    {|
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..2:0) : <Tok_line_delim>
      [2:0..2:0) : <Tok_end_of_input>
    {|    a|}
      [1:0..1:4) : <Tok_indent>
      [1:4..1:5) : <Tok_uident="a">
      [1:5..1:5) : <Tok_dedent>
      [1:5..1:5) : <Tok_end_of_input>
    {|    a
    |}
      [1:0..1:4) : <Tok_indent>
      [1:4..1:5) : <Tok_uident="a">
      [1:5..2:0) : <Tok_whitespace>
      [2:0..2:0) : <Tok_dedent>
      [2:0..2:0) : <Tok_end_of_input>
    {|    a
    b
    |}
      [1:0..1:4) : <Tok_indent>
      [1:4..1:5) : <Tok_uident="a">
      [1:5..2:0) : <Tok_whitespace>
      [2:0..2:0) : <Tok_dedent>
      [2:0..2:1) : <Tok_uident="b">
      [2:1..3:0) : <Tok_whitespace>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:0) : <Tok_end_of_input>
    {|a
    b
    c
        d
        e
        f
    g
    h
    i
    |}
      [1:0..1:1) : <Tok_uident="a">
      [1:1..2:0) : <Tok_whitespace>
      [2:0..2:0) : <Tok_line_delim>
      [2:0..2:1) : <Tok_uident="b">
      [2:1..3:0) : <Tok_whitespace>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="c">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="d">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:4) : <Tok_line_delim>
      [5:4..5:5) : <Tok_uident="e">
      [5:5..6:0) : <Tok_whitespace>
      [6:0..6:4) : <Tok_line_delim>
      [6:4..6:5) : <Tok_uident="f">
      [6:5..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_dedent>
      [7:0..7:1) : <Tok_uident="g">
      [7:1..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_line_delim>
      [8:0..8:1) : <Tok_uident="h">
      [8:1..9:0) : <Tok_whitespace>
      [9:0..9:0) : <Tok_line_delim>
      [9:0..9:1) : <Tok_uident="i">
      [9:1..10:0) : <Tok_whitespace>
      [10:0..10:0) : <Tok_line_delim>
      [10:0..10:0) : <Tok_end_of_input>
    {|a
        b
          c
        d
    e
    |}
      [1:0..1:1) : <Tok_uident="a">
      [1:1..2:0) : <Tok_whitespace>
      [2:0..2:4) : <Tok_indent>
      [2:4..2:5) : <Tok_uident="b">
      [2:5..3:0) : <Tok_whitespace>
      [3:0..3:6) : <Tok_whitespace>
      [3:6..3:7) : <Tok_uident="c">
      [3:7..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_line_delim>
      [4:4..4:5) : <Tok_uident="d">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:0) : <Tok_dedent>
      [5:0..5:1) : <Tok_uident="e">
      [5:1..6:0) : <Tok_whitespace>
      [6:0..6:0) : <Tok_line_delim>
      [6:0..6:0) : <Tok_end_of_input>
    {|
    a
    b
        c
    d
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..2:0) : <Tok_line_delim>
      [2:0..2:1) : <Tok_uident="a">
      [2:1..3:0) : <Tok_whitespace>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="b">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="c">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:0) : <Tok_dedent>
      [5:0..5:1) : <Tok_uident="d">
      [5:1..6:0) : <Tok_whitespace>
      [6:0..6:0) : <Tok_line_delim>
      [6:0..6:0) : <Tok_end_of_input>
    {|
    a
        b
      c
        d
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..2:0) : <Tok_line_delim>
      [2:0..2:1) : <Tok_uident="a">
      [2:1..3:0) : <Tok_whitespace>
      [3:0..3:4) : <Tok_indent>
      [3:4..3:5) : <Tok_uident="b">
      [3:5..4:0) : <Tok_whitespace>
      [4:0..4:2) : <Tok_dedent>
      [4:2..4:3) : <Tok_uident="c">
      [4:3..5:0) : <Tok_whitespace>
      [5:0..5:4) : <Tok_indent>
      [5:4..5:5) : <Tok_uident="d">
      [5:5..6:0) : <Tok_whitespace>
      [6:0..6:0) : <Tok_dedent>
      [6:0..6:0) : <Tok_end_of_input>
    {|a
        b
        c

        d
            e
            f|}
      [1:0..1:1) : <Tok_uident="a">
      [1:1..2:0) : <Tok_whitespace>
      [2:0..2:4) : <Tok_indent>
      [2:4..2:5) : <Tok_uident="b">
      [2:5..3:0) : <Tok_whitespace>
      [3:0..3:4) : <Tok_line_delim>
      [3:4..3:5) : <Tok_uident="c">
      [3:5..4:0) : <Tok_whitespace>
      [4:0..5:0) : <Tok_whitespace>
      [5:0..5:4) : <Tok_line_delim>
      [5:4..5:5) : <Tok_uident="d">
      [5:5..6:0) : <Tok_whitespace>
      [6:0..6:8) : <Tok_indent>
      [6:8..6:9) : <Tok_uident="e">
      [6:9..7:0) : <Tok_whitespace>
      [7:0..7:8) : <Tok_line_delim>
      [7:8..7:9) : <Tok_uident="f">
      [7:9..7:9) : <Tok_dedent>
      [7:9..7:9) : <Tok_dedent>
      [7:9..7:9) : <Tok_end_of_input>
    {|\
        a\
        b \
        c
        d|}
      [1:0..2:4) : <Tok_whitespace>
      [2:4..2:5) : <Tok_uident="a">
      [2:5..3:4) : <Tok_whitespace>
      [3:4..3:5) : <Tok_uident="b">
      [3:5..4:4) : <Tok_whitespace>
      [4:4..4:5) : <Tok_uident="c">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:4) : <Tok_indent>
      [5:4..5:5) : <Tok_uident="d">
      [5:5..5:5) : <Tok_dedent>
      [5:5..5:5) : <Tok_end_of_input>
    {|	||}
      [1:0..1:8) : <Tok_error>
      [1:8..1:9) : <Tok_bar>
      [1:9..1:9) : <Tok_end_of_input>
    {|  |}
      [1:0..1:2) : <Tok_whitespace>
      [1:2..1:2) : <Tok_end_of_input>
    {|

    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_whitespace>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:0) : <Tok_end_of_input>
    {|\
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..2:0) : <Tok_end_of_input>
    {|\
    a|}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..2:1) : <Tok_uident="a">
      [2:1..2:1) : <Tok_end_of_input>
    {|\
    \
    a|}
      [1:0..3:0) : <Tok_whitespace>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..3:1) : <Tok_end_of_input>
    {|# a
    b
        c
        # d
    # e
        f
    g
    |}
      [1:0..2:0) : <Tok_hash_comment>
      [2:0..2:0) : <Tok_line_delim>
      [2:0..2:1) : <Tok_uident="b">
      [2:1..3:0) : <Tok_whitespace>
      [3:0..3:4) : <Tok_indent>
      [3:4..3:5) : <Tok_uident="c">
      [3:5..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_line_delim>
      [4:4..5:0) : <Tok_hash_comment>
      [5:0..6:0) : <Tok_hash_comment>
      [6:0..6:4) : <Tok_line_delim>
      [6:4..6:5) : <Tok_uident="f">
      [6:5..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_dedent>
      [7:0..7:1) : <Tok_uident="g">
      [7:1..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_line_delim>
      [8:0..8:0) : <Tok_end_of_input>
    {|a


    b|}
      [1:0..1:1) : <Tok_uident="a">
      [1:1..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_whitespace>
      [3:0..4:0) : <Tok_whitespace>
      [4:0..4:0) : <Tok_line_delim>
      [4:0..4:1) : <Tok_uident="b">
      [4:1..4:1) : <Tok_end_of_input>
    {|a
        b
    (* Ignore. *)
        c
    |}
      [1:0..1:1) : <Tok_uident="a">
      [1:1..2:0) : <Tok_whitespace>
      [2:0..2:4) : <Tok_indent>
      [2:4..2:5) : <Tok_uident="b">
      [2:5..3:0) : <Tok_whitespace>
      [3:0..3:13) : <Tok_paren_comment>
      [3:13..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_line_delim>
      [4:4..4:5) : <Tok_uident="c">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:0) : <Tok_dedent>
      [5:0..5:0) : <Tok_end_of_input>
    {|a
        b
    (* Ignore. *) (* ... *) # ...
        c
    |}
      [1:0..1:1) : <Tok_uident="a">
      [1:1..2:0) : <Tok_whitespace>
      [2:0..2:4) : <Tok_indent>
      [2:4..2:5) : <Tok_uident="b">
      [2:5..3:0) : <Tok_whitespace>
      [3:0..3:13) : <Tok_paren_comment>
      [3:13..3:14) : <Tok_whitespace>
      [3:14..3:23) : <Tok_paren_comment>
      [3:23..3:24) : <Tok_whitespace>
      [3:24..4:0) : <Tok_hash_comment>
      [4:0..4:4) : <Tok_line_delim>
      [4:4..4:5) : <Tok_uident="c">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:0) : <Tok_dedent>
      [5:0..5:0) : <Tok_end_of_input>
    {|a
        b
    (* Ignore. *) (*
     ... *) \
    # ...
        c
    |}
      [1:0..1:1) : <Tok_uident="a">
      [1:1..2:0) : <Tok_whitespace>
      [2:0..2:4) : <Tok_indent>
      [2:4..2:5) : <Tok_uident="b">
      [2:5..3:0) : <Tok_whitespace>
      [3:0..3:13) : <Tok_paren_comment>
      [3:13..3:14) : <Tok_whitespace>
      [3:14..4:7) : <Tok_paren_comment>
      [4:7..5:0) : <Tok_whitespace>
      [5:0..6:0) : <Tok_hash_comment>
      [6:0..6:4) : <Tok_line_delim>
      [6:4..6:5) : <Tok_uident="c">
      [6:5..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_dedent>
      [7:0..7:0) : <Tok_end_of_input>
    {|a
        b
    (* Don't ignore. *) true
        c
    |}
      [1:0..1:1) : <Tok_uident="a">
      [1:1..2:0) : <Tok_whitespace>
      [2:0..2:4) : <Tok_indent>
      [2:4..2:5) : <Tok_uident="b">
      [2:5..3:0) : <Tok_whitespace>
      [3:0..3:0) : <Tok_dedent>
      [3:0..3:19) : <Tok_paren_comment>
      [3:19..3:20) : <Tok_whitespace>
      [3:20..3:24) : <Tok_true>
      [3:24..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="c">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:0) : <Tok_dedent>
      [5:0..5:0) : <Tok_end_of_input>
    {|a
        b
    (* Don't ignore. *) (* ...
     *) \
     true
        c
    |}
      [1:0..1:1) : <Tok_uident="a">
      [1:1..2:0) : <Tok_whitespace>
      [2:0..2:4) : <Tok_indent>
      [2:4..2:5) : <Tok_uident="b">
      [2:5..3:0) : <Tok_whitespace>
      [3:0..3:0) : <Tok_dedent>
      [3:0..3:19) : <Tok_paren_comment>
      [3:19..3:20) : <Tok_whitespace>
      [3:20..4:3) : <Tok_paren_comment>
      [4:3..5:1) : <Tok_whitespace>
      [5:1..5:5) : <Tok_true>
      [5:5..6:0) : <Tok_whitespace>
      [6:0..6:4) : <Tok_indent>
      [6:4..6:5) : <Tok_uident="c">
      [6:5..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_dedent>
      [7:0..7:0) : <Tok_end_of_input>
    {|a
        b
    (* Don't ignore. *) (*
     ... *) true
        c
    |}
      [1:0..1:1) : <Tok_uident="a">
      [1:1..2:0) : <Tok_whitespace>
      [2:0..2:4) : <Tok_indent>
      [2:4..2:5) : <Tok_uident="b">
      [2:5..3:0) : <Tok_whitespace>
      [3:0..3:0) : <Tok_dedent>
      [3:0..3:19) : <Tok_paren_comment>
      [3:19..3:20) : <Tok_whitespace>
      [3:20..4:7) : <Tok_paren_comment>
      [4:7..4:8) : <Tok_whitespace>
      [4:8..4:12) : <Tok_true>
      [4:12..5:0) : <Tok_whitespace>
      [5:0..5:4) : <Tok_indent>
      [5:4..5:5) : <Tok_uident="c">
      [5:5..6:0) : <Tok_whitespace>
      [6:0..6:0) : <Tok_dedent>
      [6:0..6:0) : <Tok_end_of_input>
    {|a
        b
            c
                d
        e
    |}
      [1:0..1:1) : <Tok_uident="a">
      [1:1..2:0) : <Tok_whitespace>
      [2:0..2:4) : <Tok_indent>
      [2:4..2:5) : <Tok_uident="b">
      [2:5..3:0) : <Tok_whitespace>
      [3:0..3:8) : <Tok_indent>
      [3:8..3:9) : <Tok_uident="c">
      [3:9..4:0) : <Tok_whitespace>
      [4:0..4:12) : <Tok_indent>
      [4:12..4:13) : <Tok_uident="d">
      [4:13..5:0) : <Tok_whitespace>
      [5:0..5:0) : <Tok_dedent>
      [5:0..5:4) : <Tok_dedent>
      [5:4..5:5) : <Tok_uident="e">
      [5:5..6:0) : <Tok_whitespace>
      [6:0..6:0) : <Tok_dedent>
      [6:0..6:0) : <Tok_end_of_input>
    {|a
        b
            c
                d
          e
        f
    |}
      [1:0..1:1) : <Tok_uident="a">
      [1:1..2:0) : <Tok_whitespace>
      [2:0..2:4) : <Tok_indent>
      [2:4..2:5) : <Tok_uident="b">
      [2:5..3:0) : <Tok_whitespace>
      [3:0..3:8) : <Tok_indent>
      [3:8..3:9) : <Tok_uident="c">
      [3:9..4:0) : <Tok_whitespace>
      [4:0..4:12) : <Tok_indent>
      [4:12..4:13) : <Tok_uident="d">
      [4:13..5:0) : <Tok_whitespace>
      [5:0..5:0) : <Tok_dedent>
      [5:0..5:6) : <Tok_dedent>
      [5:6..5:7) : <Tok_uident="e">
      [5:7..6:0) : <Tok_whitespace>
      [6:0..6:4) : <Tok_line_delim>
      [6:4..6:5) : <Tok_uident="f">
      [6:5..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_dedent>
      [7:0..7:0) : <Tok_end_of_input>
    {|a
        b
        c
      d
        e
            f
          g
            h
        i
    |}
      [1:0..1:1) : <Tok_uident="a">
      [1:1..2:0) : <Tok_whitespace>
      [2:0..2:4) : <Tok_indent>
      [2:4..2:5) : <Tok_uident="b">
      [2:5..3:0) : <Tok_whitespace>
      [3:0..3:4) : <Tok_line_delim>
      [3:4..3:5) : <Tok_uident="c">
      [3:5..4:0) : <Tok_whitespace>
      [4:0..4:2) : <Tok_dedent>
      [4:2..4:3) : <Tok_uident="d">
      [4:3..5:0) : <Tok_whitespace>
      [5:0..5:4) : <Tok_indent>
      [5:4..5:5) : <Tok_uident="e">
      [5:5..6:0) : <Tok_whitespace>
      [6:0..6:8) : <Tok_indent>
      [6:8..6:9) : <Tok_uident="f">
      [6:9..7:0) : <Tok_whitespace>
      [7:0..7:6) : <Tok_dedent>
      [7:6..7:7) : <Tok_uident="g">
      [7:7..8:0) : <Tok_whitespace>
      [8:0..8:8) : <Tok_indent>
      [8:8..8:9) : <Tok_uident="h">
      [8:9..9:0) : <Tok_whitespace>
      [9:0..9:4) : <Tok_dedent>
      [9:4..9:5) : <Tok_uident="i">
      [9:5..10:0) : <Tok_whitespace>
      [10:0..10:0) : <Tok_dedent>
      [10:0..10:0) : <Tok_end_of_input>
    {|a
      b
      c
    d
    |}
      [1:0..1:1) : <Tok_uident="a">
      [1:1..2:0) : <Tok_whitespace>
      [2:0..2:2) : <Tok_whitespace>
      [2:2..2:3) : <Tok_uident="b">
      [2:3..3:0) : <Tok_whitespace>
      [3:0..3:2) : <Tok_whitespace>
      [3:2..3:3) : <Tok_uident="c">
      [3:3..4:0) : <Tok_whitespace>
      [4:0..4:0) : <Tok_line_delim>
      [4:0..4:1) : <Tok_uident="d">
      [4:1..5:0) : <Tok_whitespace>
      [5:0..5:0) : <Tok_line_delim>
      [5:0..5:0) : <Tok_end_of_input>
    {|a
            b
    |}
      [1:0..1:1) : <Tok_uident="a">
      [1:1..2:0) : <Tok_whitespace>
      [2:0..2:0) : <Tok_indent_absent>
      [2:0..2:8) : <Tok_indent>
      [2:8..2:9) : <Tok_uident="b">
      [2:9..3:0) : <Tok_whitespace>
      [3:0..3:0) : <Tok_dedent>
      [3:0..3:0) : <Tok_dedent>
      [3:0..3:0) : <Tok_end_of_input>
    {|a
        b
            c
    d
    |}
      [1:0..1:1) : <Tok_uident="a">
      [1:1..2:0) : <Tok_whitespace>
      [2:0..2:4) : <Tok_indent>
      [2:4..2:5) : <Tok_uident="b">
      [2:5..3:0) : <Tok_whitespace>
      [3:0..3:8) : <Tok_indent>
      [3:8..3:9) : <Tok_uident="c">
      [3:9..4:0) : <Tok_whitespace>
      [4:0..4:0) : <Tok_dedent>
      [4:0..4:0) : <Tok_dedent>
      [4:0..4:1) : <Tok_uident="d">
      [4:1..5:0) : <Tok_whitespace>
      [5:0..5:0) : <Tok_line_delim>
      [5:0..5:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
    a2
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_dedent>
      [7:0..7:0) : <Tok_dedent>
      [7:0..7:0) : <Tok_dedent>
      [7:0..7:2) : <Tok_uident="a2">
      [7:2..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_line_delim>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
     x
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_dedent_absent>
      [7:0..7:0) : <Tok_dedent_absent>
      [7:0..7:0) : <Tok_dedent_absent>
      [7:0..7:1) : <Tok_misaligned>
      [7:1..7:2) : <Tok_uident="x">
      [7:2..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_line_delim>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
      a'
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_dedent>
      [7:0..7:0) : <Tok_dedent>
      [7:0..7:2) : <Tok_dedent>
      [7:2..7:4) : <Tok_uident="a'">
      [7:4..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_line_delim>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
       x
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_dedent_absent>
      [7:0..7:0) : <Tok_dedent_absent>
      [7:0..7:3) : <Tok_misaligned>
      [7:3..7:4) : <Tok_uident="x">
      [7:4..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
        b2
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_dedent>
      [7:0..7:4) : <Tok_dedent>
      [7:4..7:6) : <Tok_uident="b2">
      [7:6..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
         x
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_dedent_absent>
      [7:0..7:0) : <Tok_dedent_absent>
      [7:0..7:5) : <Tok_misaligned>
      [7:5..7:6) : <Tok_uident="x">
      [7:6..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
          b'
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_dedent>
      [7:0..7:6) : <Tok_dedent>
      [7:6..7:8) : <Tok_uident="b'">
      [7:8..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
           x
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_dedent_absent>
      [7:0..7:7) : <Tok_misaligned>
      [7:7..7:8) : <Tok_uident="x">
      [7:8..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
            c2
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:8) : <Tok_dedent>
      [7:8..7:10) : <Tok_uident="c2">
      [7:10..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
             x
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_dedent_absent>
      [7:0..7:9) : <Tok_misaligned>
      [7:9..7:10) : <Tok_uident="x">
      [7:10..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
              c'
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:10) : <Tok_dedent>
      [7:10..7:12) : <Tok_uident="c'">
      [7:12..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
               x
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:11) : <Tok_misaligned>
      [7:11..7:12) : <Tok_uident="x">
      [7:12..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
                d2
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:12) : <Tok_line_delim>
      [7:12..7:14) : <Tok_uident="d2">
      [7:14..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
                 x
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:13) : <Tok_misaligned>
      [7:13..7:14) : <Tok_uident="x">
      [7:14..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
                  d'
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:14) : <Tok_whitespace>
      [7:14..7:16) : <Tok_uident="d'">
      [7:16..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
                   x
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_indent_absent>
      [7:0..7:15) : <Tok_misaligned>
      [7:15..7:16) : <Tok_uident="x">
      [7:16..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
                    e
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:16) : <Tok_indent>
      [7:16..7:17) : <Tok_uident="e">
      [7:17..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
                     x
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_indent_absent>
      [7:0..7:17) : <Tok_misaligned>
      [7:17..7:18) : <Tok_uident="x">
      [7:18..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
                      x
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_indent_absent>
      [7:0..7:18) : <Tok_whitespace>
      [7:18..7:19) : <Tok_uident="x">
      [7:19..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
                       x
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_indent_absent>
      [7:0..7:0) : <Tok_indent_absent>
      [7:0..7:19) : <Tok_misaligned>
      [7:19..7:20) : <Tok_uident="x">
      [7:20..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
                        x
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_indent_absent>
      [7:0..7:20) : <Tok_indent>
      [7:20..7:21) : <Tok_uident="x">
      [7:21..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
                         x
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_indent_absent>
      [7:0..7:0) : <Tok_indent_absent>
      [7:0..7:21) : <Tok_misaligned>
      [7:21..7:22) : <Tok_uident="x">
      [7:22..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
                          x
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_indent_absent>
      [7:0..7:0) : <Tok_indent_absent>
      [7:0..7:22) : <Tok_whitespace>
      [7:22..7:23) : <Tok_uident="x">
      [7:23..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
                           x
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_indent_absent>
      [7:0..7:0) : <Tok_indent_absent>
      [7:0..7:0) : <Tok_indent_absent>
      [7:0..7:23) : <Tok_misaligned>
      [7:23..7:24) : <Tok_uident="x">
      [7:24..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_end_of_input>
    {|
    # : | : | : | : | : | : |
    a
        b
            c
                d
                            x
    |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..3:0) : <Tok_hash_comment>
      [3:0..3:0) : <Tok_line_delim>
      [3:0..3:1) : <Tok_uident="a">
      [3:1..4:0) : <Tok_whitespace>
      [4:0..4:4) : <Tok_indent>
      [4:4..4:5) : <Tok_uident="b">
      [4:5..5:0) : <Tok_whitespace>
      [5:0..5:8) : <Tok_indent>
      [5:8..5:9) : <Tok_uident="c">
      [5:9..6:0) : <Tok_whitespace>
      [6:0..6:12) : <Tok_indent>
      [6:12..6:13) : <Tok_uident="d">
      [6:13..7:0) : <Tok_whitespace>
      [7:0..7:0) : <Tok_indent_absent>
      [7:0..7:0) : <Tok_indent_absent>
      [7:0..7:24) : <Tok_indent>
      [7:24..7:25) : <Tok_uident="x">
      [7:25..8:0) : <Tok_whitespace>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_dedent>
      [8:0..8:0) : <Tok_end_of_input>
    |xxx}]

let%expect_test "line directive" =
  let open Format in
  printf "@[<h>";

  scan_str {|:1
|};
  scan_str {|:10123456789
|};
  scan_str {|:42 "foo.hm"
|};

  (* Errors. *)
  scan_str {|:a|};
  scan_str {|:0|};
  scan_str {|:1 |};
  scan_str {|:1"|};
  scan_str {|:1 "foo.hm" |};
  scan_str {|:1 "foo.hm"x
|};

  printf "@]";

  [%expect{xxx|
    {|:1
    |}
      [1:0..1:0) : <Tok_end_of_input>
    {|:10123456789
    |}
      [10123456789:0..10123456789:0) : <Tok_end_of_input>
    {|:42 "foo.hm"
    |}
      foo.hm:[42:0..42:0) : <Tok_end_of_input>
    {|:a|}
      [1:0..1:2) : <Tok_error>
      [1:2..1:2) : <Tok_end_of_input>
    {|:0|}
      [1:0..1:2) : <Tok_error>
      [1:2..1:2) : <Tok_end_of_input>
    {|:1 |}
      [1:0..1:3) : <Tok_error>
      [1:3..1:3) : <Tok_end_of_input>
    {|:1"|}
      [1:0..1:3) : <Tok_error>
      [1:3..1:3) : <Tok_end_of_input>
    {|:1 "foo.hm" |}
      [1:0..1:12) : <Tok_error>
      [1:12..1:12) : <Tok_end_of_input>
    {|:1 "foo.hm"x
    |}
      [1:0..2:0) : <Tok_error>
      [2:0..2:0) : <Tok_end_of_input>
   |xxx}]

let%expect_test "punctuation" =
  let open Format in
  printf "@[<h>";
  scan_str ", ,,";
  scan_str ". ..";
  scan_str "; ;; ;;;";
  scan_str "; : := :: :::"; (* Avoid line directive syntax. *)
  scan_str "]|]|[|[";
  scan_str "{}";
  scan_str {|\^&\&\|};
  scan_str "- -> ->> --> ->";
  scan_str "> >- >-> >->> >>-> >-> >e-> >{>e,mut}->";
  scan_str "~f ~> >{mut}~> ~- ~-+*/%@!$<=>|:.~?";
  scan_str "?x ?? ?-+*/%@!$<=>|:.~?";
  printf "@]";

  [%expect{xxx|
    {|, ,,|}
      [1:0..1:1) : <Tok_comma>
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:3) : <Tok_comma>
      [1:3..1:4) : <Tok_comma>
      [1:4..1:4) : <Tok_end_of_input>
    {|. ..|}
      [1:0..1:1) : <Tok_dot>
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:3) : <Tok_dot>
      [1:3..1:4) : <Tok_dot>
      [1:4..1:4) : <Tok_end_of_input>
    {|; ;; ;;;|}
      [1:0..1:1) : <Tok_semi>
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:3) : <Tok_semi>
      [1:3..1:4) : <Tok_semi>
      [1:4..1:5) : <Tok_whitespace>
      [1:5..1:6) : <Tok_semi>
      [1:6..1:7) : <Tok_semi>
      [1:7..1:8) : <Tok_semi>
      [1:8..1:8) : <Tok_end_of_input>
    {|; : := :: :::|}
      [1:0..1:1) : <Tok_semi>
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:3) : <Tok_colon>
      [1:3..1:4) : <Tok_whitespace>
      [1:4..1:6) : <Tok_colon_eq>
      [1:6..1:7) : <Tok_whitespace>
      [1:7..1:9) : <Tok_cons>
      [1:9..1:10) : <Tok_whitespace>
      [1:10..1:12) : <Tok_cons>
      [1:12..1:13) : <Tok_colon>
      [1:13..1:13) : <Tok_end_of_input>
    {|]|]|[|[|}
      [1:0..1:1) : <Tok_rbrack>
      [1:1..1:3) : <Tok_rarray>
      [1:3..1:4) : <Tok_bar>
      [1:4..1:6) : <Tok_larray>
      [1:6..1:7) : <Tok_lbrack>
      [1:7..1:7) : <Tok_end_of_input>
    {|{}|}
      [1:0..1:1) : <Tok_lcurly>
      [1:1..1:2) : <Tok_rcurly>
      [1:2..1:2) : <Tok_end_of_input>
    {|\^&\&\|}
      [1:0..1:1) : <Tok_bslash>
      [1:1..1:2) : <Tok_caret>
      [1:2..1:3) : <Tok_amp>
      [1:3..1:4) : <Tok_bslash>
      [1:4..1:5) : <Tok_amp>
      [1:5..1:6) : <Tok_bslash>
      [1:6..1:6) : <Tok_end_of_input>
    {|- -> ->> --> ->|}
      [1:0..1:1) : <Tok_minus_op="-">
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:4) : <Tok_arrow>
      [1:4..1:5) : <Tok_whitespace>
      [1:5..1:8) : <Tok_minus_op="->>">
      [1:8..1:9) : <Tok_whitespace>
      [1:9..1:12) : <Tok_minus_op="-->">
      [1:12..1:13) : <Tok_whitespace>
      [1:13..1:15) : <Tok_arrow>
      [1:15..1:15) : <Tok_end_of_input>
    {|> >- >-> >->> >>-> >-> >e-> >{>e,mut}->|}
      [1:0..1:1) : <Tok_gt>
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:4) : <Tok_gt_op=">-">
      [1:4..1:5) : <Tok_whitespace>
      [1:5..1:8) : <Tok_parrow>
      [1:8..1:9) : <Tok_whitespace>
      [1:9..1:13) : <Tok_gt_op=">->>">
      [1:13..1:14) : <Tok_whitespace>
      [1:14..1:18) : <Tok_gt_op=">>->">
      [1:18..1:19) : <Tok_whitespace>
      [1:19..1:22) : <Tok_parrow>
      [1:22..1:23) : <Tok_whitespace>
      [1:23..1:24) : <Tok_gt>
      [1:24..1:25) : <Tok_uident="e">
      [1:25..1:27) : <Tok_arrow>
      [1:27..1:28) : <Tok_whitespace>
      [1:28..1:29) : <Tok_gt>
      [1:29..1:30) : <Tok_lcurly>
      [1:30..1:31) : <Tok_gt>
      [1:31..1:32) : <Tok_uident="e">
      [1:32..1:33) : <Tok_comma>
      [1:33..1:36) : <Tok_uident="mut">
      [1:36..1:37) : <Tok_rcurly>
      [1:37..1:39) : <Tok_arrow>
      [1:39..1:39) : <Tok_end_of_input>
    {|~f ~> >{mut}~> ~- ~-+*/%@!$<=>|:.~?|}
      [1:0..1:1) : <Tok_tilde>
      [1:1..1:2) : <Tok_uident="f">
      [1:2..1:3) : <Tok_whitespace>
      [1:3..1:5) : <Tok_earrow>
      [1:5..1:6) : <Tok_whitespace>
      [1:6..1:7) : <Tok_gt>
      [1:7..1:8) : <Tok_lcurly>
      [1:8..1:11) : <Tok_uident="mut">
      [1:11..1:12) : <Tok_rcurly>
      [1:12..1:14) : <Tok_earrow>
      [1:14..1:15) : <Tok_whitespace>
      [1:15..1:17) : <Tok_tilde_op="~-">
      [1:17..1:18) : <Tok_whitespace>
      [1:18..1:35) : <Tok_tilde_op="~-+*/%@!$<=>|:.~?">
      [1:35..1:35) : <Tok_end_of_input>
    {|?x ?? ?-+*/%@!$<=>|:.~?|}
      [1:0..1:1) : <Tok_qmark>
      [1:1..1:2) : <Tok_uident="x">
      [1:2..1:3) : <Tok_whitespace>
      [1:3..1:5) : <Tok_qmark_op="??">
      [1:5..1:6) : <Tok_whitespace>
      [1:6..1:23) : <Tok_qmark_op="?-+*/%@!$<=>|:.~?">
      [1:23..1:23) : <Tok_end_of_input>
    |xxx}]

let%expect_test "operators" =
  let open Format in
  printf "@[<h>";
  scan_str "~ ~-+*/%@!$<=>|:.~?";
  scan_str "? ?-+*/%@!$<=>|:.~?";
  scan_str "* *-+*/%@!$<=>|:.~?";
  scan_str "** **-+*/%@!$<=>|:.~?";
  scan_str "% %-+*/%@!$<=>|:.~?";
  scan_str "+ +-+*/%@!$<=>|:.~?";
  scan_str "- --+*/%@!$<=>|:.~?";
  scan_str "@ @-+*/%@!$<=>|:.~?";
  scan_str "! !-+*/%@!$<=>|:.~?";
  scan_str "$ $-+*/%@!$<=>|:.~?";
  scan_str "< <-+*/%@!$<=>|:.~?";
  scan_str "= =-+*/%@!$<=>|:.~?";
  scan_str "> >-+*/%@!$<=>|:.~?";
  scan_str "|-+*/%@!$<=>|:.~?";
  printf "@]";

  [%expect{xxx|
    {|~ ~-+*/%@!$<=>|:.~?|}
      [1:0..1:1) : <Tok_tilde>
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:19) : <Tok_tilde_op="~-+*/%@!$<=>|:.~?">
      [1:19..1:19) : <Tok_end_of_input>
    {|? ?-+*/%@!$<=>|:.~?|}
      [1:0..1:1) : <Tok_qmark>
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:19) : <Tok_qmark_op="?-+*/%@!$<=>|:.~?">
      [1:19..1:19) : <Tok_end_of_input>
    {|* *-+*/%@!$<=>|:.~?|}
      [1:0..1:1) : <Tok_star_op="*">
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:19) : <Tok_star_op="*-+*/%@!$<=>|:.~?">
      [1:19..1:19) : <Tok_end_of_input>
    {|** **-+*/%@!$<=>|:.~?|}
      [1:0..1:2) : <Tok_star_star_op="**">
      [1:2..1:3) : <Tok_whitespace>
      [1:3..1:21) : <Tok_star_star_op="**-+*/%@!$<=>|:.~?">
      [1:21..1:21) : <Tok_end_of_input>
    {|% %-+*/%@!$<=>|:.~?|}
      [1:0..1:1) : <Tok_pct_op="%">
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:19) : <Tok_pct_op="%-+*/%@!$<=>|:.~?">
      [1:19..1:19) : <Tok_end_of_input>
    {|+ +-+*/%@!$<=>|:.~?|}
      [1:0..1:1) : <Tok_plus_op="+">
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:19) : <Tok_plus_op="+-+*/%@!$<=>|:.~?">
      [1:19..1:19) : <Tok_end_of_input>
    {|- --+*/%@!$<=>|:.~?|}
      [1:0..1:1) : <Tok_minus_op="-">
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:19) : <Tok_minus_op="--+*/%@!$<=>|:.~?">
      [1:19..1:19) : <Tok_end_of_input>
    {|@ @-+*/%@!$<=>|:.~?|}
      [1:0..1:1) : <Tok_at_op="@">
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:19) : <Tok_at_op="@-+*/%@!$<=>|:.~?">
      [1:19..1:19) : <Tok_end_of_input>
    {|! !-+*/%@!$<=>|:.~?|}
      [1:0..1:1) : <Tok_error>
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:3) : <Tok_error>
      [1:3..1:19) : <Tok_minus_op="-+*/%@!$<=>|:.~?">
      [1:19..1:19) : <Tok_end_of_input>
    {|$ $-+*/%@!$<=>|:.~?|}
      [1:0..1:1) : <Tok_error>
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:3) : <Tok_error>
      [1:3..1:19) : <Tok_minus_op="-+*/%@!$<=>|:.~?">
      [1:19..1:19) : <Tok_end_of_input>
    {|< <-+*/%@!$<=>|:.~?|}
      [1:0..1:1) : <Tok_lt_op="<">
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:19) : <Tok_lt_op="<-+*/%@!$<=>|:.~?">
      [1:19..1:19) : <Tok_end_of_input>
    {|= =-+*/%@!$<=>|:.~?|}
      [1:0..1:1) : <Tok_eq_op="=">
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:19) : <Tok_eq_op="=-+*/%@!$<=>|:.~?">
      [1:19..1:19) : <Tok_end_of_input>
    {|> >-+*/%@!$<=>|:.~?|}
      [1:0..1:1) : <Tok_gt>
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:19) : <Tok_gt_op=">-+*/%@!$<=>|:.~?">
      [1:19..1:19) : <Tok_end_of_input>
    {||-+*/%@!$<=>|:.~?|}
      [1:0..1:17) : <Tok_bar_op="|-+*/%@!$<=>|:.~?">
      [1:17..1:17) : <Tok_end_of_input>
  |xxx}]

let%expect_test "uident" =
  let open Format in
  printf "@[<h>";
  scan_str "_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'";
  scan_str "a b c d e f g h i j k l m n o p q r s t u v w x y z _";
  scan_str "a as ass asse asser assert asserts";

  scan_str "and";
  scan_str "also";
  scan_str "as";
  scan_str "assert";
  scan_str "conceal";
  scan_str "do";
  scan_str "downto";
  scan_str "effect";
  scan_str "else";
  scan_str "expose";
  scan_str "external";
  scan_str "false";
  scan_str "for";
  scan_str "fun";
  scan_str "function";
  scan_str "functor";
  scan_str "if";
  scan_str "import";
  scan_str "in";
  scan_str "include";
  scan_str "lazy";
  scan_str "let";
  scan_str "match";
  scan_str "module";
  scan_str "of";
  scan_str "open";
  scan_str "or";
  scan_str "rec";
  scan_str "sig";
  scan_str "struct";
  scan_str "then";
  scan_str "to";
  scan_str "true";
  scan_str "type";
  scan_str "val";
  scan_str "when";
  scan_str "while";
  scan_str "with";
  printf "@]";

  [%expect{xxx|
    {|_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'|}
      [1:0..1:65) : <Tok_uident="_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'">
      [1:65..1:65) : <Tok_end_of_input>
    {|a b c d e f g h i j k l m n o p q r s t u v w x y z _|}
      [1:0..1:1) : <Tok_uident="a">
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:3) : <Tok_uident="b">
      [1:3..1:4) : <Tok_whitespace>
      [1:4..1:5) : <Tok_uident="c">
      [1:5..1:6) : <Tok_whitespace>
      [1:6..1:7) : <Tok_uident="d">
      [1:7..1:8) : <Tok_whitespace>
      [1:8..1:9) : <Tok_uident="e">
      [1:9..1:10) : <Tok_whitespace>
      [1:10..1:11) : <Tok_uident="f">
      [1:11..1:12) : <Tok_whitespace>
      [1:12..1:13) : <Tok_uident="g">
      [1:13..1:14) : <Tok_whitespace>
      [1:14..1:15) : <Tok_uident="h">
      [1:15..1:16) : <Tok_whitespace>
      [1:16..1:17) : <Tok_uident="i">
      [1:17..1:18) : <Tok_whitespace>
      [1:18..1:19) : <Tok_uident="j">
      [1:19..1:20) : <Tok_whitespace>
      [1:20..1:21) : <Tok_uident="k">
      [1:21..1:22) : <Tok_whitespace>
      [1:22..1:23) : <Tok_uident="l">
      [1:23..1:24) : <Tok_whitespace>
      [1:24..1:25) : <Tok_uident="m">
      [1:25..1:26) : <Tok_whitespace>
      [1:26..1:27) : <Tok_uident="n">
      [1:27..1:28) : <Tok_whitespace>
      [1:28..1:29) : <Tok_uident="o">
      [1:29..1:30) : <Tok_whitespace>
      [1:30..1:31) : <Tok_uident="p">
      [1:31..1:32) : <Tok_whitespace>
      [1:32..1:33) : <Tok_uident="q">
      [1:33..1:34) : <Tok_whitespace>
      [1:34..1:35) : <Tok_uident="r">
      [1:35..1:36) : <Tok_whitespace>
      [1:36..1:37) : <Tok_uident="s">
      [1:37..1:38) : <Tok_whitespace>
      [1:38..1:39) : <Tok_uident="t">
      [1:39..1:40) : <Tok_whitespace>
      [1:40..1:41) : <Tok_uident="u">
      [1:41..1:42) : <Tok_whitespace>
      [1:42..1:43) : <Tok_uident="v">
      [1:43..1:44) : <Tok_whitespace>
      [1:44..1:45) : <Tok_uident="w">
      [1:45..1:46) : <Tok_whitespace>
      [1:46..1:47) : <Tok_uident="x">
      [1:47..1:48) : <Tok_whitespace>
      [1:48..1:49) : <Tok_uident="y">
      [1:49..1:50) : <Tok_whitespace>
      [1:50..1:51) : <Tok_uident="z">
      [1:51..1:52) : <Tok_whitespace>
      [1:52..1:53) : <Tok_uident="_">
      [1:53..1:53) : <Tok_end_of_input>
    {|a as ass asse asser assert asserts|}
      [1:0..1:1) : <Tok_uident="a">
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:4) : <Tok_as>
      [1:4..1:5) : <Tok_whitespace>
      [1:5..1:8) : <Tok_uident="ass">
      [1:8..1:9) : <Tok_whitespace>
      [1:9..1:13) : <Tok_uident="asse">
      [1:13..1:14) : <Tok_whitespace>
      [1:14..1:19) : <Tok_uident="asser">
      [1:19..1:20) : <Tok_whitespace>
      [1:20..1:26) : <Tok_assert>
      [1:26..1:27) : <Tok_whitespace>
      [1:27..1:34) : <Tok_uident="asserts">
      [1:34..1:34) : <Tok_end_of_input>
    {|and|}
      [1:0..1:3) : <Tok_and>
      [1:3..1:3) : <Tok_end_of_input>
    {|also|}
      [1:0..1:4) : <Tok_also>
      [1:4..1:4) : <Tok_end_of_input>
    {|as|}
      [1:0..1:2) : <Tok_as>
      [1:2..1:2) : <Tok_end_of_input>
    {|assert|}
      [1:0..1:6) : <Tok_assert>
      [1:6..1:6) : <Tok_end_of_input>
    {|conceal|}
      [1:0..1:7) : <Tok_conceal>
      [1:7..1:7) : <Tok_end_of_input>
    {|do|}
      [1:0..1:2) : <Tok_do>
      [1:2..1:2) : <Tok_end_of_input>
    {|downto|}
      [1:0..1:6) : <Tok_downto>
      [1:6..1:6) : <Tok_end_of_input>
    {|effect|}
      [1:0..1:6) : <Tok_effect>
      [1:6..1:6) : <Tok_end_of_input>
    {|else|}
      [1:0..1:4) : <Tok_else>
      [1:4..1:4) : <Tok_end_of_input>
    {|expose|}
      [1:0..1:6) : <Tok_expose>
      [1:6..1:6) : <Tok_end_of_input>
    {|external|}
      [1:0..1:8) : <Tok_external>
      [1:8..1:8) : <Tok_end_of_input>
    {|false|}
      [1:0..1:5) : <Tok_false>
      [1:5..1:5) : <Tok_end_of_input>
    {|for|}
      [1:0..1:3) : <Tok_for>
      [1:3..1:3) : <Tok_end_of_input>
    {|fun|}
      [1:0..1:3) : <Tok_fun>
      [1:3..1:3) : <Tok_end_of_input>
    {|function|}
      [1:0..1:8) : <Tok_function>
      [1:8..1:8) : <Tok_end_of_input>
    {|functor|}
      [1:0..1:7) : <Tok_functor>
      [1:7..1:7) : <Tok_end_of_input>
    {|if|}
      [1:0..1:2) : <Tok_if>
      [1:2..1:2) : <Tok_end_of_input>
    {|import|}
      [1:0..1:6) : <Tok_import>
      [1:6..1:6) : <Tok_end_of_input>
    {|in|}
      [1:0..1:2) : <Tok_in>
      [1:2..1:2) : <Tok_end_of_input>
    {|include|}
      [1:0..1:7) : <Tok_include>
      [1:7..1:7) : <Tok_end_of_input>
    {|lazy|}
      [1:0..1:4) : <Tok_lazy>
      [1:4..1:4) : <Tok_end_of_input>
    {|let|}
      [1:0..1:3) : <Tok_let>
      [1:3..1:3) : <Tok_end_of_input>
    {|match|}
      [1:0..1:5) : <Tok_match>
      [1:5..1:5) : <Tok_end_of_input>
    {|module|}
      [1:0..1:6) : <Tok_module>
      [1:6..1:6) : <Tok_end_of_input>
    {|of|}
      [1:0..1:2) : <Tok_of>
      [1:2..1:2) : <Tok_end_of_input>
    {|open|}
      [1:0..1:4) : <Tok_open>
      [1:4..1:4) : <Tok_end_of_input>
    {|or|}
      [1:0..1:2) : <Tok_or>
      [1:2..1:2) : <Tok_end_of_input>
    {|rec|}
      [1:0..1:3) : <Tok_rec>
      [1:3..1:3) : <Tok_end_of_input>
    {|sig|}
      [1:0..1:3) : <Tok_sig>
      [1:3..1:3) : <Tok_end_of_input>
    {|struct|}
      [1:0..1:6) : <Tok_struct>
      [1:6..1:6) : <Tok_end_of_input>
    {|then|}
      [1:0..1:4) : <Tok_then>
      [1:4..1:4) : <Tok_end_of_input>
    {|to|}
      [1:0..1:2) : <Tok_to>
      [1:2..1:2) : <Tok_end_of_input>
    {|true|}
      [1:0..1:4) : <Tok_true>
      [1:4..1:4) : <Tok_end_of_input>
    {|type|}
      [1:0..1:4) : <Tok_type>
      [1:4..1:4) : <Tok_end_of_input>
    {|val|}
      [1:0..1:3) : <Tok_val>
      [1:3..1:3) : <Tok_end_of_input>
    {|when|}
      [1:0..1:4) : <Tok_when>
      [1:4..1:4) : <Tok_end_of_input>
    {|while|}
      [1:0..1:5) : <Tok_while>
      [1:5..1:5) : <Tok_end_of_input>
    {|with|}
      [1:0..1:4) : <Tok_with>
      [1:4..1:4) : <Tok_end_of_input>
  |xxx}]

let%expect_test "cident" =
  let open Format in
  printf "@[<h>";
  scan_str "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'";
  scan_str "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z";
  printf "@]";

  [%expect{xxx|
    {|ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'|}
      [1:0..1:64) : <Tok_cident="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'">
      [1:64..1:64) : <Tok_end_of_input>
    {|A B C D E F G H I J K L M N O P Q R S T U V W X Y Z|}
      [1:0..1:1) : <Tok_cident="A">
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:3) : <Tok_cident="B">
      [1:3..1:4) : <Tok_whitespace>
      [1:4..1:5) : <Tok_cident="C">
      [1:5..1:6) : <Tok_whitespace>
      [1:6..1:7) : <Tok_cident="D">
      [1:7..1:8) : <Tok_whitespace>
      [1:8..1:9) : <Tok_cident="E">
      [1:9..1:10) : <Tok_whitespace>
      [1:10..1:11) : <Tok_cident="F">
      [1:11..1:12) : <Tok_whitespace>
      [1:12..1:13) : <Tok_cident="G">
      [1:13..1:14) : <Tok_whitespace>
      [1:14..1:15) : <Tok_cident="H">
      [1:15..1:16) : <Tok_whitespace>
      [1:16..1:17) : <Tok_cident="I">
      [1:17..1:18) : <Tok_whitespace>
      [1:18..1:19) : <Tok_cident="J">
      [1:19..1:20) : <Tok_whitespace>
      [1:20..1:21) : <Tok_cident="K">
      [1:21..1:22) : <Tok_whitespace>
      [1:22..1:23) : <Tok_cident="L">
      [1:23..1:24) : <Tok_whitespace>
      [1:24..1:25) : <Tok_cident="M">
      [1:25..1:26) : <Tok_whitespace>
      [1:26..1:27) : <Tok_cident="N">
      [1:27..1:28) : <Tok_whitespace>
      [1:28..1:29) : <Tok_cident="O">
      [1:29..1:30) : <Tok_whitespace>
      [1:30..1:31) : <Tok_cident="P">
      [1:31..1:32) : <Tok_whitespace>
      [1:32..1:33) : <Tok_cident="Q">
      [1:33..1:34) : <Tok_whitespace>
      [1:34..1:35) : <Tok_cident="R">
      [1:35..1:36) : <Tok_whitespace>
      [1:36..1:37) : <Tok_cident="S">
      [1:37..1:38) : <Tok_whitespace>
      [1:38..1:39) : <Tok_cident="T">
      [1:39..1:40) : <Tok_whitespace>
      [1:40..1:41) : <Tok_cident="U">
      [1:41..1:42) : <Tok_whitespace>
      [1:42..1:43) : <Tok_cident="V">
      [1:43..1:44) : <Tok_whitespace>
      [1:44..1:45) : <Tok_cident="W">
      [1:45..1:46) : <Tok_whitespace>
      [1:46..1:47) : <Tok_cident="X">
      [1:47..1:48) : <Tok_whitespace>
      [1:48..1:49) : <Tok_cident="Y">
      [1:49..1:50) : <Tok_whitespace>
      [1:50..1:51) : <Tok_cident="Z">
      [1:51..1:51) : <Tok_end_of_input>
  |xxx}]

let%expect_test "codepoint" =
  let open Format in
  printf "@[<h>";
  scan_str "'a' '\n'";
  scan_str "'\\t' '\\n' '\\r' '\\'' '\\\\'";
  scan_str "'\\u{41}'";
  scan_str "'\\u{000_ff_fd}'";

  (* Type parameter sigils. *)
  scan_str "'";
  scan_str "' ";
  scan_str "'\n";
  scan_str "'\\\n";
  scan_str "' a";
  scan_str "'a";
  scan_str "'abcdefghijklmnopqrstuvwxyz_";
  scan_str "'aa'";

  scan_str "'\\u{0}x'";
  scan_str "'\\u{110ffff}'";
  scan_str "'\\u{110000}'";
  scan_str "'\\u{110000}'";
  scan_str "'\\u{d800}'"; (* Surrogate. *)
  scan_str "'\\u{0z1}'";
  scan_str "'\\u{x'";
  scan_str "'\\u{0}a'";
  scan_str "'\\u{0}";
  scan_str "'\\u{0'";
  scan_str "'\\u{'";
  scan_str "'\\u00'";
  scan_str "'\\u0'";
  scan_str "'\\u'";
  scan_str "'\\u";
  scan_str "'\\x'";
  scan_str "'\\";
  scan_str "'''";
  scan_str "''";
  printf "@]";

  [%expect{xxx|
    {|'a' '
    '|}
      [1:0..1:3) : <Tok_codepoint=Constant 'a'>
      [1:3..1:4) : <Tok_whitespace>
      [1:4..2:1) : <Tok_codepoint=Constant '\n'>
      [2:1..2:1) : <Tok_end_of_input>
    {|'\t' '\n' '\r' '\'' '\\'|}
      [1:0..1:4) : <Tok_codepoint=Constant '\t'>
      [1:4..1:5) : <Tok_whitespace>
      [1:5..1:9) : <Tok_codepoint=Constant '\n'>
      [1:9..1:10) : <Tok_whitespace>
      [1:10..1:14) : <Tok_codepoint=Constant '\r'>
      [1:14..1:15) : <Tok_whitespace>
      [1:15..1:19) : <Tok_codepoint=Constant '\''>
      [1:19..1:20) : <Tok_whitespace>
      [1:20..1:24) : <Tok_codepoint=Constant '\\'>
      [1:24..1:24) : <Tok_end_of_input>
    {|'\u{41}'|}
      [1:0..1:8) : <Tok_codepoint=Constant 'A'>
      [1:8..1:8) : <Tok_end_of_input>
    {|'\u{000_ff_fd}'|}
      [1:0..1:15) : <Tok_codepoint=Constant '�'>
      [1:15..1:15) : <Tok_end_of_input>
    {|'|}
      [1:0..1:1) : <Tok_tick>
      [1:1..1:1) : <Tok_end_of_input>
    {|' |}
      [1:0..1:1) : <Tok_tick>
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:2) : <Tok_end_of_input>
    {|'
    |}
      [1:0..1:1) : <Tok_tick>
      [1:1..2:0) : <Tok_whitespace>
      [2:0..2:0) : <Tok_line_delim>
      [2:0..2:0) : <Tok_end_of_input>
    {|'\
    |}
      [1:0..1:2) : <Tok_tick>
      [1:2..2:0) : <Tok_whitespace>
      [2:0..2:0) : <Tok_line_delim>
      [2:0..2:0) : <Tok_end_of_input>
    {|' a|}
      [1:0..1:1) : <Tok_tick>
      [1:1..1:2) : <Tok_whitespace>
      [1:2..1:3) : <Tok_uident="a">
      [1:3..1:3) : <Tok_end_of_input>
    {|'a|}
      [1:0..1:1) : <Tok_tick>
      [1:1..1:2) : <Tok_uident="a">
      [1:2..1:2) : <Tok_end_of_input>
    {|'abcdefghijklmnopqrstuvwxyz_|}
      [1:0..1:1) : <Tok_tick>
      [1:1..1:28) : <Tok_uident="abcdefghijklmnopqrstuvwxyz_">
      [1:28..1:28) : <Tok_end_of_input>
    {|'aa'|}
      [1:0..1:1) : <Tok_tick>
      [1:1..1:4) : <Tok_uident="aa'">
      [1:4..1:4) : <Tok_end_of_input>
    {|'\u{0}x'|}
      [1:0..1:8) : <Tok_codepoint=Malformed ["[1:6..1:7): Excess codepoint before terminator"]>
      [1:8..1:8) : <Tok_end_of_input>
    {|'\u{110ffff}'|}
      [1:0..1:13) : <Tok_codepoint=Malformed ["[1:1..1:12): Invalid Unicode value"]>
      [1:13..1:13) : <Tok_end_of_input>
    {|'\u{110000}'|}
      [1:0..1:12) : <Tok_codepoint=Malformed ["[1:1..1:11): Invalid Unicode value"]>
      [1:12..1:12) : <Tok_end_of_input>
    {|'\u{110000}'|}
      [1:0..1:12) : <Tok_codepoint=Malformed ["[1:1..1:11): Invalid Unicode value"]>
      [1:12..1:12) : <Tok_end_of_input>
    {|'\u{d800}'|}
      [1:0..1:10) : <Tok_codepoint=Malformed ["[1:1..1:9): Invalid Unicode value"]>
      [1:10..1:10) : <Tok_end_of_input>
    {|'\u{0z1}'|}
      [1:0..1:9) : <Tok_codepoint=Malformed ["[1:5..1:6): Invalid hexadecimal digit"]>
      [1:9..1:9) : <Tok_end_of_input>
    {|'\u{x'|}
      [1:0..1:6) : <Tok_codepoint=Malformed ["[1:1..1:5): Partial \u{...}"]>
      [1:6..1:6) : <Tok_end_of_input>
    {|'\u{0}a'|}
      [1:0..1:8) : <Tok_codepoint=Malformed ["[1:6..1:7): Excess codepoint before terminator"]>
      [1:8..1:8) : <Tok_end_of_input>
    {|'\u{0}|}
      [1:0..1:6) : <Tok_codepoint=Malformed ["[1:0..1:6): Unterminated codepoint literal"]>
      [1:6..1:6) : <Tok_end_of_input>
    {|'\u{0'|}
      [1:0..1:6) : <Tok_codepoint=Malformed ["[1:1..1:5): Partial \u{...}"]>
      [1:6..1:6) : <Tok_end_of_input>
    {|'\u{'|}
      [1:0..1:5) : <Tok_codepoint=Malformed ["[1:1..1:4): Partial \u{...}"]>
      [1:5..1:5) : <Tok_end_of_input>
    {|'\u00'|}
      [1:0..1:6) : <Tok_codepoint=Malformed ["[1:1..1:3): Illegal backslash escape"; "[1:3..1:4): Excess codepoint before terminator"; "[1:4..1:5): Excess codepoint before terminator"]>
      [1:6..1:6) : <Tok_end_of_input>
    {|'\u0'|}
      [1:0..1:5) : <Tok_codepoint=Malformed ["[1:1..1:3): Illegal backslash escape"; "[1:3..1:4): Excess codepoint before terminator"]>
      [1:5..1:5) : <Tok_end_of_input>
    {|'\u'|}
      [1:0..1:4) : <Tok_codepoint=Malformed ["[1:1..1:3): Illegal backslash escape"]>
      [1:4..1:4) : <Tok_end_of_input>
    {|'\u|}
      [1:0..1:3) : <Tok_codepoint=Malformed ["[1:0..1:3): Unterminated codepoint literal"]>
      [1:3..1:3) : <Tok_end_of_input>
    {|'\x'|}
      [1:0..1:4) : <Tok_codepoint=Malformed ["[1:1..1:3): Illegal backslash escape"]>
      [1:4..1:4) : <Tok_end_of_input>
    {|'\|}
      [1:0..1:2) : <Tok_codepoint=Malformed ["[1:0..1:2): Unterminated codepoint literal"]>
      [1:2..1:2) : <Tok_end_of_input>
    {|'''|}
      [1:0..1:2) : <Tok_codepoint=Malformed ["[1:0..1:2): Empty codepoint literal"]>
      [1:2..1:3) : <Tok_tick>
      [1:3..1:3) : <Tok_end_of_input>
    {|''|}
      [1:0..1:2) : <Tok_codepoint=Malformed ["[1:0..1:2): Empty codepoint literal"]>
      [1:2..1:2) : <Tok_end_of_input>
  |xxx}]

let%expect_test "istring" =
  let open Format in
  printf "@[<h>";
  scan_str {|""|};
  scan_str {|"
"|};
  scan_str {|"a \n \t \n \r \" \\ \u{41} \u{000_ff_fd}"|};

  scan_str {|"\u{110ffff}"|};
  scan_str {|"\u{110000}"|};
  scan_str {|"\u{110000}"|};
  scan_str {|"\u{d800}"|}; (* Surrogate. *)
  scan_str {|"\u{x"|};
  scan_str {|"\u{0"|};
  scan_str {|"\u{"|};
  scan_str {|"\u0"|};
  scan_str {|"\u"|};
  scan_str {|"\x"|};
  scan_str {|"""|};
  scan_str {|"|};
  scan_str {|"\u\v\w"|};
  printf "@]";

  [%expect{xxx|
    {|""|}
      [1:0..1:2) : <Tok_istring=Constant "">
      [1:2..1:2) : <Tok_end_of_input>
    {|"
    "|}
      [1:0..2:1) : <Tok_istring=Constant "\n">
      [2:1..2:1) : <Tok_end_of_input>
    {|"a \n \t \n \r \" \\ \u{41} \u{000_ff_fd}"|}
      [1:0..1:42) : <Tok_istring=Constant "a \n \t \n \r \" \\ A �">
      [1:42..1:42) : <Tok_end_of_input>
    {|"\u{110ffff}"|}
      [1:0..1:13) : <Tok_istring=Malformed ["[1:1..1:11): Invalid Unicode value"]>
      [1:13..1:13) : <Tok_end_of_input>
    {|"\u{110000}"|}
      [1:0..1:12) : <Tok_istring=Malformed ["[1:1..1:10): Invalid Unicode value"]>
      [1:12..1:12) : <Tok_end_of_input>
    {|"\u{110000}"|}
      [1:0..1:12) : <Tok_istring=Malformed ["[1:1..1:10): Invalid Unicode value"]>
      [1:12..1:12) : <Tok_end_of_input>
    {|"\u{d800}"|}
      [1:0..1:10) : <Tok_istring=Malformed ["[1:1..1:8): Invalid Unicode value"]>
      [1:10..1:10) : <Tok_end_of_input>
    {|"\u{x"|}
      [1:0..1:6) : <Tok_istring=Malformed ["[1:1..1:4): Partial \u{...}"]>
      [1:6..1:6) : <Tok_end_of_input>
    {|"\u{0"|}
      [1:0..1:6) : <Tok_istring=Malformed ["[1:1..1:5): Partial \u{...}"]>
      [1:6..1:6) : <Tok_end_of_input>
    {|"\u{"|}
      [1:0..1:5) : <Tok_istring=Malformed ["[1:1..1:4): Partial \u{...}"]>
      [1:5..1:5) : <Tok_end_of_input>
    {|"\u0"|}
      [1:0..1:5) : <Tok_istring=Malformed ["[1:1..1:3): Illegal backslash escape"]>
      [1:5..1:5) : <Tok_end_of_input>
    {|"\u"|}
      [1:0..1:4) : <Tok_istring=Malformed ["[1:1..1:3): Illegal backslash escape"]>
      [1:4..1:4) : <Tok_end_of_input>
    {|"\x"|}
      [1:0..1:4) : <Tok_istring=Malformed ["[1:1..1:3): Illegal backslash escape"]>
      [1:4..1:4) : <Tok_end_of_input>
    {|"""|}
      [1:0..1:2) : <Tok_istring=Constant "">
      [1:2..1:3) : <Tok_istring=Malformed ["[1:2..1:3): Unterminated string literal"]>
      [1:3..1:3) : <Tok_end_of_input>
    {|"|}
      [1:0..1:1) : <Tok_istring=Malformed ["[1:0..1:1): Unterminated string literal"]>
      [1:1..1:1) : <Tok_end_of_input>
    {|"\u\v\w"|}
      [1:0..1:8) : <Tok_istring=Malformed ["[1:1..1:3): Illegal backslash escape"; "[1:3..1:5): Illegal backslash escape"; "[1:5..1:7): Illegal backslash escape"]>
      [1:8..1:8) : <Tok_end_of_input>
  |xxx}]

let%expect_test "rstring" =
  let open Format in
  printf "@[<h>";
  scan_str {|""|};
  scan_str {|````|};
  scan_str {|``
``|};
  scan_str {|``

``|};
  scan_str {|``


``|};
  scan_str {|``a
b``|};
  scan_str {|``
a
b
``|};
  scan_str {|``a``|};
  scan_str {|`aoeu_``htns`gcrl`htns``aoeu_`|};
  scan_str {|``a\u{0}\t\n\r\"\\\
b``|};

  scan_str{|`|};
  scan_str{|``|};
  scan_str{|```|};
  scan_str{|`tag``tag|};
  scan_str{|`tag``ta|};
  scan_str{|`tag``|};
  scan_str{|`tag`|};
  scan_str{|`tag|};
  printf "@]";

  [%expect{xxx|
    {|""|}
      [1:0..1:2) : <Tok_istring=Constant "">
      [1:2..1:2) : <Tok_end_of_input>
    {|````|}
      [1:0..1:4) : <Tok_rstring=Constant "">
      [1:4..1:4) : <Tok_end_of_input>
    {|``
    ``|}
      [1:0..2:2) : <Tok_rstring=Constant "">
      [2:2..2:2) : <Tok_end_of_input>
    {|``

    ``|}
      [1:0..3:2) : <Tok_rstring=Constant "">
      [3:2..3:2) : <Tok_end_of_input>
    {|``


    ``|}
      [1:0..4:2) : <Tok_rstring=Constant "\n">
      [4:2..4:2) : <Tok_end_of_input>
    {|``a
    b``|}
      [1:0..2:3) : <Tok_rstring=Constant "a\nb">
      [2:3..2:3) : <Tok_end_of_input>
    {|``
    a
    b
    ``|}
      [1:0..4:2) : <Tok_rstring=Constant "a\nb">
      [4:2..4:2) : <Tok_end_of_input>
    {|``a``|}
      [1:0..1:5) : <Tok_rstring=Constant "a">
      [1:5..1:5) : <Tok_end_of_input>
    {|`aoeu_``htns`gcrl`htns``aoeu_`|}
      [1:0..1:30) : <Tok_rstring=Constant "`htns`gcrl`htns`">
      [1:30..1:30) : <Tok_end_of_input>
    {|``a\u{0}\t\n\r\"\\\
    b``|}
      [1:0..2:3) : <Tok_rstring=Constant "a\\u{0}\\t\\n\\r\\\"\\\\\\\nb">
      [2:3..2:3) : <Tok_end_of_input>
    {|`|}
      [1:0..1:1) : <Tok_rstring=Malformed ["[1:0..1:1): Unterminated string literal"]>
      [1:1..1:1) : <Tok_end_of_input>
    {|``|}
      [1:0..1:2) : <Tok_rstring=Malformed ["[1:0..1:2): Unterminated string literal"]>
      [1:2..1:2) : <Tok_end_of_input>
    {|```|}
      [1:0..1:3) : <Tok_rstring=Malformed ["[1:0..1:3): Unterminated string literal"]>
      [1:3..1:3) : <Tok_end_of_input>
    {|`tag``tag|}
      [1:0..1:9) : <Tok_rstring=Malformed ["[1:0..1:9): Unterminated string literal"]>
      [1:9..1:9) : <Tok_end_of_input>
    {|`tag``ta|}
      [1:0..1:8) : <Tok_rstring=Malformed ["[1:0..1:8): Unterminated string literal"]>
      [1:8..1:8) : <Tok_end_of_input>
    {|`tag``|}
      [1:0..1:6) : <Tok_rstring=Malformed ["[1:0..1:6): Unterminated string literal"]>
      [1:6..1:6) : <Tok_end_of_input>
    {|`tag`|}
      [1:0..1:5) : <Tok_rstring=Malformed ["[1:0..1:5): Unterminated string literal"]>
      [1:5..1:5) : <Tok_end_of_input>
    {|`tag|}
      [1:0..1:4) : <Tok_rstring=Malformed ["[1:0..1:4): Unterminated string literal"]>
      [1:4..1:4) : <Tok_end_of_input>
  |xxx}]

let%expect_test "bstring" =
  let open Format in
  printf "@[<h>";
  scan_str {|
`|
`|};

  scan_str {|
`|a
`|};

  scan_str {|
`|a
 |b
`|};

  scan_str {|
`|
 |a
 |b
 |
`|};

  scan_str {|`||};
  scan_str {|
`|
 |};
  scan_str {|
`|
 ||};

  scan_str {|
`|a
|b
 |c
  |d
`|};
  printf "@]";

  [%expect{xxx|
    {|
    `|
    `|}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..2:0) : <Tok_line_delim>
      [2:0..3:1) : <Tok_bstring=Constant "">
      [3:1..3:1) : <Tok_end_of_input>
    {|
    `|a
    `|}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..2:0) : <Tok_line_delim>
      [2:0..3:1) : <Tok_bstring=Constant "a">
      [3:1..3:1) : <Tok_end_of_input>
    {|
    `|a
     |b
    `|}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..2:0) : <Tok_line_delim>
      [2:0..4:1) : <Tok_bstring=Constant "a\nb">
      [4:1..4:1) : <Tok_end_of_input>
    {|
    `|
     |a
     |b
     |
    `|}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..2:0) : <Tok_line_delim>
      [2:0..6:1) : <Tok_bstring=Constant "\na\nb\n">
      [6:1..6:1) : <Tok_end_of_input>
    {|`||}
      [1:0..1:2) : <Tok_bstring=Malformed ["[1:0..1:2): Unterminated string literal"]>
      [1:2..1:2) : <Tok_end_of_input>
    {|
    `|
     |}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..2:0) : <Tok_line_delim>
      [2:0..3:1) : <Tok_bstring=Malformed ["[2:0..3:1): Unterminated string literal"]>
      [3:1..3:1) : <Tok_end_of_input>
    {|
    `|
     ||}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..2:0) : <Tok_line_delim>
      [2:0..3:2) : <Tok_bstring=Malformed ["[2:0..3:2): Unterminated string literal"]>
      [3:2..3:2) : <Tok_end_of_input>
    {|
    `|a
    |b
     |c
      |d
    `|}
      [1:0..2:0) : <Tok_whitespace>
      [2:0..2:0) : <Tok_line_delim>
      [2:0..6:1) : <Tok_bstring=Malformed ["[3:0..3:1): Invalid bar string indentation"; "[5:0..5:3): Invalid bar string indentation"]>
      [6:1..6:1) : <Tok_end_of_input>
  |xxx}]

let%expect_test "real" =
  let open Format in
  printf "@[<h>";

  scan_str "0.";
  scan_str "0.0";
  scan_str "00.0";
  scan_str "00.00";

  scan_str "0e0";
  scan_str "0e-0";
  scan_str "0e+0";
  scan_str "0e00";

  scan_str "0r";
  scan_str "0r64";
  scan_str "0r32";
  scan_str "1.0";
  scan_str "1_000_000.0";
  scan_str "42.e44";
  scan_str "42.3e-78";
  scan_str "1.5r32";
  scan_str "1.234_567_e_+89_r32";

  scan_str "0x0r";
  scan_str "0x1r";
  scan_str "0x3r";
  scan_str "0xffr";
  scan_str "0x1p42";
  scan_str "0x0.1p42";
  scan_str "0x0.01p42";
  scan_str "0x0.001p42";
  scan_str "0x0.0001p42";

  scan_str "0x0.1";
  scan_str "0x0.01";
  scan_str "0x00.001";
  scan_str "0x00.0001";

  scan_str "0b1.101p42";
  scan_str "0o7.406p42";
  scan_str "0x4.a3";
  scan_str "0x4a.3d2p+42";
  scan_str "0x4a.3d2p+42_r32";
  scan_str "0x4a.3d2p0";
  scan_str "0x4a.3d2p-42";

  scan_str "0x0p0";
  scan_str "0x0p-0";
  scan_str "0x0p1";

  (* Invalid. *)

  scan_str "0x1.z";
  scan_str "0x1px5";
  scan_str "0x1p0x5";
  scan_str "0x1p0x5y";
  scan_str "0r42";
  scan_str "0r032";
  scan_str "0r3x2";
  scan_str "0x1.zpxyr042";

  scan_str "0x3.f_ffff_ffff_ffff";
  scan_str "0x1p-1023";
  scan_str "0x1p1024";

  scan_str "0x3.f_ffff_e_r32";
  scan_str "0x1p-127_r32";
  scan_str "0x1p128_r32";

  printf "@]";

  [%expect{xxx|
    {|0.|}
      [1:0..1:2) : <Tok_r64=Constant 0x0p+0>
      [1:2..1:2) : <Tok_end_of_input>
    {|0.0|}
      [1:0..1:3) : <Tok_r64=Constant 0x0p+0>
      [1:3..1:3) : <Tok_end_of_input>
    {|00.0|}
      [1:0..1:4) : <Tok_r64=Constant 0x0p+0>
      [1:4..1:4) : <Tok_end_of_input>
    {|00.00|}
      [1:0..1:5) : <Tok_r64=Constant 0x0p+0>
      [1:5..1:5) : <Tok_end_of_input>
    {|0e0|}
      [1:0..1:3) : <Tok_r64=Constant 0x0p+0>
      [1:3..1:3) : <Tok_end_of_input>
    {|0e-0|}
      [1:0..1:4) : <Tok_r64=Constant 0x0p+0>
      [1:4..1:4) : <Tok_end_of_input>
    {|0e+0|}
      [1:0..1:4) : <Tok_r64=Constant 0x0p+0>
      [1:4..1:4) : <Tok_end_of_input>
    {|0e00|}
      [1:0..1:4) : <Tok_r64=Constant 0x0p+0>
      [1:4..1:4) : <Tok_end_of_input>
    {|0r|}
      [1:0..1:2) : <Tok_r64=Constant 0x0p+0>
      [1:2..1:2) : <Tok_end_of_input>
    {|0r64|}
      [1:0..1:4) : <Tok_r64=Constant 0x0p+0>
      [1:4..1:4) : <Tok_end_of_input>
    {|0r32|}
      [1:0..1:4) : <Tok_r32=Constant 0x0p+0>
      [1:4..1:4) : <Tok_end_of_input>
    {|1.0|}
      [1:0..1:3) : <Tok_r64=Constant 0x1p+0>
      [1:3..1:3) : <Tok_end_of_input>
    {|1_000_000.0|}
      [1:0..1:11) : <Tok_r64=Constant 0x1.e848p+19>
      [1:11..1:11) : <Tok_end_of_input>
    {|42.e44|}
      [1:0..1:6) : <Tok_r64=Constant 0x1.78ab455e05473p+151>
      [1:6..1:6) : <Tok_end_of_input>
    {|42.3e-78|}
      [1:0..1:8) : <Tok_r64=Constant 0x1.3978eb8908931p-254>
      [1:8..1:8) : <Tok_end_of_input>
    {|1.5r32|}
      [1:0..1:6) : <Tok_r32=Constant 0x1.8p+0>
      [1:6..1:6) : <Tok_end_of_input>
    {|1.234_567_e_+89_r32|}
      [1:0..1:19) : <Tok_r32=Constant 0x1.f07c18386f74ep+295>
      [1:19..1:19) : <Tok_end_of_input>
    {|0x0r|}
      [1:0..1:4) : <Tok_r64=Constant 0x0p+0>
      [1:4..1:4) : <Tok_end_of_input>
    {|0x1r|}
      [1:0..1:4) : <Tok_r64=Constant 0x1p+0>
      [1:4..1:4) : <Tok_end_of_input>
    {|0x3r|}
      [1:0..1:4) : <Tok_r64=Constant 0x1.8p+1>
      [1:4..1:4) : <Tok_end_of_input>
    {|0xffr|}
      [1:0..1:5) : <Tok_r64=Constant 0x1.fep+7>
      [1:5..1:5) : <Tok_end_of_input>
    {|0x1p42|}
      [1:0..1:6) : <Tok_r64=Constant 0x1p+42>
      [1:6..1:6) : <Tok_end_of_input>
    {|0x0.1p42|}
      [1:0..1:8) : <Tok_r64=Constant 0x1p+38>
      [1:8..1:8) : <Tok_end_of_input>
    {|0x0.01p42|}
      [1:0..1:9) : <Tok_r64=Constant 0x1p+34>
      [1:9..1:9) : <Tok_end_of_input>
    {|0x0.001p42|}
      [1:0..1:10) : <Tok_r64=Constant 0x1p+30>
      [1:10..1:10) : <Tok_end_of_input>
    {|0x0.0001p42|}
      [1:0..1:11) : <Tok_r64=Constant 0x1p+26>
      [1:11..1:11) : <Tok_end_of_input>
    {|0x0.1|}
      [1:0..1:5) : <Tok_r64=Constant 0x1p-4>
      [1:5..1:5) : <Tok_end_of_input>
    {|0x0.01|}
      [1:0..1:6) : <Tok_r64=Constant 0x1p-8>
      [1:6..1:6) : <Tok_end_of_input>
    {|0x00.001|}
      [1:0..1:8) : <Tok_r64=Constant 0x1p-12>
      [1:8..1:8) : <Tok_end_of_input>
    {|0x00.0001|}
      [1:0..1:9) : <Tok_r64=Constant 0x1p-16>
      [1:9..1:9) : <Tok_end_of_input>
    {|0b1.101p42|}
      [1:0..1:10) : <Tok_r64=Constant 0x1.ap+42>
      [1:10..1:10) : <Tok_end_of_input>
    {|0o7.406p42|}
      [1:0..1:10) : <Tok_r64=Constant 0x1.e0cp+44>
      [1:10..1:10) : <Tok_end_of_input>
    {|0x4.a3|}
      [1:0..1:6) : <Tok_r64=Constant 0x1.28cp+2>
      [1:6..1:6) : <Tok_end_of_input>
    {|0x4a.3d2p+42|}
      [1:0..1:12) : <Tok_r64=Constant 0x1.28f48p+48>
      [1:12..1:12) : <Tok_end_of_input>
    {|0x4a.3d2p+42_r32|}
      [1:0..1:16) : <Tok_r32=Constant 0x1.28f48p+48>
      [1:16..1:16) : <Tok_end_of_input>
    {|0x4a.3d2p0|}
      [1:0..1:10) : <Tok_r64=Constant 0x1.28f48p+6>
      [1:10..1:10) : <Tok_end_of_input>
    {|0x4a.3d2p-42|}
      [1:0..1:12) : <Tok_r64=Constant 0x1.28f48p-36>
      [1:12..1:12) : <Tok_end_of_input>
    {|0x0p0|}
      [1:0..1:5) : <Tok_r64=Constant 0x0p+0>
      [1:5..1:5) : <Tok_end_of_input>
    {|0x0p-0|}
      [1:0..1:6) : <Tok_r64=Constant 0x0p+0>
      [1:6..1:6) : <Tok_end_of_input>
    {|0x0p1|}
      [1:0..1:5) : <Tok_r64=Constant 0x0p+0>
      [1:5..1:5) : <Tok_end_of_input>
    {|0x1.z|}
      [1:0..1:5) : <Tok_r64=Malformed ["[1:4..1:5): Invalid codepoint in numerical constant"]>
      [1:5..1:5) : <Tok_end_of_input>
    {|0x1px5|}
      [1:0..1:6) : <Tok_r64=Malformed ["[1:4..1:5): Invalid codepoint in numerical constant"]>
      [1:6..1:6) : <Tok_end_of_input>
    {|0x1p0x5|}
      [1:0..1:7) : <Tok_r64=Malformed ["[1:5..1:6): Invalid codepoint in numerical constant"]>
      [1:7..1:7) : <Tok_end_of_input>
    {|0x1p0x5y|}
      [1:0..1:8) : <Tok_r64=Malformed ["[1:5..1:6): Invalid codepoint in numerical constant"; "[1:7..1:8): Invalid codepoint in numerical constant"]>
      [1:8..1:8) : <Tok_end_of_input>
    {|0r42|}
      [1:0..1:4) : <Tok_r64=Malformed ["[1:1..1:4): Unsupported bitwidth in numerical constant"]>
      [1:4..1:4) : <Tok_end_of_input>
    {|0r032|}
      [1:0..1:5) : <Tok_r32=Malformed ["[1:2..1:3): Leading zero in numerical constant type suffix"]>
      [1:5..1:5) : <Tok_end_of_input>
    {|0r3x2|}
      [1:0..1:5) : <Tok_r32=Malformed ["[1:3..1:4): Invalid codepoint in numerical constant"]>
      [1:5..1:5) : <Tok_end_of_input>
    {|0x1.zpxyr042|}
      [1:0..1:12) : <Tok_r64=Malformed ["[1:4..1:5): Invalid codepoint in numerical constant"; "[1:6..1:7): Invalid codepoint in numerical constant"; "[1:7..1:8): Invalid codepoint in numerical constant"; "[1:9..1:10): Leading zero in numerical constant type suffix"; "[1:8..1:12): Unsupported bitwidth in numerical constant"]>
      [1:12..1:12) : <Tok_end_of_input>
    {|0x3.f_ffff_ffff_ffff|}
      [1:0..1:20) : <Tok_r64=Malformed ["[1:0..1:20): Numerical constant cannot be precisely represented"]>
      [1:20..1:20) : <Tok_end_of_input>
    {|0x1p-1023|}
      [1:0..1:9) : <Tok_r64=Malformed ["[1:0..1:9): Numerical constant cannot be precisely represented"]>
      [1:9..1:9) : <Tok_end_of_input>
    {|0x1p1024|}
      [1:0..1:8) : <Tok_r64=Malformed ["[1:0..1:8): Numerical constant cannot be precisely represented"]>
      [1:8..1:8) : <Tok_end_of_input>
    {|0x3.f_ffff_e_r32|}
      [1:0..1:16) : <Tok_r32=Malformed ["[1:0..1:16): Numerical constant cannot be precisely represented"]>
      [1:16..1:16) : <Tok_end_of_input>
    {|0x1p-127_r32|}
      [1:0..1:12) : <Tok_r32=Malformed ["[1:0..1:12): Numerical constant cannot be precisely represented"]>
      [1:12..1:12) : <Tok_end_of_input>
    {|0x1p128_r32|}
      [1:0..1:11) : <Tok_r32=Malformed ["[1:0..1:11): Numerical constant cannot be precisely represented"]>
      [1:11..1:11) : <Tok_end_of_input>
  |xxx}]

let%expect_test "integer" =
  let open Format in
  printf "@[<h>";

  (* Valid. *)
  scan_str "0";
  scan_str "00_";
  scan_str "01234567890_";
  scan_str "11";
  scan_str "22";
  scan_str "33";
  scan_str "44";
  scan_str "55";
  scan_str "66";
  scan_str "77";
  scan_str "88";
  scan_str "99";

  scan_str "0b01_01";
  scan_str "0o0123_4567";
  scan_str "0x0123_4567_89ab_cdef";

  scan_str "0u";
  scan_str "0i";
  scan_str "0u8";
  scan_str "0i8";

  scan_str "42u";
  scan_str "42i";
  scan_str "42u8";
  scan_str "42i8";

  (* Errors. *)
  scan_str "0a";
  scan_str "0AB42CD77";
  scan_str "0AB42CD77i";
  scan_str "0b2";
  scan_str "0o8";
  scan_str "0xg";

  scan_str "0u7";

  scan_str "0xffu8 0x100u8";
  scan_str "0x80i8 0x81i8";

  (* Miscellaneous. *)
  scan_str "0";
  scan_str "1";
  scan_str "-1";
  scan_str "42";

  scan_str "0b1010_1011u8";
  scan_str "0o253u8";
  scan_str "0xabu8";

  scan_str "15u";
  scan_str "17u64";
  scan_str "0x0123_4567_89ab_cdef";
  scan_str "0o660";
  scan_str "0b10_0001";
  scan_str "0b0100_0001";
  scan_str "1_000_000";
  scan_str "0x___1_fffd";
  scan_str "17i64";
  scan_str "0x_ab__c_i";
  scan_str "0o777";

  printf "@]";

  [%expect{xxx|
    {|0|}
      [1:0..1:1) : <Tok_u64=Constant 0u64>
      [1:1..1:1) : <Tok_end_of_input>
    {|00_|}
      [1:0..1:3) : <Tok_u64=Constant 0u64>
      [1:3..1:3) : <Tok_end_of_input>
    {|01234567890_|}
      [1:0..1:12) : <Tok_u64=Constant 1234567890u64>
      [1:12..1:12) : <Tok_end_of_input>
    {|11|}
      [1:0..1:2) : <Tok_u64=Constant 11u64>
      [1:2..1:2) : <Tok_end_of_input>
    {|22|}
      [1:0..1:2) : <Tok_u64=Constant 22u64>
      [1:2..1:2) : <Tok_end_of_input>
    {|33|}
      [1:0..1:2) : <Tok_u64=Constant 33u64>
      [1:2..1:2) : <Tok_end_of_input>
    {|44|}
      [1:0..1:2) : <Tok_u64=Constant 44u64>
      [1:2..1:2) : <Tok_end_of_input>
    {|55|}
      [1:0..1:2) : <Tok_u64=Constant 55u64>
      [1:2..1:2) : <Tok_end_of_input>
    {|66|}
      [1:0..1:2) : <Tok_u64=Constant 66u64>
      [1:2..1:2) : <Tok_end_of_input>
    {|77|}
      [1:0..1:2) : <Tok_u64=Constant 77u64>
      [1:2..1:2) : <Tok_end_of_input>
    {|88|}
      [1:0..1:2) : <Tok_u64=Constant 88u64>
      [1:2..1:2) : <Tok_end_of_input>
    {|99|}
      [1:0..1:2) : <Tok_u64=Constant 99u64>
      [1:2..1:2) : <Tok_end_of_input>
    {|0b01_01|}
      [1:0..1:7) : <Tok_u64=Constant 5u64>
      [1:7..1:7) : <Tok_end_of_input>
    {|0o0123_4567|}
      [1:0..1:11) : <Tok_u64=Constant 342391u64>
      [1:11..1:11) : <Tok_end_of_input>
    {|0x0123_4567_89ab_cdef|}
      [1:0..1:21) : <Tok_u64=Constant 81985529216486895u64>
      [1:21..1:21) : <Tok_end_of_input>
    {|0u|}
      [1:0..1:2) : <Tok_u64=Constant 0u64>
      [1:2..1:2) : <Tok_end_of_input>
    {|0i|}
      [1:0..1:2) : <Tok_i64=Constant 0i64>
      [1:2..1:2) : <Tok_end_of_input>
    {|0u8|}
      [1:0..1:3) : <Tok_u8=Constant 0u8>
      [1:3..1:3) : <Tok_end_of_input>
    {|0i8|}
      [1:0..1:3) : <Tok_i8=Constant 0i8>
      [1:3..1:3) : <Tok_end_of_input>
    {|42u|}
      [1:0..1:3) : <Tok_u64=Constant 42u64>
      [1:3..1:3) : <Tok_end_of_input>
    {|42i|}
      [1:0..1:3) : <Tok_i64=Constant 42i64>
      [1:3..1:3) : <Tok_end_of_input>
    {|42u8|}
      [1:0..1:4) : <Tok_u8=Constant 42u8>
      [1:4..1:4) : <Tok_end_of_input>
    {|42i8|}
      [1:0..1:4) : <Tok_i8=Constant 42i8>
      [1:4..1:4) : <Tok_end_of_input>
    {|0a|}
      [1:0..1:2) : <Tok_u64=Malformed ["[1:1..1:2): Invalid codepoint in numerical constant"]>
      [1:2..1:2) : <Tok_end_of_input>
    {|0AB42CD77|}
      [1:0..1:9) : <Tok_u64=Malformed ["[1:1..1:2): Invalid codepoint in numerical constant"; "[1:2..1:3): Invalid codepoint in numerical constant"; "[1:5..1:6): Invalid codepoint in numerical constant"; "[1:6..1:7): Invalid codepoint in numerical constant"]>
      [1:9..1:9) : <Tok_end_of_input>
    {|0AB42CD77i|}
      [1:0..1:10) : <Tok_i64=Malformed ["[1:1..1:2): Invalid codepoint in numerical constant"; "[1:2..1:3): Invalid codepoint in numerical constant"; "[1:5..1:6): Invalid codepoint in numerical constant"; "[1:6..1:7): Invalid codepoint in numerical constant"]>
      [1:10..1:10) : <Tok_end_of_input>
    {|0b2|}
      [1:0..1:3) : <Tok_u64=Malformed ["[1:2..1:3): Invalid codepoint in numerical constant"]>
      [1:3..1:3) : <Tok_end_of_input>
    {|0o8|}
      [1:0..1:3) : <Tok_u64=Malformed ["[1:2..1:3): Invalid codepoint in numerical constant"]>
      [1:3..1:3) : <Tok_end_of_input>
    {|0xg|}
      [1:0..1:3) : <Tok_u64=Malformed ["[1:2..1:3): Invalid codepoint in numerical constant"]>
      [1:3..1:3) : <Tok_end_of_input>
    {|0u7|}
      [1:0..1:3) : <Tok_u64=Malformed ["[1:1..1:3): Unsupported bitwidth in numerical constant"]>
      [1:3..1:3) : <Tok_end_of_input>
    {|0xffu8 0x100u8|}
      [1:0..1:6) : <Tok_u8=Constant 255u8>
      [1:6..1:7) : <Tok_whitespace>
      [1:7..1:14) : <Tok_u8=Malformed ["[1:7..1:14): Numerical constant exceeds 0x0000_0000_0000_00ff"]>
      [1:14..1:14) : <Tok_end_of_input>
    {|0x80i8 0x81i8|}
      [1:0..1:6) : <Tok_i8=Constant -128i8>
      [1:6..1:7) : <Tok_whitespace>
      [1:7..1:13) : <Tok_i8=Malformed ["[1:7..1:13): Numerical constant exceeds 0x0000_0000_0000_0080"]>
      [1:13..1:13) : <Tok_end_of_input>
    {|0|}
      [1:0..1:1) : <Tok_u64=Constant 0u64>
      [1:1..1:1) : <Tok_end_of_input>
    {|1|}
      [1:0..1:1) : <Tok_u64=Constant 1u64>
      [1:1..1:1) : <Tok_end_of_input>
    {|-1|}
      [1:0..1:1) : <Tok_minus_op="-">
      [1:1..1:2) : <Tok_u64=Constant 1u64>
      [1:2..1:2) : <Tok_end_of_input>
    {|42|}
      [1:0..1:2) : <Tok_u64=Constant 42u64>
      [1:2..1:2) : <Tok_end_of_input>
    {|0b1010_1011u8|}
      [1:0..1:13) : <Tok_u8=Constant 171u8>
      [1:13..1:13) : <Tok_end_of_input>
    {|0o253u8|}
      [1:0..1:7) : <Tok_u8=Constant 171u8>
      [1:7..1:7) : <Tok_end_of_input>
    {|0xabu8|}
      [1:0..1:6) : <Tok_u8=Constant 171u8>
      [1:6..1:6) : <Tok_end_of_input>
    {|15u|}
      [1:0..1:3) : <Tok_u64=Constant 15u64>
      [1:3..1:3) : <Tok_end_of_input>
    {|17u64|}
      [1:0..1:5) : <Tok_u64=Constant 17u64>
      [1:5..1:5) : <Tok_end_of_input>
    {|0x0123_4567_89ab_cdef|}
      [1:0..1:21) : <Tok_u64=Constant 81985529216486895u64>
      [1:21..1:21) : <Tok_end_of_input>
    {|0o660|}
      [1:0..1:5) : <Tok_u64=Constant 432u64>
      [1:5..1:5) : <Tok_end_of_input>
    {|0b10_0001|}
      [1:0..1:9) : <Tok_u64=Constant 33u64>
      [1:9..1:9) : <Tok_end_of_input>
    {|0b0100_0001|}
      [1:0..1:11) : <Tok_u64=Constant 65u64>
      [1:11..1:11) : <Tok_end_of_input>
    {|1_000_000|}
      [1:0..1:9) : <Tok_u64=Constant 1000000u64>
      [1:9..1:9) : <Tok_end_of_input>
    {|0x___1_fffd|}
      [1:0..1:11) : <Tok_u64=Constant 131069u64>
      [1:11..1:11) : <Tok_end_of_input>
    {|17i64|}
      [1:0..1:5) : <Tok_i64=Constant 17i64>
      [1:5..1:5) : <Tok_end_of_input>
    {|0x_ab__c_i|}
      [1:0..1:10) : <Tok_i64=Constant 2748i64>
      [1:10..1:10) : <Tok_end_of_input>
    {|0o777|}
      [1:0..1:5) : <Tok_u64=Constant 511u64>
      [1:5..1:5) : <Tok_end_of_input>
  |xxx}]
