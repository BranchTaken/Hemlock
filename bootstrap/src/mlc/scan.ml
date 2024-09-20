(* The OCaml scanner is a minimum-viable thin wrapper around the Hemlock scanner. The Hemlock
 * scanner directly supports a few OCaml-specific tokens:
 *
 * - `42L` "long" integers
 * - `&&` logical and operators
 * - `{|...|}` quoted strings
 *
 * The wrapper performs several transformations:
 *
 * - Dentation-related tokens are filtered out. This causes no issues with regard to whitespace
 *   because dentation tokens are always zero-length.
 * - Hemlock-specific keywords are converted to uident.
 * - OCaml-specific keywords are converted from uident. Keywords which are unused (e.g. objects,
 *   exceptions, bitwise operators) are omitted, since no effort is made to parse such code anyway.
 * - The `||` bar operator is converted to logical or.
 * - Hemlock-specific tokens with no OCaml equivalent are converted to error tokens.
 * - Codepoints are converted to chars, malformed if out of char range. *)

open Basis
open Basis.Rudiments

module Source = Hmc.Source

module Token = struct
  module Rendition = Hmc.Scan.Token.Rendition

  type t =
    (* Keywords. *)
    | Tok_and of {source: Source.Slice.t}
    | Tok_as of {source: Source.Slice.t}
    | Tok_begin of {source: Source.Slice.t}
    | Tok_else of {source: Source.Slice.t}
    | Tok_end of {source: Source.Slice.t}
    | Tok_external of {source: Source.Slice.t}
    | Tok_false of {source: Source.Slice.t}
    | Tok_fun of {source: Source.Slice.t}
    | Tok_function of {source: Source.Slice.t}
    | Tok_if of {source: Source.Slice.t}
    | Tok_import of {source: Source.Slice.t}
    | Tok_in of {source: Source.Slice.t}
    | Tok_include of {source: Source.Slice.t}
    | Tok_lazy of {source: Source.Slice.t}
    | Tok_let of {source: Source.Slice.t}
    | Tok_match of {source: Source.Slice.t}
    | Tok_mod of {source: Source.Slice.t}
    | Tok_module of {source: Source.Slice.t}
    | Tok_mutable of {source: Source.Slice.t}
    | Tok_nonrec of {source: Source.Slice.t}
    | Tok_of of {source: Source.Slice.t}
    | Tok_open of {source: Source.Slice.t}
    | Tok_or of {source: Source.Slice.t}
    | Tok_rec of {source: Source.Slice.t}
    | Tok_sig of {source: Source.Slice.t}
    | Tok_struct of {source: Source.Slice.t}
    | Tok_then of {source: Source.Slice.t}
    | Tok_true of {source: Source.Slice.t}
    | Tok_type of {source: Source.Slice.t}
    | Tok_val of {source: Source.Slice.t}
    | Tok_when of {source: Source.Slice.t}
    | Tok_with of {source: Source.Slice.t}

    (* Operators. *)
    | Tok_tilde_op of {source: Source.Slice.t; tilde_op: string}
    | Tok_qmark_op of {source: Source.Slice.t; qmark_op: string}
    | Tok_star_star_op of {source: Source.Slice.t; star_star_op: string}
    | Tok_star_op of {source: Source.Slice.t; star_op: string}
    | Tok_slash_op of {source: Source.Slice.t; slash_op: string}
    | Tok_pct_op of {source: Source.Slice.t; pct_op: string}
    | Tok_plus_op of {source: Source.Slice.t; plus_op: string}
    | Tok_minus_op of {source: Source.Slice.t; minus_op: string}
    | Tok_at_op of {source: Source.Slice.t; at_op: string}
    | Tok_caret_op of {source: Source.Slice.t; caret_op: string}
    | Tok_dollar_op of {source: Source.Slice.t; dollar_op: string}
    | Tok_lt_op of {source: Source.Slice.t; lt_op: string}
    | Tok_eq_op of {source: Source.Slice.t; eq_op: string}
    | Tok_gt_op of {source: Source.Slice.t; gt_op: string}
    | Tok_bar_op of {source: Source.Slice.t; bar_op: string}
    | Tok_colon_op of {source: Source.Slice.t; colon_op: string}
    | Tok_dot_op of {source: Source.Slice.t; dot_op: string}

    (* Punctuation. *)
    | Tok_tilde of {source: Source.Slice.t}
    | Tok_qmark of {source: Source.Slice.t}
    | Tok_minus of {source: Source.Slice.t}
    | Tok_lt of {source: Source.Slice.t}
    | Tok_lt_eq of {source: Source.Slice.t}
    | Tok_eq of {source: Source.Slice.t}
    | Tok_lt_gt of {source: Source.Slice.t}
    | Tok_gt_eq of {source: Source.Slice.t}
    | Tok_gt of {source: Source.Slice.t}
    | Tok_comma of {source: Source.Slice.t}
    | Tok_dot of {source: Source.Slice.t}
    | Tok_dot_dot of {source: Source.Slice.t}
    | Tok_semi of {source: Source.Slice.t}
    | Tok_colon of {source: Source.Slice.t}
    | Tok_colon_colon of {source: Source.Slice.t}
    | Tok_colon_eq of {source: Source.Slice.t}
    | Tok_lparen of {source: Source.Slice.t}
    | Tok_rparen of {source: Source.Slice.t}
    | Tok_lbrack of {source: Source.Slice.t}
    | Tok_rbrack of {source: Source.Slice.t}
    | Tok_lcurly of {source: Source.Slice.t}
    | Tok_rcurly of {source: Source.Slice.t}
    | Tok_bar of {source: Source.Slice.t}
    | Tok_bar_bar of {source: Source.Slice.t}
    | Tok_larray of {source: Source.Slice.t}
    | Tok_rarray of {source: Source.Slice.t}
    | Tok_bslash of {source: Source.Slice.t}
    | Tok_tick of {source: Source.Slice.t}
    | Tok_caret of {source: Source.Slice.t}
    | Tok_amp_amp of {source: Source.Slice.t}
    | Tok_xmark of {source: Source.Slice.t}
    | Tok_arrow of {source: Source.Slice.t}

    (* Miscellaneous. *)
    | Tok_whitespace of {source: Source.Slice.t}
    | Tok_paren_comment of {source: Source.Slice.t; paren_comment: unit Rendition.t}
    | Tok_uscore of {source: Source.Slice.t}
    | Tok_uident of {source: Source.Slice.t; uident: string Rendition.t}
    | Tok_cident of {source: Source.Slice.t; cident: string}
    | Tok_char of {source: Source.Slice.t; char: codepoint Rendition.t}
    | Tok_qstring of {source: Source.Slice.t; qstring: string Rendition.t}
    | Tok_istring of {source: Source.Slice.t; istring: string Rendition.t}
    | Tok_r64 of {source: Source.Slice.t; r64: real Rendition.t}
    | Tok_long of {source: Source.Slice.t; long: u64 Rendition.t}
    | Tok_end_of_input of {source: Source.Slice.t}
    | Tok_error of {source: Source.Slice.t; error: Rendition.Malformation.t list}

  let pp t formatter =
    formatter
    |> Fmt.fmt "("
    |> (fun formatter ->
      match t with
      (* Keywords. *)
      | Tok_and {source} ->
        formatter |> Fmt.fmt "Tok_and {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_as {source} ->
        formatter |> Fmt.fmt "Tok_as {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_begin {source} ->
        formatter |> Fmt.fmt "Tok_begin {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_else {source} ->
        formatter |> Fmt.fmt "Tok_else {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_end {source} ->
        formatter |> Fmt.fmt "Tok_end {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_external {source} ->
        formatter |> Fmt.fmt "Tok_external {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_false {source} ->
        formatter |> Fmt.fmt "Tok_false {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_fun {source} ->
        formatter |> Fmt.fmt "Tok_fun {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_function {source} ->
        formatter |> Fmt.fmt "Tok_function {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_if {source} ->
        formatter |> Fmt.fmt "Tok_if {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_import {source} ->
        formatter |> Fmt.fmt "Tok_import {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_in {source} ->
        formatter |> Fmt.fmt "Tok_in {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_include {source} ->
        formatter |> Fmt.fmt "Tok_include {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_lazy {source} ->
        formatter |> Fmt.fmt "Tok_lazy {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_let {source} ->
        formatter |> Fmt.fmt "Tok_let {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_match {source} ->
        formatter |> Fmt.fmt "Tok_match {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_mod {source} ->
        formatter |> Fmt.fmt "Tok_mod {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_module {source} ->
        formatter |> Fmt.fmt "Tok_module {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_mutable {source} ->
        formatter |> Fmt.fmt "Tok_mutable {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_nonrec {source} ->
        formatter |> Fmt.fmt "Tok_nonrec {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_of {source} ->
        formatter |> Fmt.fmt "Tok_of {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_open {source} ->
        formatter |> Fmt.fmt "Tok_open {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_or {source} ->
        formatter |> Fmt.fmt "Tok_or {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_rec {source} ->
        formatter |> Fmt.fmt "Tok_rec {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_sig {source} ->
        formatter |> Fmt.fmt "Tok_sig {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_struct {source} ->
        formatter |> Fmt.fmt "Tok_struct {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_then {source} ->
        formatter |> Fmt.fmt "Tok_then {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_true {source} ->
        formatter |> Fmt.fmt "Tok_true {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_type {source} ->
        formatter |> Fmt.fmt "Tok_type {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_val {source} ->
        formatter |> Fmt.fmt "Tok_val {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_when {source} ->
        formatter |> Fmt.fmt "Tok_when {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_with {source} ->
        formatter |> Fmt.fmt "Tok_with {source=" |> Source.Slice.pp source |> Fmt.fmt "}"

      (* Operators. *)
      | Tok_tilde_op {source; tilde_op} -> begin
          formatter
          |> Fmt.fmt "Tok_tilde_op {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; tilde_op="
          |> String.pp tilde_op
          |> Fmt.fmt "}"
        end
      | Tok_qmark_op {source; qmark_op} -> begin
          formatter
          |> Fmt.fmt "Tok_qmark_op {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; qmark_op="
          |> String.pp qmark_op
          |> Fmt.fmt "}"
        end
      | Tok_star_star_op {source; star_star_op} -> begin
          formatter
          |> Fmt.fmt "Tok_star_star_op {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; star_star_op="
          |> String.pp star_star_op
          |> Fmt.fmt "}"
        end
      | Tok_star_op {source; star_op} -> begin
          formatter
          |> Fmt.fmt "Tok_star_op {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; star_op="
          |> String.pp star_op
          |> Fmt.fmt "}"
        end
      | Tok_slash_op {source; slash_op} -> begin
          formatter
          |> Fmt.fmt "Tok_slash_op {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; slash_op="
          |> String.pp slash_op
          |> Fmt.fmt "}"
        end
      | Tok_pct_op {source; pct_op} -> begin
          formatter
          |> Fmt.fmt "Tok_pct_op {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; pct_op="
          |> String.pp pct_op
          |> Fmt.fmt "}"
        end
      | Tok_plus_op {source; plus_op} -> begin
          formatter
          |> Fmt.fmt "Tok_plus_op {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; plus_op="
          |> String.pp plus_op
          |> Fmt.fmt "}"
        end
      | Tok_minus_op {source; minus_op} -> begin
          formatter
          |> Fmt.fmt "Tok_minus_op {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; minus_op="
          |> String.pp minus_op
          |> Fmt.fmt "}"
        end
      | Tok_at_op {source; at_op} -> begin
          formatter
          |> Fmt.fmt "Tok_at_op {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; at_op="
          |> String.pp at_op
          |> Fmt.fmt "}"
        end
      | Tok_caret_op {source; caret_op} -> begin
          formatter
          |> Fmt.fmt "Tok_caret_op {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; caret_op="
          |> String.pp caret_op
          |> Fmt.fmt "}"
        end
      | Tok_dollar_op {source; dollar_op} -> begin
          formatter
          |> Fmt.fmt "Tok_dollar_op {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; dollar_op="
          |> String.pp dollar_op
          |> Fmt.fmt "}"
        end
      | Tok_lt_op {source; lt_op} -> begin
          formatter
          |> Fmt.fmt "Tok_lt_op {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; lt_op="
          |> String.pp lt_op
          |> Fmt.fmt "}"
        end
      | Tok_eq_op {source; eq_op} -> begin
          formatter
          |> Fmt.fmt "Tok_eq_op {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; eq_op="
          |> String.pp eq_op
          |> Fmt.fmt "}"
        end
      | Tok_gt_op {source; gt_op} -> begin
          formatter
          |> Fmt.fmt "Tok_gt_op {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; gt_op="
          |> String.pp gt_op
          |> Fmt.fmt "}"
        end
      | Tok_bar_op {source; bar_op} -> begin
          formatter
          |> Fmt.fmt "Tok_bar_op {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; bar_op="
          |> String.pp bar_op
          |> Fmt.fmt "}"
        end
      | Tok_colon_op {source; colon_op} -> begin
          formatter
          |> Fmt.fmt "Tok_colon_op {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; colon_op="
          |> String.pp colon_op
          |> Fmt.fmt "}"
        end
      | Tok_dot_op {source; dot_op} -> begin
          formatter
          |> Fmt.fmt "Tok_dot_op {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; dot_op="
          |> String.pp dot_op
          |> Fmt.fmt "}"
        end

      (* Punctuation. *)
      | Tok_tilde {source} ->
        formatter |> Fmt.fmt "Tok_tilde {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_qmark {source} ->
        formatter |> Fmt.fmt "Tok_qmark {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_minus {source} ->
        formatter |> Fmt.fmt "Tok_minus {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_lt {source} ->
        formatter |> Fmt.fmt "Tok_lt {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_lt_eq {source} ->
        formatter |> Fmt.fmt "Tok_lt_eq {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_eq {source} ->
        formatter |> Fmt.fmt "Tok_eq {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_lt_gt {source} ->
        formatter |> Fmt.fmt "Tok_lt_gt {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_gt_eq {source} ->
        formatter |> Fmt.fmt "Tok_gt_eq {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_gt {source} ->
        formatter |> Fmt.fmt "Tok_gt {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_comma {source} ->
        formatter |> Fmt.fmt "Tok_comma {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_dot {source} ->
        formatter |> Fmt.fmt "Tok_dot {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_dot_dot {source} ->
        formatter |> Fmt.fmt "Tok_dot_dot {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_semi {source} ->
        formatter |> Fmt.fmt "Tok_semi {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_colon {source} ->
        formatter |> Fmt.fmt "Tok_colon {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_colon_colon {source} ->
        formatter |> Fmt.fmt "Tok_colon_colon {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_colon_eq {source} ->
        formatter |> Fmt.fmt "Tok_colon_eq {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_lparen {source} ->
        formatter |> Fmt.fmt "Tok_lparen {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_rparen {source} ->
        formatter |> Fmt.fmt "Tok_rparen {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_lbrack {source} ->
        formatter |> Fmt.fmt "Tok_lbrack {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_rbrack {source} ->
        formatter |> Fmt.fmt "Tok_rbrack {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_lcurly {source} ->
        formatter |> Fmt.fmt "Tok_lcurly {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_rcurly {source} ->
        formatter |> Fmt.fmt "Tok_rcurly {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_bar {source} ->
        formatter |> Fmt.fmt "Tok_bar {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_bar_bar {source} ->
        formatter |> Fmt.fmt "Tok_bar_bar {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_larray {source} ->
        formatter |> Fmt.fmt "Tok_larray {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_rarray {source} ->
        formatter |> Fmt.fmt "Tok_rarray {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_bslash {source} ->
        formatter |> Fmt.fmt "Tok_bslash {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_tick {source} ->
        formatter |> Fmt.fmt "Tok_tick {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_caret {source} ->
        formatter |> Fmt.fmt "Tok_caret {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_amp_amp {source} ->
        formatter |> Fmt.fmt "Tok_amp_amp {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_xmark {source} ->
        formatter |> Fmt.fmt "Tok_xmark {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_arrow {source} ->
        formatter |> Fmt.fmt "Tok_arrow {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_whitespace {source} ->
        formatter |> Fmt.fmt "Tok_whitespace {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_paren_comment {source; paren_comment} -> begin
          formatter
          |> Fmt.fmt "Tok_paren_comment {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; paren_comment="
          |> Rendition.pp Unit.pp paren_comment
          |> Fmt.fmt "}"
        end
      | Tok_uscore {source} ->
        formatter |> Fmt.fmt "Tok_uscore {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_uident {source; uident} -> begin
          formatter
          |> Fmt.fmt "Tok_uident {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; uident="
          |> Rendition.pp String.pp uident
          |> Fmt.fmt "}"
        end
      | Tok_cident {source; cident} -> begin
          formatter
          |> Fmt.fmt "Tok_cident {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; cident="
          |> String.pp cident
          |> Fmt.fmt "}"
        end
      | Tok_char {source; char} -> begin
          formatter
          |> Fmt.fmt "Tok_char {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; char="
          |> Rendition.pp Codepoint.pp char
          |> Fmt.fmt "}"
        end
      | Tok_qstring {source; qstring} -> begin
          formatter
          |> Fmt.fmt "Tok_qstring {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; qstring="
          |> Rendition.pp String.pp qstring
          |> Fmt.fmt "}"
        end
      | Tok_istring {source; istring} -> begin
          formatter
          |> Fmt.fmt "Tok_istring {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; istring="
          |> Rendition.pp String.pp istring
          |> Fmt.fmt "}"
        end
      | Tok_r64 {source; r64} -> begin
          formatter
          |> Fmt.fmt "Tok_r64 {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; r64="
          |> Rendition.pp Real.(fmt ~alt:true ~radix:Radix.Hex ~precision:13L
              ~notation:Fmt.Normalized) r64
          |> Fmt.fmt "}"
        end
      | Tok_long {source; long} -> begin
          formatter
          |> Fmt.fmt "Tok_long {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; long="
          |> Rendition.pp U64.pp long
          |> Fmt.fmt "}"
        end
      | Tok_end_of_input {source} ->
        formatter |> Fmt.fmt "Tok_end_of_input {source=" |> Source.Slice.pp source |> Fmt.fmt "}"
      | Tok_error {source; error} -> begin
          formatter
          |> Fmt.fmt "Tok_error {source="
          |> Source.Slice.pp source
          |> Fmt.fmt "; error="
          |> (List.pp Rendition.Malformation.pp) error
          |> Fmt.fmt "}"
        end
    )
    |> Fmt.fmt ")"

  let source = function
    | Tok_and {source}
    | Tok_as {source}
    | Tok_begin {source}
    | Tok_else {source}
    | Tok_end {source}
    | Tok_external {source}
    | Tok_false {source}
    | Tok_fun {source}
    | Tok_function {source}
    | Tok_if {source}
    | Tok_import {source}
    | Tok_in {source}
    | Tok_include {source}
    | Tok_lazy {source}
    | Tok_let {source}
    | Tok_match {source}
    | Tok_mod {source}
    | Tok_module {source}
    | Tok_mutable {source}
    | Tok_nonrec {source}
    | Tok_of {source}
    | Tok_open {source}
    | Tok_or {source}
    | Tok_rec {source}
    | Tok_sig {source}
    | Tok_struct {source}
    | Tok_then {source}
    | Tok_true {source}
    | Tok_type {source}
    | Tok_val {source}
    | Tok_when {source}
    | Tok_with {source}
    | Tok_tilde_op {source; _}
    | Tok_qmark_op {source; _}
    | Tok_star_star_op {source; _}
    | Tok_star_op {source; _}
    | Tok_slash_op {source; _}
    | Tok_pct_op {source; _}
    | Tok_plus_op {source; _}
    | Tok_minus_op {source; _}
    | Tok_at_op {source; _}
    | Tok_caret_op {source; _}
    | Tok_dollar_op {source; _}
    | Tok_lt_op {source; _}
    | Tok_eq_op {source; _}
    | Tok_gt_op {source; _}
    | Tok_bar_op {source; _}
    | Tok_colon_op {source; _}
    | Tok_dot_op {source; _}
    | Tok_tilde {source}
    | Tok_qmark {source}
    | Tok_minus {source}
    | Tok_lt {source}
    | Tok_lt_eq {source}
    | Tok_eq {source}
    | Tok_lt_gt {source}
    | Tok_gt_eq {source}
    | Tok_gt {source}
    | Tok_comma {source}
    | Tok_dot {source}
    | Tok_dot_dot {source}
    | Tok_semi {source}
    | Tok_colon {source}
    | Tok_colon_colon {source}
    | Tok_colon_eq {source}
    | Tok_lparen {source}
    | Tok_rparen {source}
    | Tok_lbrack {source}
    | Tok_rbrack {source}
    | Tok_lcurly {source}
    | Tok_rcurly {source}
    | Tok_bar {source}
    | Tok_bar_bar {source}
    | Tok_larray {source}
    | Tok_rarray {source}
    | Tok_bslash {source}
    | Tok_tick {source}
    | Tok_caret {source}
    | Tok_amp_amp {source}
    | Tok_xmark {source}
    | Tok_arrow {source}
    | Tok_whitespace {source}
    | Tok_paren_comment {source; _}
    | Tok_uscore {source}
    | Tok_uident {source; _}
    | Tok_cident {source; _}
    | Tok_char {source; _}
    | Tok_qstring {source; _}
    | Tok_istring {source; _}
    | Tok_r64 {source; _}
    | Tok_long {source; _}
    | Tok_end_of_input {source}
    | Tok_error {source; _}
      -> source

  let malformations = function
    (* Keywords. *)
    | Tok_and _ | Tok_as _ | Tok_begin _ | Tok_else _ | Tok_end _ | Tok_external _ | Tok_false _
    | Tok_fun _ | Tok_function _ | Tok_if _ | Tok_import _ | Tok_in _ | Tok_include _ | Tok_lazy _
    | Tok_let _ | Tok_match _ | Tok_mod _ | Tok_module _ | Tok_mutable _ | Tok_nonrec _ | Tok_of _
    | Tok_open _ | Tok_or _ | Tok_rec _ | Tok_sig _ | Tok_struct _ | Tok_then _ | Tok_true _
    | Tok_type _ | Tok_val _ | Tok_when _ | Tok_with _
    (* Operators. *)
    | Tok_tilde_op _ | Tok_qmark_op _ | Tok_star_star_op _ | Tok_star_op _ | Tok_slash_op _
    | Tok_pct_op _ | Tok_plus_op _ | Tok_minus_op _ | Tok_at_op _ | Tok_caret_op _ | Tok_dollar_op _
    | Tok_lt_op _ | Tok_eq_op _ | Tok_gt_op _ | Tok_bar_op _ | Tok_colon_op _ | Tok_dot_op _
    (* Punctuation. *)
    | Tok_tilde _ | Tok_qmark _ | Tok_minus _ | Tok_lt _ | Tok_lt_eq _ | Tok_eq _ | Tok_lt_gt _
    | Tok_gt_eq _ | Tok_gt _ | Tok_comma _ | Tok_dot _ | Tok_dot_dot _ | Tok_semi _ | Tok_colon _
    | Tok_colon_colon _ | Tok_colon_eq _ | Tok_lparen _ | Tok_rparen _ | Tok_lbrack _ | Tok_rbrack _
    | Tok_lcurly _ | Tok_rcurly _ | Tok_bar _ | Tok_bar_bar _ | Tok_larray _ | Tok_rarray _
    | Tok_bslash _ | Tok_tick _ | Tok_caret _ | Tok_amp_amp _ | Tok_xmark _ | Tok_arrow _
    (* Miscellaneous. *)
    | Tok_whitespace _
    | Tok_paren_comment {paren_comment=(Constant _); _}
    | Tok_uscore _
    | Tok_uident {uident=(Constant _); _}
    | Tok_cident _
    | Tok_char {char=(Constant _); _}
    | Tok_qstring {qstring=(Constant _); _}
    | Tok_istring {istring=(Constant _); _}
    | Tok_r64 {r64=(Constant _); _}
    | Tok_long {long=(Constant _); _}
    | Tok_end_of_input _
      -> []
    (* Malformations. *)
    | Tok_paren_comment {paren_comment=(Malformed mals); _}
    | Tok_uident {uident=(Malformed mals); _}
    | Tok_char {char=(Malformed mals); _}
    | Tok_qstring {qstring=(Malformed mals); _}
    | Tok_istring {istring=(Malformed mals); _}
    | Tok_r64 {r64=(Malformed mals); _}
    | Tok_long {long=(Malformed mals); _}
    | Tok_error {error=mals; _}
      -> mals
end

type t = Hmc.Scan.t

let init = Hmc.Scan.init

let pp = Hmc.Scan.pp

let text = Hmc.Scan.text

let cursor = Hmc.Scan.cursor

(**************************************************************************************************)

let malformation ~source description =
  Token.Tok_error {source; error=[Token.Rendition.Malformation.of_source ~source ~description]}

let rec next t =
  let t', hmc_token = Hmc.Scan.next t in
  match hmc_token with
  | Tok_line_delim _
  | Tok_indent _
  | Tok_dedent _
  | Tok_misaligned _
    -> next t'
  | _ -> begin
      let token = match hmc_token with
        | Tok_and {source} -> Token.Tok_and {source}
        | Tok_also {source} -> Tok_uident {source; uident=Constant "also"}
        | Tok_as {source} -> Tok_as {source}
        | Tok_conceal {source} -> Tok_uident {source; uident=Constant "conceal"}
        | Tok_effect {source} -> Tok_uident {source; uident=Constant "effect"}
        | Tok_else {source} -> Tok_else {source}
        | Tok_expose {source} -> Tok_uident {source; uident=Constant "expose"}
        | Tok_external {source} -> Tok_external {source}
        | Tok_false {source} -> Tok_false {source}
        | Tok_fn {source} -> Tok_uident {source; uident=Constant "fn"}
        | Tok_function {source} -> Tok_function {source}
        | Tok_if {source} -> Tok_if {source}
        | Tok_import {source} -> Tok_import {source}
        | Tok_include {source} -> Tok_include {source}
        | Tok_lazy {source} -> Tok_lazy {source}
        | Tok_let {source} -> Tok_let {source}
        | Tok_match {source} -> Tok_match {source}
        | Tok_mutability {source} -> Tok_uident {source; uident=Constant "mutability"}
        | Tok_of {source} -> Tok_of {source}
        | Tok_open {source} -> Tok_open {source}
        | Tok_or {source} -> Tok_or {source}
        | Tok_rec {source} -> Tok_rec {source}
        | Tok_then {source} -> Tok_then {source}
        | Tok_true {source} -> Tok_true {source}
        | Tok_type {source} -> Tok_type {source}
        | Tok_when {source} -> Tok_when {source}
        | Tok_with {source} -> Tok_with {source}

        | Tok_tilde_op {source; tilde_op} -> Tok_tilde_op {source; tilde_op}
        | Tok_qmark_op {source; qmark_op} -> Tok_qmark_op {source; qmark_op}
        | Tok_star_star_op {source; star_star_op} -> Tok_star_star_op {source; star_star_op}
        | Tok_star_op {source; star_op} -> Tok_star_op {source; star_op}
        | Tok_slash_op {source; slash_op} -> Tok_slash_op {source; slash_op}
        | Tok_pct_op {source; pct_op} -> Tok_pct_op {source; pct_op}
        | Tok_plus_op {source; plus_op} -> Tok_plus_op {source; plus_op}
        | Tok_minus_op {source; minus_op} -> Tok_minus_op {source; minus_op}
        | Tok_at_op {source; at_op} -> Tok_at_op {source; at_op}
        | Tok_caret_op {source; caret_op} -> Tok_caret_op {source; caret_op}
        | Tok_dollar_op {source; dollar_op} -> Tok_dollar_op {source; dollar_op}
        | Tok_lt_op {source; lt_op} -> Tok_lt_op {source; lt_op}
        | Tok_eq_op {source; eq_op} -> Tok_eq_op {source; eq_op}
        | Tok_gt_op {source; gt_op} -> Tok_gt_op {source; gt_op}
        | Tok_bar_op {source; bar_op} -> begin
            match bar_op with
            | "||" -> Tok_bar_bar {source}
            | _ -> Tok_bar_op {source; bar_op}
          end
        | Tok_colon_op {source; colon_op} -> Tok_colon_op {source; colon_op}
        | Tok_dot_op {source; dot_op} -> Tok_dot_op {source; dot_op}

        | Tok_tilde {source} -> Tok_tilde {source}
        | Tok_qmark {source} -> Tok_qmark {source}
        | Tok_minus {source} -> Tok_minus {source}
        | Tok_lt {source} -> Tok_lt {source}
        | Tok_lt_eq {source} -> Tok_lt_eq {source}
        | Tok_eq {source} -> Tok_eq {source}
        | Tok_lt_gt {source} -> Tok_lt_gt {source}
        | Tok_gt_eq {source} -> Tok_gt_eq {source}
        | Tok_gt {source} -> Tok_gt {source}
        | Tok_comma {source} -> Tok_comma {source}
        | Tok_dot {source} -> Tok_dot {source}
        | Tok_dot_dot {source} -> Tok_dot_dot {source}
        | Tok_semi {source} -> Tok_semi {source}
        | Tok_colon {source} -> Tok_colon {source}
        | Tok_colon_colon {source} -> Tok_colon_colon {source}
        | Tok_colon_eq {source} -> Tok_colon_eq {source}
        | Tok_lparen {source} -> Tok_lparen {source}
        | Tok_rparen {source} -> Tok_rparen {source}
        | Tok_lbrack {source} -> Tok_lbrack {source}
        | Tok_rbrack {source} -> Tok_rbrack {source}
        | Tok_lcurly {source} -> Tok_lcurly {source}
        | Tok_rcurly {source} -> Tok_rcurly {source}
        | Tok_bar {source} -> Tok_bar {source}
        | Tok_larray {source} -> Tok_larray {source}
        | Tok_rarray {source} -> Tok_rarray {source}
        | Tok_bslash {source} -> Tok_bslash {source}
        | Tok_tick {source} -> Tok_tick {source}
        | Tok_caret {source} -> Tok_caret {source}
        | Tok_amp_amp {source} -> Tok_amp_amp {source}
        | Tok_xmark {source} -> Tok_xmark {source}
        | Tok_arrow {source} -> Tok_arrow {source}

        | Tok_lcapture {source} | Tok_rcapture {source} | Tok_amp {source} | Tok_carrow {source}
          -> malformation ~source "Hemlock-specific operator"

        | Tok_source_directive {source; _} ->
          malformation ~source "Hemlock-specific source directive"

        | Tok_line_delim _ | Tok_indent _ | Tok_dedent _
          -> not_reached () (* Handled by outer match. *)

        | Tok_whitespace {source} -> Tok_whitespace {source}
        | Tok_hash_comment {source} -> malformation ~source "Hemlock-specific comment syntax"
        | Tok_paren_comment {source; paren_comment} -> Tok_paren_comment {source; paren_comment}
        | Tok_uscore {source} -> Tok_uscore {source}
        | Tok_uident {source; uident} -> begin
            match uident with
            | Constant s -> begin
                match s with
                | "begin" -> Tok_begin {source}
                | "end" -> Tok_end {source}
                | "fun" -> Tok_fun {source}
                | "in" -> Tok_in {source}
                | "mod" -> Tok_mod {source}
                | "module" -> Tok_module {source}
                | "mutable" -> Tok_mutable {source}
                | "nonrec" -> Tok_nonrec {source}
                | "sig" -> Tok_sig {source}
                | "struct" -> Tok_struct {source}
                | "val" -> Tok_val {source}
                | _ -> Tok_uident {source; uident}
              end
            | Malformed _ -> Tok_uident {source; uident}
          end
        | Tok_cident {source; cident} -> Tok_cident {source; cident}
        | Tok_codepoint {source; codepoint} -> begin
            match codepoint with
            | Constant cp -> begin
                match Codepoint.(cp < kv 0x100L) with
                | true -> Tok_char {source; char=codepoint}
                | false -> Tok_char {
                  source;
                  char=Malformed [
                    Token.Rendition.Malformation.of_source ~source
                      ~description:"Codepoint exceeds char range"]}
              end
            | Malformed _ -> Tok_char {source; char=codepoint}
          end
        | Tok_rstring {source; _} -> malformation ~source "Hemlock-specific raw string syntax"
        | Tok_qstring {source; qstring} -> Tok_qstring {source; qstring}
        | Tok_istring {source; istring} -> Tok_istring {source; istring}
        | Tok_fstring_lditto {source; _} | Tok_fstring_interpolated {source; _}
        | Tok_fstring_pct {source; _} | Tok_fstring_pad {source; _} | Tok_fstring_just {source; _}
        | Tok_fstring_sign {source; _} | Tok_fstring_alt {source; _} | Tok_fstring_zpad {source; _}
        | Tok_fstring_width_star {source; _} | Tok_fstring_width {source; _}
        | Tok_fstring_pmode {source; _} | Tok_fstring_precision_star {source; _}
        | Tok_fstring_precision {source; _} | Tok_fstring_radix {source; _}
        | Tok_fstring_notation {source; _} | Tok_fstring_pretty {source; _}
        | Tok_fstring_fmt {source; _} | Tok_fstring_sep {source; _} | Tok_fstring_label {source; _}
        | Tok_fstring_lparen_caret {source; _} | Tok_fstring_caret_rparen {source; _}
        | Tok_fstring_rditto {source; _}
          -> malformation ~source "Hemlock-specific formatted string syntax"
        | Tok_r32 {source; _} -> malformation ~source "Hemlock-specific real syntax"
        | Tok_r64 {source; r64} -> Tok_r64 {source; r64}
        | Tok_u8 {source; _} | Tok_i8 {source; _} | Tok_u16 {source; _} | Tok_i16 {source; _}
        | Tok_u32 {source; _} | Tok_i32 {source; _} | Tok_u64 {source; _} | Tok_i64 {source; _}
        | Tok_u128 {source; _} | Tok_i128 {source; _} | Tok_u256 {source; _} | Tok_i256 {source; _}
        | Tok_u512 {source; _} | Tok_i512 {source; _} | Tok_nat {source; _} | Tok_zint {source; _}
          -> malformation ~source "Hemlock-specific integer syntax"
        | Tok_long {source; long} -> Tok_long {source; long}
        | Tok_end_of_input {source} -> Tok_end_of_input {source}
        | Tok_misaligned _ -> not_reached () (* Handled by outer match. *)
        | Tok_error {source; error} -> Tok_error {source; error}
      in
      t', token
    end
