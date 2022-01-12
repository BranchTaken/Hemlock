(** Formatted string/file output. *)

(** Output type for [sync]. *)
type 'a synced =
  | To_string of string (** [sync] produced a string. *)
  | Synced of 'a        (** [sync] synchronized. *)

(** Formatter module type, used both for string and file output. *)
module type Formatter = sig
  type t
  (** Abstract formatter type. *)

  val state: t
  (** Opaque state. *)

  val fmt: string -> t -> t
  (** [fmt s t] produces a new state which incorporates [s] into [t]. *)

  val sync: t -> t synced
  (** [sync t] synchronizes any pending formatting. String formatters produce [To_string string],
      whereas file formatters flush any output buffers and produce [Synced t]. *)
end

val pp_synced: ('a -> (module Formatter) -> (module Formatter)) -> 'a synced -> (module Formatter)
  -> (module Formatter)
(** [pp_synced pp_a synced formatter] uses [pp_a] to format a syntactically valid representation of
    [synced]. *)

type just =
  | Left   (** Left-justify. *)
  | Center (** Center. *)
  | Right  (** Right-justify. *)

val pp_just: just -> (module Formatter) -> (module Formatter)
(** [pp_just just] formats a syntactically valid representation of [just]. *)

type sign =
  | Implicit (** Non-negative sign is implicit. *)
  | Explicit (** Sign is always explicit. *)
  | Space    (** Non-negative sign is formatted as a space. *)

val pp_sign: sign -> (module Formatter) -> (module Formatter)
(** [pp_sign sign] formats a syntactically valid representation of [sign]. *)

type base =
  | Bin (** Binary base (2). *)
  | Oct (** Octal base (8). *)
  | Dec (** Decimal base (10). *)
  | Hex (** Hexadecimal base (16). *)

val pp_base: base -> (module Formatter) -> (module Formatter)
(** [pp_base base] formats a syntactically valid representation of [base]. *)

type pmode =
  | Limited (** Limited precision, i.e. trailing zeros omitted. *)
  | Fixed   (** Fixed precision, i.e. trailing zeros as needed. *)

val pp_pmode: pmode -> (module Formatter) -> (module Formatter)
(** [pp_pmode pmode] formats a syntactically valid representation of [pmode]. *)

type notation =
  | Normalized (** Normalized scientific form. *)
  | RadixPoint (** Radix point form. *)
  | Compact    (** The more compact of [Normalized]/[RadixPoint]. *)

val pp_notation: notation -> (module Formatter) -> (module Formatter)
(** [pp_notation notation] formats a syntactically valid representation of [notation]. *)

val pad_default: string
(** Default padding ([" "]). This is a {!type:string} rather than {!type:codepoint} due to
    bootstrapping constraints. Where possible the [fmt] functions in other modules present
    [?pad:codepoint]. *)

val just_default: just
(** Default justification ([Right]). *)

val sign_default: sign
(** Default sign formatting ([Implicit]). *)

val alt_default: bool
(** Default alternate formatting ([false]). *)

val zpad_default: bool
(** Default zero-padding ([false]). *)

val width_default: int64
(** Default width ([0]). *)

val pmode_default: pmode
(** Default precision mode ([Limited]). *)

val precision_bin_m_default: int64
(** Default digits of [Normalized] precision to right of binary radix point ([52]). *)

val precision_bin_a_default: int64
(** Default digits of [RadixPoint] precision to right of binary radix point ([53]). *)

val precision_oct_m_default: int64
(** Default digits of [Normalized] precision to right of octal radix point ([18]). *)

val precision_oct_a_default: int64
(** Default digits of [RadixPoint] precision to right of octal radix point ([18]). *)

val precision_dec_m_default: int64
(** Default digits of [Normalized] precision to right of decimal radix point ([15]). *)

val precision_dec_a_default: int64
(** Default digits of [RadixPoint] precision to right of decimal radix point ([3]). *)

val precision_hex_m_default: int64
(** Default digits of [Normalized] precision to right of hexadecimal radix point ([13]). *)

val precision_hex_a_default: int64
(** Default digits of [RadixPoint] precision to right of hexadecimal radix point ([14]). *)

val base_default: base
(** Default numerical base ([Dec]). *)

val notation_default: notation
(** Default notation ([Compact]). *)

val pretty_default: bool
(** Default pretty ([false]). *)

val fmt: ?pad:string -> ?just:just -> ?width:int64 -> string -> (module Formatter)
  -> (module Formatter)
(** [fmt ~pad ~just ~width s formatter] creates a string based on [s] that is [~just]-justified with
    minimum [~pad]-padded [~width] and applies the result to [formatter]. *)

val sync: (module Formatter) -> (module Formatter) synced
(** [sync formatter] calls [formatter]'s [sync] function and returns the result. A string formatter
    returns [To_string string], whereas a file formatter returns [Synced formatter]. *)

val flush: (module Formatter) -> (module Formatter)
(** [flush formatter] calls [formatter]'s [sync] function and returns [formatter'] if [sync]
    returns [Synced formatter'], or halts if [sync] returns [To_string string]. *)

val to_string: (module Formatter) -> string
(** [to_string formatter] calls [formatter]'s [sync] function and returns the resulting string if
    [sync] returns [To_string string], or halts if [sync] returns [Synced formatter']. *)
