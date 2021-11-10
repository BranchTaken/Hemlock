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

type just =
  | Left   (** Left-justify. *)
  | Center (** Center. *)
  | Right  (** Right-justify. *)

type sign =
  | Implicit (** Non-negative sign is implicit. *)
  | Explicit (** Sign is always explicit. *)
  | Space    (** Non-negative sign is formatted as a space. *)

type base =
  | Bin (** Binary base (2). *)
  | Oct (** Octal base (8). *)
  | Dec (** Decimal base (10). *)
  | Hex (** Hexadecimal base (16). *)

type notation =
  | Normalized (** Normalized scientific form. *)
  | RadixPoint (** Radix point form. *)
  | Compact    (** The more compact of [Normalized]/[RadixPoint]. *)

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

val precision_default: int64
(** Default digits of precision to right of point ([2]). *)

val base_default: base
(** Default numerical base ([Dec]). *)

val notation_default: notation
(** Default notation ([Compact]). *)

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
