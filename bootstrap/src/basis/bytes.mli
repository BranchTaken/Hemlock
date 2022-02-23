(** Byte array convenience functions. {!type:string} can represent only well formed UTF-8-encoded
    {!type:codepoint} sequences, and byte arrays provide a convenient less constrained
    representation for conversion to/from {!type:string}. *)

open Rudiments0

type t = byte array

module Cursor : sig
  include CursorIntf.SMonoIndex
    with type container := byte array
    with type elm := byte
end

module Slice : sig
  include SliceIntf.SMonoIndex
    with type container := byte array
    with type cursor := Cursor.t
    with type elm := byte
  include FormattableIntf.SMono with type t := t

  val hash_fold: t -> Hash.State.t -> Hash.State.t
  (** [hash_fold bytes] incorporates the hash of [t] into [state] and returns the resulting state.
  *)

  val length: t -> uns
  (** [length t] returns the length of [t] in bytes. *)

  val get: uns -> t -> byte
  (** [get i t] returns the byte at index [i] within [t]. *)

  val to_bytes: t -> byte array
  (** [to_bytes t] creates a new byte array containing the contents enclosed by [t]. *)

  val of_codepoint: codepoint -> t
  (** [of_codepoint codepoint] creates a slice of bytes corresponding to the UTF-8 encoding of
      [codepoint]. *)

  val of_string_slice: String.C.Slice.t -> t
  (** [of_string_slice slice] creates a slice of bytes corresponding to the UTF-8 encoding of
      [slice]. *)

  val join: ?sep:t -> t list -> t
  (** [join ~sep slices] creates a slice comprised of the concatenation of the [slices] list, with
      [sep] interposed between the inputs. *)

  val to_string: t -> string option
  (** [to_string t] interprets [t] as a sequence of UTF-8 code points and returns a corresponding
      {!type:string}, or [None] if [t] is malformed. *)

  val to_string_replace: t -> string
  (** [to_string_replace t] interprets [t] as a sequence of UTF-8 code points and returns a
      corresponding {!type:string}, with malformed input converted to one or more '�' replacement
      characters. *)

  val to_string_hlt: t -> string
  (** [to_string_hlt t] interprets [t] as a sequence of UTF-8 code points and returns a
      corresponding {!type:string}, or halts if [t] is malformed. *)
end

include FormattableIntf.SMono with type t := t

val hash_fold: t -> Hash.State.t -> Hash.State.t
(** [hash_fold bytes] incorporates the hash of [t] into [state] and returns the resulting state. *)

val length: t -> uns
(* [length t] returns the length of [t] in bytes. *)

val get: uns -> t -> byte
(** [get i t] returns the byte at index [i] within [t]. *)

val of_codepoint: codepoint -> t
(** [of_codepoint codepoint] creates an array of bytes corresponding to the UTF-8 encoding of
    [codepoint]. *)

val of_string_slice: String.C.Slice.t -> t
(** [of_string_slice slice] creates a slice of bytes corresponding to the UTF-8 encoding of [slice].
*)

val to_string: t -> string option
(** [to_string t] interprets [t] as a sequence of UTF-8 code points and returns a corresponding
    {!type:string}, or [None] if [t] is malformed. *)

val to_string_replace: t -> string
(** [to_string_replace t] interprets [t] as a sequence of UTF-8 code points and returns a
    corresponding {!type:string}, with malformed input converted to one or more '�' replacement
    characters. *)

val to_string_hlt: t -> string
(** [to_string_hlt t] interprets [t] as a sequence of UTF-8 code points and returns a corresponding
    {!type:string}, or halts if [t] is malformed. *)
