(** Byte array convenience functions. {!type:string} can represent only well
    formed UTF-8-encoded {!type:codepoint} sequences, and byte arrays provide a
    convenient less constrained representation for conversion to/from
    {!type:string}. *)

open Rudiments0

type t = byte array

include Formattable_intf.S_mono with type t := t

val hash_fold: t -> Hash.State.t -> Hash.State.t
(** [hash_fold bytes] incorporates the hash of [t] into [state] and returns the
    resulting state. *)

val of_codepoint: codepoint -> t
(** [of_codepoint codepoint] creates an array of bytes corresponding to the
    UTF-8 encoding of [codepoint]. *)

val of_string: string -> t
(** [of_string string] creates an array of bytes corresponding to the UTF-8
    encoding of [string]. *)

val to_string: t -> string option
(** [to_string t] interprets [t] as a sequence of UTF-8 code points and returns
    a corresponding {!type:string}, or [None] if [t] is malformed. *)

val to_string_hlt: t -> string
(** [to_string_hlt t] interprets [t] as a sequence of UTF-8 code points and
    returns a corresponding {!type:string}, or halts if [t] is malformed. *)
