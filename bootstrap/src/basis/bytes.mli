(** Byte array convenience functions.  {!type:string} can represent only well
    formed UTF-8-encoded {!type:codepoint} sequences, and byte arrays provide a
    convenient less constrained representation for conversion to/from
    {!type:string}. *)

open Rudiments

include Formattable_intf.S_mono with type t := byte array

val hash_fold: byte array -> Hash.State.t -> Hash.State.t
(** [hash_fold bytes] incorporates the hash of [bytes] into [state] and returns
    the resulting state. *)

val of_codepoint: codepoint -> byte array
(** [of_codepoint codepoint] creates an array of bytes corresponding to the
    UTF-8 encoding of [codepoint]. *)

val of_string: string -> byte array
(** [of_string string] creates an array of bytes corresponding to the UTF-8
    encoding of [string]. *)

val to_string: byte array -> string option
(** [to_string bytes] interprets [bytes] as a sequence of UTF-8 code points and
    returns a corresponding {!type:string}, or [None] if [bytes] is malformed.
*)

val to_string_hlt: byte array -> string
(** [to_string bytes] interprets [bytes] as a sequence of UTF-8 code points and
    returns a corresponding {!type:string}, or halts if [bytes] is malformed. *)
