(* Aliases. *)

module Uns = U63
module Byte = U8

(* Types. *)

type i63 = I63.t

type u63 = U63.t

type i32 = I32.t

type u32 = U32.t

type i16 = I16.t

type u16 = U16.t

type i8 = I8.t

type u8 = U8.t
type byte = Byte.t

type codepoint = Codepoint.t

include Rudiments_functions
include Rudiments_int
