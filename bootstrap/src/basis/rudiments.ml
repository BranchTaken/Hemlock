(* Aliases. *)

module Int = I63
module Uns = U63
module Codepoint = U21
module Byte = U8

(* Types. *)

type i63 = I63.t

type u63 = U63.t

type u21 = U21.t
type codepoint = Codepoint.t

type u8 = U8.t
type byte = Byte.t

type i2 = I2.t

include Rudiments_functions
include Rudiments_int
