type 'a array = 'a Array.t

type i63 = I63.t
module Int = I63
type int = Int.t

type u63 = U63.t
module Uint = U63
type uint = Uint.t

type u21 = U21.t
module Codepoint = U21
type codepoint = Codepoint.t

type u8 = U8.t
module Byte = U8
type byte = Byte.t

include Rudiments_functions
