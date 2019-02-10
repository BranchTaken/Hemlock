(* Partial Rudiments. *)
module Codepoint = U21
module Byte = U8
type 'a array = 'a Array.t
type string = String.t
type codepoint = Codepoint.t
type byte = Byte.t

val of_codepoint: codepoint -> byte array
val of_string: string -> byte array
val to_string: byte array -> string option
val to_string_hlt: byte array -> string
