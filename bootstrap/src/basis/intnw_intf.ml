(** Functor interfaces and signatures for integers of specific power-of-two
    wordwidth. *)

include Rudiments_int0
include Intnb_intf

(** Functor input interface for an integer type with a specific power-of-two
    wordwidth. *)
module type I = sig
  val num_bits: uns
  (** Number of bits in integer representation. *)
end

(** Functor output signature for an unsigned integer type with a specific
    power-of-two wordwidth. *)
module type S_u = sig
  type t

  include S with type t := t
end

(** Functor output signature for a signed integer type with a specific
    power-of-two wordwidth. *)
module type S_i = sig
  type t

  include S with type t := t
  include S_signed with type t := t
end
