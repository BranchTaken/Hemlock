(** Functor interfaces and signatures for integers of specific power-of-two
    wordwidth. *)

include Rudiments_int0

(** Functor input interface for an integer type with a specific power-of-two
    wordwidth. *)
module type I = sig
  type t

  val num_bits: uns
  (** Number of bits in integer representation. *)

  val of_arr: u64 array -> t
  (** Convert from array. *)

  val to_arr: t -> u64 array
  (** Convert to array. *)
end

module type S = sig
  include Intnb_intf.S

  val of_arr: u64 array -> t
  (** Convert from array. *)

  val to_arr: t -> u64 array
  (** Convert to array. *)

  val to_u64: t -> u64
  (** Convert to 64-bit unsigned integer, with possible loss. *)

  val to_u64_hlt: t -> u64
  (** Convert to 64-bit unsigned integer, or halt if conversion would be lossy.
  *)

  val of_u64: u64 -> t
  (** Initialize from a 64-bit unsigned integer. *)

  val to_uns: t -> uns
  (** Convert to default-width unsigned integer, with possible loss. *)

  val to_uns_hlt: t -> uns
  (** Convert to default-width unsigned integer, or halt if conversion would be
      lossy. *)

  val of_uns: uns -> t
  (** Initialize from a default-width unsigned integer. *)
end

(** Functor output signature for an unsigned integer type with a specific
    power-of-two wordwidth. *)
module type S_u = sig
  type t

  include S with type t := t
end

module type S_signed = Intnb_intf.S_signed

(** Functor output signature for a signed integer type with a specific
    power-of-two wordwidth. *)
module type S_i = sig
  type t

  include S with type t := t
  include S_signed with type t := t
end
