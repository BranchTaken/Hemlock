(** Functor interfaces and signatures for integers of optionally constrained wordwidth. These
    functors provide the building blocks for the fixed-wordwidth types ({!type:u128}, {!type:u256},
    {!type:u512}, {!type:i128}, {!type:i256}, and {!type:i512}), but arbitrary-precision integers
    are also supported via relaxed wordwidth constraints (\[0..Uns.max_value\]). *)

include RudimentsInt0

(** Functor input interface for a variable-wordwidth integer type. *)
module type IV = sig
  type t

  val min_word_length: uns
  (** Minimum number of 64-bit words in physical representation, at least 1, and no larger than
      {!max_word_length}. *)

  val max_word_length: uns
  (** Maximum number of 64-bit words in physical representation, no smaller than {!min_word_length}.
  *)

  val init: uns -> f:(uns -> u64) -> t
  (** Initialize integer. [init len ~f] initializes an integer of given wordwidth, where [f]
      provides the value for each element at given index. *)

  val word_length: t -> uns
  (** Current number of 64-bit words in physical representation, in [\[min_word_length ..
      max_word_length\]]. *)

  val get: uns -> t -> u64
  (** Get word. [get i t] returns the element at index [i]. *)
end

(** Functor input interface for a fixed-wordwidth integer type. *)
module type IF = sig
  type t

  val word_length: uns
  (** Number of 64-bit words in physical representation *)

  val init: f:(uns -> u64) -> t
  (** Initialize integer. [init len ~f] initializes an integer of fixed wordwidth, where [f]
      provides the value for each element at given index. *)

  val get: uns -> t -> u64
  (** Get word. [get i t] returns the element at index [i]. *)
end

module type SCommon = sig
  type t

  val to_u64: t -> u64
  (** Convert to 64-bit unsigned integer, with possible loss. *)

  val to_u64_opt: t -> u64 option
  (** Convert to 64-bit unsigned integer, or return [None] if conversion would be lossy. *)

  val to_u64_hlt: t -> u64
  (** Convert to 64-bit unsigned integer, or halt if conversion would be lossy. *)

  val of_u64: u64 -> t
  (** Initialize from a 64-bit unsigned integer. *)

  val to_uns: t -> uns
  (** Convert to default-width unsigned integer, with possible loss. *)

  val to_uns_opt: t -> uns option
  (** Convert to default-width unsigned integer, or return [None] if conversion would be lossy. *)

  val to_uns_hlt: t -> uns
  (** Convert to default-width unsigned integer, or halt if conversion would be lossy. *)

  val of_uns: uns -> t
  (** Initialize from a default-width unsigned integer. *)
end

module type SSigned = sig
  include IntnbIntf.SSigned

  val to_i64: t -> i64
  (** Convert to 64-bit signed integer, with possible loss. *)

  val to_i64_opt: t -> i64 option
  (** Convert to 64-bit signed integer, or return [None] if conversion would be lossy. *)

  val to_i64_hlt: t -> i64
  (** Convert to 64-bit signed integer, or halt if conversion would be lossy. *)

  val of_i64: i64 -> t
  (** Initialize from a 64-bit signed integer. *)

  val to_sint: t -> sint
  (** Convert to default-width signed integer, with possible loss. *)

  val to_sint_opt: t -> sint option
  (** Convert to default-width signed integer, or return [None] if conversion would be lossy. *)

  val to_sint_hlt: t -> sint
  (** Convert to default-width signed integer, or halt if conversion would be lossy. *)

  val of_sint: sint -> t
  (** Initialize from a default-width signed integer. *)
end

module type SVCommon = sig
  include IV
  include SCommon with type t := t
  include IntnbIntf.SLimitless with type t := t

  val bit_length: t -> uns
  (** Number of bits in physical representation, equivalent to [word_length t * 64]. *)
end

(** Functor output signature for an unsigned integer type with a variable wordwidth. *)
module type SVU = sig
  type t

  include SVCommon with type t := t
end

(** Functor output signature for a signed integer type with a variable wordwidth. *)
module type SVI = sig
  type t

  include SVCommon with type t := t
  include SSigned with type t := t
end

module type SFCommon = sig
  include IF
  include SCommon with type t := t
  include IntnbIntf.S with type t := t

  val bit_length: uns
  (** Number of bits in physical representation, equivalent to [word_length t * 64]. *)
end

(** Functor output signature for an unsigned integer type with a fixed wordwidth. *)
module type SFU = sig
  type t

  include SFCommon with type t := t
end

(** Functor output signature for a signed integer type with a fixed wordwidth. *)
module type SFI = sig
  type t

  include SFCommon with type t := t
  include SSigned with type t := t
end
