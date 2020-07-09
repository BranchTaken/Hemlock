(** Hash type. *)

open Rudiments_int0

type t = u128
(** Hash type.  All bits are i.i.d. uniform variables. *)

include Formattable_intf.S_mono with type t := t

(** Hash state management and hashing. *)
module State : sig
  type t
  (** Hash state type. *)

  include Formattable_intf.S_mono with type t := t

  val empty: t
  (** Unseeded hash state (constant).  Note that no stateless hash functions
      exist, but they can be simulated.  For example, a stable {!type:usize}
      hash function can be implemented as [(fun u -> Usize.hash_fold u
      Hash.State.empty)]. *)

  val of_u128: u128 -> t
  (** [of_u128 u] creates a state based on [u].  This function has no practical
      use outside of testing.  Prefer to manually seed via {!module:Entropy}. *)

  val seed: t
  (** Return the seed state for this execution of the application.  The seed
      state is based on {!Entropy.seed}. *)

  (** Decomposed hash state generator API for use in implementing [hash_fold]
      for types that do not map perfectly to other types.  The hash algorithm is
      the 128-bit variant of {{: https://en.wikipedia.org/wiki/MurmurHash}
      MurmurHash3}, which hashes 128-bit blocks with automatic support for a
      partial block during finalization. *)
  module Gen : sig
    type outer = t

    type t
    (** Hash state generator type. *)

    val init: outer -> t
    (** [init state] creates a new hash generator, which can then be fed data
        via zero or more calls to [fold_*]. *)

    val fold_u128: usize -> f:(usize -> u128) -> t -> t
    (** [fold_u128 n ~f t] incorporates the hash of [n] indexed values provided
        by [~f] into [t] and returns the resulting hash state generator. *)

    val fold_u8: usize -> f:(usize -> usize) -> t -> t
    (** [fold_u8 n ~f t] incorporates the hash of [n] indexed values provided by
        [~f] into [t] and returns the resulting hash state generator.  Note that
        the values returned by [~f] are treated as {!type:u8} (only the least
        significant 8 bits are used), but the exported type signature differs
        due to bootstrapping constraints. *)

    val fini: t -> outer
    (** [fini t] produces a hash state which is a function of the state provided
        to [init] and the values provided to [fold_*]. *)
  end
end

val t_of_state: State.t -> t
(** [t_of_state state] returns the hash value associated with [state]. *)
