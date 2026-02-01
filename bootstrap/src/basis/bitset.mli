(** Bitset, where the underlying implementation is a {!type:Nat.t}. The least significant bit is bit
    0, and 0 is a set member iff bit 0 is set, 1 is a set member iff the adjacent bit is set,
    etc. *)

open Rudiments0

type t

include SetIntf.SOrdMono with type t := t with type elm := uns

val of_nat: Nat.t -> t
(** [of_nat nat] returns a bitset based on the bits in [nat]. *)

val to_nat: t -> Nat.t
(** [to_nat t] returns the value underlying [t]. *)
