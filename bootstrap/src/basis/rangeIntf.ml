(** Range interfaces. *)

(** Range length type. Maximal full-open ranges may overflow the type being used to express length,
    which is encoded as [Overflow]. Furthermore the `nat` type may have infinite range length, which
    is also encoded as [Overflow]. *)
type 'a l =
  | Overflow
  | Length of 'a

(** Functor output signature for half-open ranges. *)
module type SRangeH = sig
  type t
  (** Container type. *)

  type elm
  (** Element type. *)

  type l
  (** Length type. *)

  val ( =:< ): elm -> elm -> t
  (** [base =:< past] initializes a half-open range, i.e [\[base .. past)]. No constraint is assumed
      regarding ordering of [base] and [past], which among other behaviors enables ranges which wrap
      around. *)

  val base: t -> elm
  (** Base element in range. *)

  val past: t -> elm
  (** Element immediately past last element in range. *)

  include ContainerIntf.SMono with type t := t with type elm := elm

  val length: t -> l
  (** Number of elements in range. *)
end

(** Functor output signature for full-open ranges. *)
module type SRangeF = sig
  type t
  (** Container type. *)

  type elm
  (** Element type. *)

  type l
  (** Length type. *)

  val ( =:= ): elm -> elm -> t
  (** [base =:= last] initializes a full-open range, i.e [\[base .. last\]]. No constraint is
      assumed regarding ordering of [base] and [last], which among other behaviors enables ranges
      which wrap around. *)

  val base: t -> elm
  (** Base element in range. *)

  val last: t -> elm
  (** Last element in range. *)

  include ContainerIntf.SMono with type t := t with type elm := elm

  val length: t -> l
  (** Number of elements in range. *)
end
