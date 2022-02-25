(** Range interfaces. *)

(** Range limit type. The distinction matters for minimal and maximal ranges. *)
type 'a limit =
  | Excl of 'a (** Half-open range limit. *)
  | Incl of 'a (** Full-open range limit. *)

(** Range length type. Maximal full-open ranges may overflow the type being used to express length,
    which is encoded as [Overflow]. Furthermore the `nat` type may have infinite range length, which
    is also encoded as [Overflow]. *)
type 'a length =
  | Overflow
  | Length of 'a

(** Functor output signature for ranges. *)
module type SRange = sig
  type t
  (** Container type. *)

  type elm
  (** Element type. *)

  type limit
  (** Limit type. *)

  type length
  (** Length type. *)

  val ( =:< ): elm -> elm -> t
  (** [base =:< past] initializes a half-open range, i.e [\[base .. past)]. No constraint is assumed
      regarding ordering of [base] and [past], which among other behaviors enables ranges which wrap
      around. *)

  val ( =:= ): elm -> elm -> t
  (** [base =:= last] initializes a full-open range, i.e [\[base .. last\]]. No constraint is
      assumed regarding ordering of [base] and [last], which among other behaviors enables ranges
      which wrap around. *)

  val base: t -> elm
  (** Base element in range. *)

  val limit: t -> limit
  (** Range limit, i.e the index immediately past the last element in a half-open range, or the
      index of the last element in a full-open range. *)

  include ContainerIntf.SMonoIndex with type t := t with type elm := elm
  include ContainerIntf.SMonoMem with type t := t with type elm := elm
  include FormattableIntf.SMono with type t := t

  val length: t -> length
  (** Number of elements in range, or [Overflow] if a maximal full-open range. *)

  val length_hlt: t -> elm
  (** Number of elements in range. Halts on [Overflow]. *)
end
