open Rudiments0

module type SMonoIter = sig
  type container
  type cursor
  type elm
  type t

  val init: ?base:cursor -> ?past:cursor -> container -> t
  (** [init ~base ~past container] creates a slice enclosing \[[base .. past)] of [container].
      \[[base .. past)] defaults to the full range of [container]. *)

  val container: t -> container
  (** [container t] returns the unsliced container underlying [t]. *)

  val base: t -> cursor
  (** Return the cursor at the base of the slice. *)

  val past: t -> cursor
  (** Return the cursor past the end of the slice. *)

  val cursors: t -> cursor * cursor
  (** Return the cursors enclosing the slice. *)
end

module type SMonoIndex = sig
  type container
  type cursor
  type elm
  type t

  val init: ?range:range -> container -> t
  (** [init ~range container] returns a slice enclosing [range] of [container]. [range] defaults to
      the full range of [container]. *)

  val of_cursors: base:cursor -> past:cursor -> t
  (** [of_cursors ~base ~past] creates a slice with contents \[[base .. past)]. *)

  val container: t -> container
  (** [container t] returns the unsliced container underlying [t]. *)

  val range: t -> range
  (** Return the range of indices contained in the slice. *)

  val length: t -> uns
  (** [length t] returns the length of the slice, equivalent to [Range.Uns.length_hlt (range t)]. *)

  val base: t -> cursor
  (** Return the cursor at the base of the slice. *)

  val past: t -> cursor
  (** Return the cursor past the end of the slice. *)

  val cursors: t -> cursor * cursor
  (** Return the cursors enclosing the slice. *)
end

module type SPolyIter = sig
  type 'a container
  type 'a cursor
  type 'a elm
  type 'a t

  val init: ?base:'a cursor -> ?past:'a cursor -> 'a container -> 'a t
  (** [init ~base ~past container] creates a slice enclosing \[[base .. past)] of [container].
      \[[base .. past)] defaults to the full range of [container]. *)

  val container: 'a t -> 'a container
  (** [container t] returns the unsliced container underlying [t]. *)

  val base: 'a t -> 'a cursor
  (** Return the cursor at the base of the slice. *)

  val past: 'a t -> 'a cursor
  (** Return the cursor past the end of the slice. *)

  val cursors: 'a t -> 'a cursor * 'a cursor
  (** Return the cursors enclosing the slice. *)
end

module type SPolyIndex = sig
  type 'a container
  type 'a cursor
  type 'a elm
  type 'a t

  val init: ?range:range -> 'a container -> 'a t
  (** [init ~range container] returns a slice enclosing [range] of [container]. [range] defaults to
      the full range of [container]. *)

  val of_cursors: base:'a cursor -> past:'a cursor -> 'a t
  (** [of_cursors ~base ~past] creates a slice with contents \[[base .. past)]. *)

  val container: 'a t -> 'a container
  (** [container t] returns the unsliced container underlying [t]. *)

  val range: 'a t -> range
  (** Return the range of indices contained in the slice. *)

  val length: 'a t -> uns
  (** [length t] returns the length of the slice, equivalent to [Range.Uns.length_hlt (range t)]. *)

  val base: 'a t -> 'a cursor
  (** Return the cursor at the base of the slice. *)

  val past: 'a t -> 'a cursor
  (** Return the cursor past the end of the slice. *)

  val cursors: 'a t -> 'a cursor * 'a cursor
  (** Return the cursors enclosing the slice. *)
end
