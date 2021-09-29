open Rudiments0

module type SMono = sig
  type container
  type cursor
  type elm
  type t

  val of_cursors: base:cursor -> past:cursor -> t
  (** [of_cursors ~base ~past] creates a slice with contents \[[base .. past)]. *)

  val to_cursors: t -> cursor * cursor
  (** Return the cursors comprising the slice. *)

  val container: t -> container
  (** [container t] returns the unsliced container underlying [t]. *)

  val base: t -> cursor
  (** Return the cursor at the base of the slice. *)

  val past: t -> cursor
  (** Return the cursor past the end of the slice. *)

  val of_container: container -> t
  (** [of_container c] returns a slice enclosing the entirety of [c]. *)

  val to_container: t -> container
  (** Return a container with contents equivalent to those of the slice. *)

  val base_seek: sint -> t -> t
  (** [base_seek i t] returns a derivative slice with its [base] cursor initialized by seeking [t]'s
      [base] cursor [i] elements forward/backward. *)

  val base_succ: t -> t
  (** [base_succ t] returns a derivative slice with its [base] cursor initialized to the successor
      of [t]'s [base] cursor. *)

  val base_pred: t -> t
  (** [base_pred t] returns a derivative slice with its [base] cursor initialized to the predecessor
      of [t]'s [base] cursor. *)

  val past_seek: sint -> t -> t
  (** [past_seek i t] returns a derivative slice with its [past] cursor initialized by seeking [t]'s
      [past] cursor [i] elements forward/backward. *)

  val past_succ: t -> t
  (** [past_succ t] returns a derivative slice with its [past] cursor initialized to the successor
      of [t]'s [past] cursor. *)

  val past_pred: t -> t
  (** [past_pred t] returns a derivative slice with its [past] cursor initialized to the predecessor
      of [t]'s [past] cursor. *)
end

module type SPoly = sig
  type 'a container
  type 'a cursor
  type 'a elm
  type 'a t

  val of_cursors: base:'a cursor -> past:'a cursor -> 'a t
  (** [of_cursors ~base ~past] creates a slice with contents \[[base .. past)]. *)

  val to_cursors: 'a t -> 'a cursor * 'a cursor
  (** Return the cursors comprising the slice. *)

  val container: 'a t -> 'a container
  (** [container t] returns the unsliced container underlying [t]. *)

  val base: 'a t -> 'a cursor
  (** Return the cursor at the base of the slice. *)

  val past: 'a t -> 'a cursor
  (** Return the cursor past the end of the slice. *)

  val of_container: 'a container -> 'a t
  (** [of_container c] returns a slice enclosing the entirety of [c]. *)

  val to_container: 'a t -> 'a container
  (** Return a container with contents equivalent to those of the slice. *)

  val base_seek: sint -> 'a t -> 'a t
  (** [base_seek i t] returns a derivative slice with its [base] cursor initialized by seeking [t]'s
      [base] cursor [i] elements forward/backward. *)

  val base_succ: 'a t -> 'a t
  (** [base_succ t] returns a derivative slice with its [base] cursor initialized to the successor
      of [t]'s [base] cursor. *)

  val base_pred: 'a t -> 'a t
  (** [base_pred t] returns a derivative slice with its [base] cursor initialized to the predecessor
      of [t]'s [base] cursor. *)

  val past_seek: sint -> 'a t -> 'a t
  (** [past_seek i t] returns a derivative slice with its [past] cursor initialized by seeking [t]'s
      [past] cursor [i] elements forward/backward. *)

  val past_succ: 'a t -> 'a t
  (** [past_succ t] returns a derivative slice with its [past] cursor initialized to the successor
      of [t]'s [past] cursor. *)

  val past_pred: 'a t -> 'a t
  (** [past_pred t] returns a derivative slice with its [past] cursor initialized to the predecessor
      of [t]'s [past] cursor. *)
end
