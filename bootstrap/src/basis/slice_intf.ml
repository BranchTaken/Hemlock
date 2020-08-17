open Rudiments

module type S_mono = sig
  type container
  type cursor
  type elm
  type t

  val of_cursors: base:cursor -> past:cursor -> t

  val to_cursors: t -> cursor * cursor

  val container: t -> container

  val base: t -> cursor

  val past: t -> cursor

  val of_container: container -> t

  val to_container: t -> container

  val base_seek: sint -> t -> t

  val base_succ: t -> t

  val base_pred: t -> t

  val past_seek: sint -> t -> t

  val past_succ: t -> t

  val past_pred: t -> t

  val get: uns -> t -> elm
end