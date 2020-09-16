open Rudiments0

module type SMono = sig
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
end

module type SPoly = sig
  type 'a container
  type 'a cursor
  type 'a elm
  type 'a t

  val of_cursors: base:'a cursor -> past:'a cursor -> 'a t

  val to_cursors: 'a t -> 'a cursor * 'a cursor

  val container: 'a t -> 'a container

  val base: 'a t -> 'a cursor

  val past: 'a t -> 'a cursor

  val of_container: 'a container -> 'a t

  val to_container: 'a t -> 'a container

  val base_seek: sint -> 'a t -> 'a t

  val base_succ: 'a t -> 'a t

  val base_pred: 'a t -> 'a t

  val past_seek: sint -> 'a t -> 'a t

  val past_succ: 'a t -> 'a t

  val past_pred: 'a t -> 'a t
end
