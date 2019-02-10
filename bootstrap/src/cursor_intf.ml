(* Partial Rudiments. *)
module Int = I63
type int = Int.t

module type S_iter = sig
  type container
  type elm
  type t
  include Cmpable_intf.S with type t := t
  val hd: container -> t
  val tl: container -> t
  val succ: t -> t
  val pred: t -> t
  val lget: t -> elm
  val rget: t -> elm
end

module type S = sig
  include S_iter
  val container: t -> container
  val index: t -> int
  val seek: t -> int -> t
end
