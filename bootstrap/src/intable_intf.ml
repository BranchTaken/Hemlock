module type S = sig
  type t
  val of_int: int -> t
  val to_int: t -> int
end
