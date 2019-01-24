module type S = sig
  type t
  val of_string: string -> t
  val to_string: t -> string
end
