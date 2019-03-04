module type I_def = sig
  type t
  type elm
  val length: t -> int
  val next: t -> elm * t
end

module type I_indef = sig
  type t
  type elm
  val next: t -> (elm * t) option
end

module type S_def = sig
  include I_def
  val next_opt: t -> (elm * t) option
end

module type S_indef = sig
  include I_indef
end
