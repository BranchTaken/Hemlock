(** Formattable type functor signature. *)

(** Format pretty printing conversion functions. *)
module type S = sig
  type t

  val pp: Format.formatter -> t -> unit
  (** [pp ppf t] prints a representation of [t] to the pretty printing
      formatter, [ppf].  This function is intended for use with the [%a] format
      specifier to {!Format.printf}.*)
end

