(** Formattable type functor signature. *)

(** Monomorphic Format pretty printing conversion functions. *)
module type SMono = sig
  type t

  val pp: Format.formatter -> t -> unit
  (** [pp ppf t] prints a representation of [t] to the pretty printing formatter, [ppf]. This
      function is intended for use with the [%a] format specifier to {!Format.printf}.*)

  val fmt: t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
  (** [fmt t formatter] applies a formatted representation of [t] to the [formatter]. *)
end

(** Polymorphic Format pretty printing conversion functions. *)
module type SPoly = sig
  type 'a t

  val pp: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  (** [pp pp_a ppf t] prints a syntactic representation of [t] to the pretty printing formatter,
      [ppf], using the [pp_a] printer for the parametric type value [a]. This function is intended
      for use with the [%a] format specifier to {!Format.printf}. *)

  val fmt: ('a -> (module Fmt.Formatter) -> (module Fmt.Formatter)) -> 'a t
    -> (module Fmt.Formatter) -> (module Fmt.Formatter)
    (** [fmt fmt_a t formatter] applies a formatted representation of [t] to the [formatter] using
        [fmt_a] for the parametric type value [a]. *)
end

(** Polymorphic Format pretty printing conversion functions. *)
module type SPoly2 = sig
  type ('a, 'b) t

  val pp: (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) ->
    Format.formatter -> ('a, 'b) t -> unit
  (** [pp pp_a pp_b ppf t] prints a syntactic representation of [t] to the pretty printing
      formatter, [ppf], using the [pp_a] printer for the parametric type value [a], and [pp_b] for
      the parametric type value [b]. This function is intended for use with the [%a] format
      specifier to {!Format.printf}. *)

  val fmt: ('a -> (module Fmt.Formatter) -> (module Fmt.Formatter))
    -> ('b -> (module Fmt.Formatter) -> (module Fmt.Formatter)) -> ('a, 'b) t ->
    (module Fmt.Formatter) -> (module Fmt.Formatter)
    (** [fmt fmt_a fmt_b t formatter] applies a formatted representation of [t] to the [formatter]
        using [fmt_a] for the parametric type value [a], and [fmt_b] for the parametric type value
        [b]. *)
end
