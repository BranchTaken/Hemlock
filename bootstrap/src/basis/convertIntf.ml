(** Integer conversion function interfaces. Actual integer conversion function names replace
    [_u_]/[_x_] with e.g. [_u256_]/[_i512_]. *)

open RudimentsInt0

(** Conversion function interface for a signed integer type of/to a larger signed integer type. *)
module type IX = sig
  type t
  (** Signed integer type. *)

  type x
  (** Signed integer type larger than {!type:t}. *)

  val trunc_of_x: x -> t
  (** [trunc_of_x x] converts the least sigificant bits of [x] to a {!type:t} value. *)

  val extend_to_x: t -> x
  (** [extend_to_x t] converts [t] to an equivalent {!type:x} value. *)

  val narrow_of_x_opt: x -> t option
  (** [narrow_of_x_opt x] converts [x] to an equivalent {!type:t} value, or [None] if [x] is outside
      the {!type:t} range. *)

  val narrow_of_x_hlt: x -> t
  (** [narrow_of_x_hlt x] converts [x] to an equivalent {!type:t} value, or halts if [x] is outside
      the {!type:t} range. *)
end

(** Conversion function interface for a signed integer type of/to a larger unsigned integer type. *)
module type IU = sig
  type t
  (** Signed integer type. *)

  type u
  (** Unsigned integer type larger than {!type:t}. *)

  val trunc_of_u: u -> t
  (** [trunc_of_u u] converts the least sigificant bits of [u] to a {!type:t} value. *)

  val narrow_of_u_opt: u -> t option
  (** [narrow_of_u_opt u] converts [u] to an equivalent {!type:t} value, or [None] if [u] is outside
      the {!type:t} range. *)

  val widen_to_u_opt: t -> u option
  (** [widen_to_u_opt t] converts [t] to an equivalent {!type:u} value, or [None] if [t] is outside
      the {!type:u} range. *)

  val narrow_of_u_hlt: u -> t
  (** [narrow_of_u_hlt u] converts [u] to an equivalent {!type:t} value, or halts if [u] is outside
      the {!type:t} range. *)

  val widen_to_u_hlt: t -> u
  (** [widen_to_u_hlt t] converts [t] to an equivalent {!type:u} value, or halts if [t] is outside
      the {!type:u} range. *)
end

(** Conversion function interface for an unsigned integer type of/to a larger signed integer type.
*)
module type UX = IX

(** Conversion function interface for an unsigned integer type of/to a larger unsigned integer type.
*)
module type UU = sig
  type t
  (** Unsigned integer type. *)

  type u
  (** Unsigned integer type larger than {!type:t}. *)

  val trunc_of_u: u -> t
  (** [trunc_of_u u] converts the least sigificant bits of [u] to a {!type:t} value. *)

  val extend_to_u: t -> u
  (** [extend_to_u t] converts [t] to an equivalent {!type:u} value. *)

  val narrow_of_u_opt: u -> t option
  (** [narrow_of_u_opt u] converts [u] to an equivalent {!type:t} value, or [None] if [u] is outside
      the {!type:t} range. *)

  val narrow_of_u_hlt: u -> t
  (** [narrow_of_u_hlt u] converts [u] to an equivalent {!type:t} value, or halts if [u] is outside
      the {!type:t} range. *)
end

(** Conversion function interface for an unsigned integer type of/to a same-width signed integer
    type. *)
module type UI = sig
  type t
  (** Unsigned integer type. *)

  type x
  (** Signed integer type larger than {!type:t}. *)

  val bits_of_x: x -> t
  (** [bits_of_x x] converts [x] to a bitwise identical {!type:t} value. *)

  val bits_to_x: t -> x
  (** [bits_to_x t] converts [t] to a bitwise identical {!type:x} value. *)

  val like_of_x_opt: x -> t option
  (** [like_of_x_opt x] converts [x] to an equivalent {!type:t} value, or [None] if [x] is outside
      the {!type:t} range. *)

  val like_to_x_opt: t -> x option
  (** [like_to_x_opt t] converts [t] to an equivalent {!type:x} value, or [None] if [t] is outside
      the {!type:x} range. *)

  val like_of_x_hlt: x -> t
  (** [like_of_x_hlt x] converts [x] to an equivalent {!type:t} value, or halts if [x] is outside
      the {!type:t} range. *)

  val like_to_x_hlt: t -> x
  (** [like_to_x_hlt t] converts [t] to an equivalent {!type:x} value, or halts if [t] is outside
      the {!type:x} range. *)
end

(** Conversion function interface for an integer type smaller than [uns]/[sint] of/to [uns]/[sint].
*)
module type Nb = sig
  type t
  (** Integer type smaller than [uns]/[sint]. *)

  val trunc_of_sint: sint -> t
  (** [trunc_of_sint x] converts the least sigificant bits of [x] to a {!type:t} value. *)

  val extend_to_sint: t -> sint
  (** [extend_to_sint t] converts [t] to an equivalent {!type:sint} value. *)

  val narrow_of_sint_opt: sint -> t option
  (** [narrow_of_sint_opt x] converts [x] to an equivalent {!type:t} value, or [None] if [x] is
      outside the {!type:t} range. *)

  val narrow_of_sint_hlt: sint -> t
  (** [narrow_of_sint_hlt x] converts [x] to an equivalent {!type:t} value, or halts if [x] is
      outside the {!type:t} range. *)

  val kv: int64 -> t
  (** Create constant value. This is a stopgap solution for the lack of bitwidth-specific literals.
  *)

  val trunc_of_uns: uns -> t
  (** [trunc_of_uns u] converts the least sigificant bits of [u] to a {!type:t} value. *)

  val extend_to_uns: t -> uns
  (** [extend_to_uns t] converts [t] to an equivalent {!type:uns} value, with the nuance that if
      {!type:t} is a signed type, the conversion first performs sign extension, i.e.
      [Uns.bits_of_sint (extend_to_sint t)]. *)

  val narrow_of_uns_opt: uns -> t option
  (** [narrow_of_uns_opt u] converts [u] to an equivalent {!type:t} value, or [None] if [u] is
      outside the {!type:t} range. *)

  val narrow_of_uns_hlt: uns -> t
  (** [narrow_of_uns_hlt u] converts [u] to an equivalent {!type:t} value, or halts if [u] is
      outside the {!type:t} range. *)
end
