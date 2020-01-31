(** Functors for integers of specific bitwidth. *)

open Intnb_intf

(** Functor for signed integers of specific bitwidth. *)
module Make_i (T : I) : S_i with type t := isize

(** Functor for unsigned integers of specific bitwidth. *)
module Make_u (T : I) : S_u with type t := usize
