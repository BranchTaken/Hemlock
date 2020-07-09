(** Lazy-evaluated immutable queue.

    The q is an immutable queue container.  All operations on q are amortized
    via lazy execution and are O(1).
*)

open Rudiments

(** {1 Type and derivations} *)

type 'a t

include Formattable_intf.S_poly with type 'a t := 'a t

(** {1 Creation} *)

val empty: 'a t
(** Return an empty q. *)

(** {1 Length} *)

val length: 'a t -> usize
(** Return q length. *)

val is_empty: 'a t -> bool
(** Return [true] if q length is 0; [false] otherwise. *)

(** {1 Element access} *)

val hd: 'a t -> 'a
(** Return head (first) q element, or halt if q is empty. *)

val tl: 'a t -> 'a t
(** Return tail (a q of all elements except head), or halt if q is empty. *)

(** {1 Combining and partitioning} *)

val push_back: 'a -> 'a t -> 'a t
(** Push element onto back of q and return resulting q with element as back. *)

val pop: 'a t -> 'a * 'a t
(** Pop head element off q and return the decomposed element and remainder q.
    Halt if the input q is empty. *)
