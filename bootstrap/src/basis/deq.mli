(** Lazy-evaluated immutable double-ended queue.

    The deq is an immutable double-ended queue container.  All operations on deq
    are amortized via lazy execution and are O(1). *)

open Rudiments

(** {1 Type and derivations} *)

type 'a t

include Formattable_intf.S_poly with type 'a t := 'a t

(** {1 Creation} *)

val empty: 'a t
(** Return an empty deq. *)

(** {1 Length} *)

val length: 'a t -> usize
(** Return deq length. *)

val is_empty: 'a t -> bool
(** Return [true] if deq length is 0; [false] otherwise. *)

(** {1 Element access} *)

val hd: 'a t -> 'a
(** Return head (first) deq element, or halt if deq is empty. *)

val tl: 'a t -> 'a t
(** Return tail (a deq of all elements except head), or halt if deq is empty. *)

val back: 'a t -> 'a
(** Return back (last) deq element, or halt if deq is empty. *)

val front: 'a t -> 'a t
(** Return front (a deq of all elements except back), or halt if deq is empty.
*)

(** {1 Combining and partitioning} *)

val push: 'a -> 'a t -> 'a t
(** Push element onto head of deq and return resulting deq with element as head.
*)

val push_back: 'a -> 'a t -> 'a t
(** Push element onto back of deq and return resulting deq with element as back.
*)

val pop: 'a t -> 'a * 'a t
(** Pop head element off deq and return the decomposed element and remainder
    deq. Halt if the input deq is empty. *)

val pop_back: 'a t -> 'a * 'a t
(** Pop back element off deq and return the decomposed element and remainder
    deq. Halt if the input deq is empty. *)
