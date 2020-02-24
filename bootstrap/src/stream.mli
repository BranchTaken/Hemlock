(** Lazy-evaluated immutable stream.

    The stream is an immutable stack-like container.  Operations on the stream
    that would typically be O(n) are amortized via lazy execution and (with a
    few noted exceptions) are O(1).
*)

open Rudiments

(** {1 Type and derivations} *)

type 'a elm =
  | Nil
  | Cons of 'a * 'a t
and 'a t = 'a elm Lazy.t

include Formattable_intf.S_poly with type 'a t := 'a t

(** {1 Creation} *)

val empty: 'a t
(** Return an empty stream. *)

val init: usize -> f:(usize -> 'a) -> 'a t
(** Initialize stream.  [init len ~f:(fun i -> ...)] lazily initializes a stream
    of given length, where [f] provides the value for each element at given
    index. *)

(** {1 Length} *)

val length: 'a t -> usize
(** Return stream length.  This forces and counts every node and is Θ(n). *)

val is_empty: 'a t -> bool
(** Return [true] if stream length is 0; [false] otherwise. *)

(** {1 Element access} *)

val hd: 'a t -> 'a
(** Return head (first) stream element, or halt if stream is empty. *)

val tl: 'a t -> 'a t
(** Return tail (a stream of all elements except head), or halt if list is
    empty. *)

(** {1 Combining and partitioning} *)

val push: 'a -> 'a t -> 'a t
(** Push element onto stream and return resulting stream with element as head.
*)

val pop: 'a t -> 'a * 'a t
(** Pop head element off stream and return the decomposed element and remainder
    stream.  Halt if the input list is empty. *)

val concat: 'a t -> 'a t -> 'a t
(** Concatenate two streams. *)

val split: 'a t -> usize -> 'a t * 'a t
(** Split the stream with elements [\[0..len)] into streams with elements
    [\[0..n)] and [\[n..len)].  If stream contains fewer than [n] elements,
    returns a stream with elements [\[0..len)] and an empty stream.  Split is
    O(1), but forcing the first element of the second returned stream is O(n).
*)

val rev_split: 'a t -> usize -> 'a t * 'a t
(** Split the stream with elements [\[0..len)] into streams with elements
    [(n..0\]] and [\[n..len)].  If stream contains fewer than [n] elements,
    returns a stream with elements [(len..0\]] and an empty stream.  Split is
    O(1), but forcing the first element of either returned stream is O(n). *)

val take: 'a t -> usize -> 'a t
(** Return a new stream with the first [max n len] elements of the input stream.
*)

val rev_take: 'a t -> usize -> 'a t
(** Return a new stream with the first [max n len] elements of the input stream
    in reverse order.  Forcing the first element of the returned stream is O(n).
*)

val drop: 'a t -> usize -> 'a t
(** Return a new stream without the first [max n len] elements of the input
    stream.  Drop is O(1), but forcing the first element of the returned stream
    is O(n). *)

(** {1 Re-ordering} *)

val rev: 'a t-> 'a t
(** Create a stream with elements reversed relative to the input stream.
    Forcing the first element of the returned stream is Θ(n). *)
