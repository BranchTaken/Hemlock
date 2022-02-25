(** Lazy-evaluated immutable stream.

    The stream is an immutable stack-like container. Operations on the stream that would typically
    be O(n) are amortized via lazy execution and (with a few noted exceptions) are O(1). *)

open Rudiments0

(** {1 Type and derivations} *)

(* ('a, >e) t *)
type 'a elm =
  | Nil
  | Cons of 'a * 'a t
and 'a t = 'a elm Lazy.t

include FormattableIntf.SPoly with type 'a t := 'a t

(** {1 Creation} *)

val empty: 'a t
(** Return an empty stream. *)

(* val init: range -> f:(uns >e-> 'a) -> ('a, >e) t *)
val init: range -> f:(uns -> 'a) -> 'a t
(** Initialize stream. [init range ~f:(fun i -> ...)] lazily initializes a stream of length
    [Range.Uns.length_hlt range], where [f] maps range elements to stream elements. *)

(* val init_indef: f:(/m 'state >e-> ('a * /m 'state) option) -> /m 'state -> ('a, >e) t *)
val init_indef: f:('state -> ('a * 'state) option) -> 'state -> 'a t
(** Initialize stream. [init_indef ~f:(fun state -> ...) state] lazily initializes a stream, where
    [f] provides the value and remaining initialization state for each subseqeunt element,
    continuing until [f] returns [None]. *)

(** {1 Length} *)

(* val length: ('a, >e) t >e-> uns *)
val length: 'a t -> uns
(** Return stream length. This forces and counts every node and is Θ(n). *)

(* val is_empty: ('a, >e) t >e-> bool  *)
val is_empty: 'a t -> bool
(** Return [true] if stream length is 0; [false] otherwise. *)

(** {1 Element access} *)

(* val hd: ('a, >e) t >e-> 'a *)
val hd: 'a t -> 'a
(** Return head (first) stream element, or halt if stream is empty. *)

(* val tl: ('a, >e) t >e-> ('a, >e) t *)
val tl: 'a t -> 'a t
(** Return tail (a stream of all elements except head), or halt if stream is empty. *)

(** {1 Combining and partitioning} *)

(* val push: 'a -> ('a, >e) t -> ('a, >e) t *)
val push: 'a -> 'a t -> 'a t
(** Push element onto stream and return resulting stream with element as head. *)

(* val pop: ('a, >e) t -> 'a * ('a, >e) t *)
val pop: 'a t -> 'a * 'a t
(** Pop head element off stream and return the decomposed element and remainder stream or halt if
    stream is empty. *)

(* val concat: ('a, >e) t -> ('a, >e) t -> ('a, >e) t *)
val concat: 'a t -> 'a t -> 'a t
(** Concatenate two streams. *)

(* val split: uns -> ('a, >e) t -> ('a, >e) t * ('a, >e) t *)
val split: uns -> 'a t -> 'a t * 'a t
(** Split the stream with elements [\[0..len)] into streams with elements [\[0..n)] and [\[n..len)].
    If stream contains fewer than [n] elements, returns a stream with elements [\[0..len)] and an
    empty stream. Split is O(1), but forcing the first element of the second returned stream is
    O(n). *)

(* val rev_split: uns -> ('a, >e) t -> ('a, >e) t * ('a, >e) t *)
val rev_split: uns -> 'a t -> 'a t * 'a t
(** Split the stream with elements [\[0..len)] into streams with elements [(n..0\]] and [\[n..len)].
    If stream contains fewer than [n] elements, returns a stream with elements [(len..0\]] and an
    empty stream. Split is O(1), but forcing the first element of either returned stream is O(n). *)

(* val take: uns -> ('a, >e) t -> ('a, >e) t *)
val take: uns -> 'a t -> 'a t
(** Return a new stream with the first [max n len] elements of the input stream. *)

(* val rev_take: uns -> ('a, >e) t -> ('a, >e) t *)
val rev_take: uns -> 'a t -> 'a t
(** Return a new stream with the first [max n len] elements of the input stream in reverse order.
    Forcing the first element of the returned stream is O(n). *)

(* val drop: uns -> ('a, >e) t -> ('a, >e) t *)
val drop: uns -> 'a t -> 'a t
(** Return a new stream without the first [max n len] elements of the input stream. Drop is O(1),
    but forcing the first element of the returned stream is O(n). *)

(** {1 Re-ordering} *)

(* val rev: ('a, >e) t -> ('a, >e) t *)
val rev: 'a t -> 'a t
(** Create a stream with elements reversed relative to the input stream. Forcing the first element
    of the returned stream is Θ(n). *)
