(** Command line configuration parameters. *)

open Basis

type algorithm =
  | Lr1
  | Ielr1
  | Pgm1
  | Lalr1

val pp_algorithm: algorithm -> (module Fmt.Formatter) -> (module Fmt.Formatter)

type t

include FormattableIntf.SMono with type t := t

val of_argv: Bytes.t array -> t

val verbose: t -> bool
val text: t -> bool
val html: t -> bool
val hocc: t -> bool
val algorithm: t -> algorithm
val resolve: t -> bool
val hemlock: t -> bool
val ocaml: t -> bool
val srcdir: t -> Path.t
val module_: t -> Path.Segment.t
val dstdir: t -> Path.t
