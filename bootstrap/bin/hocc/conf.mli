(** Command line configuration parameters. *)

open Basis

type algorithm =
  | Lr1 (** LR(1) algorithm. *)
  | Ielr1 (** IELR(1) algorithm. *)
  | Pgm1 (** PGM(1) algorithm. *)
  | Lalr1 (** LALR(1) algorithm. *)

val pp_algorithm: algorithm -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** [pp_algorithm algorithm] formats [algorithm]. *)

type t

include FormattableIntf.SMono with type t := t

val of_argv: Bytes.t array -> t
(** [of_argv argv] parses command line parameters, exits with a usage message on error, or returns
    the results of parsing if there are no errors. *)

val verbose: t -> bool
(** [verbose t] returns true if verbosity is enabled. *)

val text: t -> bool
(** [text t] returns true if a plain-text automoton description is to be generated. *)

val html: t -> bool
(** [html t] returns true if an html automoton description is to be generated. *)

val hocc: t -> bool
(** [hocc t] returns true if a hocc-format grammar specification is to be generated. *)

val algorithm: t -> algorithm
(** [algorithm t] returns the algorithm to be used when generating the automoton. *)

val resolve: t -> bool
(** [resolve t] returns true if conflict resolution is enabled. *)

val hemlock: t -> bool
(** [hemlock t] returns true if a Hemlock-based parser is to be generated. *)

val ocaml: t -> bool
(** [ocaml t] returns true if an OCaml-based parser is to be generated. *)

val srcdir: t -> Path.t
(** [srcdir t] returns the source directory path of the input file. *)

val module_: t -> Path.Segment.t
(** [module_ t] returns the module name corresponding to the input/output files. *)

val dstdir: t -> Path.t
(** [dstdir t] returns the destination directory path in which to place generated output. *)
