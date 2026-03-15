(** Command line configuration parameters. *)

open Basis

type algorithm =
  | Aplr (** APLR(1) algorithm. *)
  | Ielr (** IELR(1) algorithm. *)
  | Pgm (** PGM algorithm. *)
  | Lr (** LR(1) algorithm. *)
  | Lalr (** LALR(1) algorithm. *)

type remerge =
  | Default of bool (** Default; no remerge parameter specified. *)
  | Explicit of bool (** Explicit remerge parameter specified. *)

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
(** [text t] returns true if a plain-text automaton description is to be generated. *)

val hocc: t -> bool
(** [hocc t] returns true if a hocc-format grammar specification is to be generated. *)

val algorithm: t -> algorithm
(** [algorithm t] returns the algorithm to be used when generating the automaton. *)

val resolve: t -> bool
(** [resolve t] returns true if conflict resolution is enabled. *)

val gc: t -> bool
(** [gc t] returns true if unreachable state garbage collection is enabled. *)

val remerge: t -> remerge
(** [remerge t] returns a [Default]/[Explicit] remerging parameter, true if remerging of equivalent
    split states is enabled. *)

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
