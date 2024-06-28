(** All hocc I/O is mediated by a single {!module:Io}.{!type:t} value which is threaded through the
    execution. Upon program exit, build artifacts are written only if parser generation is
    successful, i.e. invalid parser specifications do not result in partial outputs. Furthermore,
    depending on configuration some formatters may be irrelevant, and they are initialized to act as
    sinks so that they can be "written to" without effect, thus saving callers the trouble of
    conditional output logic. *)

open Basis

type t = {
  err: (module Fmt.Formatter);
  hmhi: Text.t option;
  hmh: Text.t;
  log: (module Fmt.Formatter);
  txt: (module Fmt.Formatter);
  html: (module Fmt.Formatter);
  hocc: (module Fmt.Formatter);
  hmi: (module Fmt.Formatter);
  hm: (module Fmt.Formatter);
  mli: (module Fmt.Formatter);
  ml: (module Fmt.Formatter);
}

val init: Conf.t -> t
(** [init conf] initializes formatters according to [conf]. *)

val fini: Conf.t -> t -> t
(** Write and flush results to files. *)

val fatal: t -> 'a
(** Flush error output and exit. *)

val with_err: t -> (module Fmt.Formatter) -> t
(** [with_err t err] is equivalent to [{t with err}]. *)

val with_log: t -> (module Fmt.Formatter) -> t
(** [with_log t log] flushes [log] and returns a [t] with updated [log]. *)

val with_txt: t -> (module Fmt.Formatter) -> t
(** [with_txt t txt] is equivalent to [{t with txt}]. *)

val with_html: t -> (module Fmt.Formatter) -> t
(** [with_html t html] is equivalent to [{t with html}]. *)

val with_hocc: t -> (module Fmt.Formatter) -> t
(** [with_hocc t hocc] is equivalent to [{t with hocc}]. *)

val with_hmi: t -> (module Fmt.Formatter) -> t
(** [with_hmi t hmi] is equivalent to [{t with hmi}]. *)

val with_hm: t -> (module Fmt.Formatter) -> t
(** [with_hm t hm] is equivalent to [{t with hm}]. *)

val with_mli: t -> (module Fmt.Formatter) -> t
(** [with_mli t mli] is equivalent to [{t with mli}]. *)

val with_ml: t -> (module Fmt.Formatter) -> t
(** [with_ml t ml] is equivalent to [{t with ml}]. *)
