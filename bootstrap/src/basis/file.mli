open Rudiments

module Error : sig
  (* type t *)
  type t

  (* val to_string: t $-> string *)
  val to_string: t -> String.t
end

module Flag: sig
  (* type t *)
  type t =
    | R_O   (** Open existing file for read or fail if it does not exist. *)
    | W     (** Create new or truncate existing file and open for write. *)
    | W_A   (** Create new or append to existing file and open for write. *)
    | W_AO  (** Append to existing file and open for write, failing if it does not exist. *)
    | W_C   (** Create new file and open for write, failing if file exists *)
    | W_O   (** Truncate existing file and open for write, failing if file does not exist. *)
    | RW    (** Create new or truncate existing file and open for read and write. *)
    | RW_A  (** Create new or append to existing file and open for read and write. *)
    | RW_AO (** Append to existing file and open for read and write, failing if file does not exist.
            *)
    | RW_C  (** Create new file and open for read and write, failing if file exists. *)
    | RW_O  (** Truncate existing file and open for read and write, failing if file does not exist.
            *)
end

(*
module Buffer: sig
  (* type /t t *)
  type t

  (* val of_bytes: byte /t array -> /t t *)
  val of_bytes: Bytes.t -> t

  (* val of_string: string -> /t t *)
  val of_string: String.t -> t
end
*)

(* type t *)
type t
(** An internally immutable token backed by an external, mutable Unix file descriptor. *)

(* val stdin: t *)
val stdin: t

(* val stdout: t *)
val stdout: t

(* val stderr: t *)
val stderr: t

(* val of_path: ?flag:Flag.t -> ?mode:uns -> /t Bytes.Slice.t $-> (t, Error.t) result *)
val of_path: ?flag:Flag.t -> ?mode:uns -> Bytes.Slice.t -> (t, Error.t) result
(** [of_path ~flag ~mode path] opens or creates the file at [path] with [flag] (default Flag.RW)
    Unix file permissions and [mode] (default 0o660) Unix file permissions and and returns the
    resulting file or an error if the file could not be opened. *)

(* val of_path_hlt: ?flag:Flag.t -> ?mode:uns -> Bytes.Slice.t $-> t *)
val of_path_hlt: ?flag:Flag.t -> ?mode:uns -> Bytes.Slice.t -> t
(** [of_path_hlt ~flag ~mode path] opens or creates the file at [path] with [flag] (default Flag.RW)
    Unix file permissions and [mode] (default 0o660) Unix file permissions and and returns the
    resulting file or halts if the file could not be opened. *)

(* val read_into: !&Bytes.Slice.t -> t $-> Error.t option *)
val read_into: Bytes.Slice.t -> t -> Error.t option
(** [read_into buffer t] reads up to n bytes from [t] into the given mutable [buffer], where n is
    the size of the buffer. Returns an error if bytes could not be read. *)

(* val read_into_hlt: !&Bytes.Slice.t -> t $-> unit *)
val read_into_hlt: Bytes.Slice.t -> t -> unit
(** [read_into_hlt buffer t] reads up to n bytes from [t] into the given mutable [buffer], where n
    is the size of the buffer. Halts if bytes could not be read. *)

(* val read: t $-> (/t Bytes.Slice.t, Error.t) result *)
val read: ?n:uns -> t -> (Bytes.Slice.t, Error.t) result
(** [read ~n t] reads up to [n] (default 1024) bytes from [t] into a new buffer and returns the
    buffer or an error if bytes could not be read. *)

(* val read_hlt: t $-> /t Bytes.Slice.t *)
val read_hlt: ?n:uns -> t -> Bytes.Slice.t
(** [read_hlt n t] reads up to [n] (default 1024) bytes from [t] into a new buffer and returns the
    buffer or halts if bytes could not be read. *)

(* val write: /_ Bytes.Slice.t -> t $-> Error.t option *)
val write: Bytes.Slice.t -> t -> Error.t option
(** [write bytes t] writes [bytes] to [t] and returns None or an error if bytes could not be
    written. *)

(* val write_hlt: /_ Bytes.Slice.t -> t $-> unit *)
val write_hlt: Bytes.Slice.t -> t -> unit
(** [write_hlt bytes t] writes [bytes] to [t] and returns a unit or halts if bytes could not be
    written. *)

(* val close: t $-> Error.t option *)
val close: t -> Error.t option
(** [close t] closes the external mutable Unix file descriptor associated with [t] and returns None
    or an error if it could not be closed. *)

(* val close_hlt: t $-> unit *)
val close_hlt: t -> unit
(** [close_hlt t] closes the external mutable Unix file descriptor associated with [t] and returns a
    unit or halts if it could not be closed. *)

(* val seek: sint -> t $-> (uns, Error.t) result *)
val seek: sint -> t -> (uns, Error.t) result
(** [seek i t] seeks the external mutable Unix file descriptor associated with [t] to point to the
    [i]th byte relative to the current byte position of the file. Returns the new byte index
    relative to the beginning of the file or an error if it could not be changed. *)

(* val seek_hlt: sint -> t $-> uns *)
val seek_hlt: sint -> t -> uns
(** [seek_hlt i t] seeks the external mutable Unix file descriptor associated with [t] to point to
    the [i]th byte relative to the current byte position of the file. Returns the new byte index
    relative to the beginning of the file or halts if it could not be changed. *)

(* val seek_hd: sint -> t $-> (uns, Error.t) result *)
val seek_hd: sint -> t -> (uns, Error.t) result
(** [seek_hd i t] seeks the external mutable Unix file descriptor associated with [t] to point to
    the [i]th byte relative to the head of the file. Returns the new byte index relative to the
    beginning of the file or an error if it could not be changed. *)

(* val seek_hd_hlt: sint -> t $-> uns *)
val seek_hd_hlt: sint -> t -> uns
(** [seek_hd_hlt i t] seeks the external mutable Unix file descriptor associated with [t] to point
    to the [i]th byte relative to the head of the file. Returns the new byte index relative to the
    beginning of the file or halts if it could not be changed. *)

(* val seek_tl: sint -> t $-> (uns, Error.t) result *)
val seek_tl: sint -> t -> (uns, Error.t) result
(** [seek_tl i t] seeks the external mutable Unix file descriptor associated with [t] to point to
    the [i]th byte relative to the tail of the file. Returns the new byte index relative to the
    beginning of the file or an error if it could not be changed. *)

(* val seek_tl_hlt: ?i:sint -> t $-> uns *)
val seek_tl_hlt: sint -> t -> uns
(** [seek_tl_hlt i t] seeks the external mutable Unix file descriptor associated with [t] to point
    to the [i]th byte relative to the tail of the file. Returns the new byte index relative to the
    beginning of the file or halts if it could not be changed. *)

module Stream : sig
  (* type file = t *)
  type file = t

  (* type /t $t = (/t Bytes.Slice.t, >_) Stream.t *)
  type t = Bytes.Slice.t Stream.t

  (* val of_file: file -> /t $t *)
  val of_file: file -> t
  (** [of_file file] takes a [file] and returns a [t], a lazily initialized buffer stream that reads
      subsequent chunks of [file] into buffers when forced. *)

  (* val write: file -> /t $t $-> Error.t option *)
  val write: file -> t -> Error.t option
  (** [write file t] takes an open [file] with write permissions and writes, in order, all buffers
      from [t] to it. Returns an error if not all bytes could be written. *)

  (* val write_hlt: file -> /t $t $-> unit *)
  val write_hlt: file -> t -> unit
  (** [write_hlt file t] takes an open [file] with write permissions and writes, in order, all
      buffers from [t] to it. Halts if not all bytes could be written. *)
end
