open Rudiments

(** open(2) flags. Prefer the convenience flags (no [O_] prefix) for typical use cases. *)
module Flag: sig
  type t =
    | R_O
    (** Open existing file for read or fail if it does not exist ([[O_RDONLY]]). *)
    | W
    (** Create new or truncate existing file and open for write ([[O_WRONLY; O_CREAT; O_TRUNC]]). *)
    | W_A
    (** Create new or append to existing file and open for write ([[O_WRONLY; O_APPEND; O_CREAT]]).
    *)
    | W_AO
    (** Append to existing file and open for write, failing if it does not exist ([[O_WRONLY;
        O_APPEND]]). *)
    | W_C
    (** Create new file and open for write, failing if file exists ([[O_WRONLY; O_CREAT; O_EXCL]]).
    *)
    | W_O
    (** Truncate existing file and open for write, failing if file does not exist ([[O_WRONLY;
        O_TRUNC]]). *)
    | RW
    (** Create new or truncate existing file and open for read and write ([[O_RDWR; O_CREAT;
        O_TRUNC]]). *)
    | RW_A
    (** Create new or append to existing file and open for read and write ([[O_RDWR; O_APPEND;
        O_CREAT]]). *)
    | RW_AO
    (** Append to existing file and open for read and write, failing if file does not exist
        ([[O_RDWR; O_APPEND]]). *)
    | RW_C
    (** Create new file and open for read and write, failing if file exists ([[O_RDWR; O_CREAT;
        O_EXCL]]). *)
    | RW_O
    (** Truncate existing file and open for read and write, failing if file does not exist
        ([[O_RDWR; O_TRUNC]]). *)
    | O_RDONLY (** Read-only. *)
    | O_WRONLY (** Write-only. *)
    | O_RDWR (** Read-write. Distinct from [[O_RDONLY; O_WRONLY]], which is invalid. *)
    | O_APPEND (** Append. *)
    | O_CLOEXEC (** Close-on-exec. *)
    | O_CREAT (** Create. *)
    | O_DIRECT (** Direct (bypass virtual memory buffer cache). *)
    | O_DIRECTORY (** Require path to refer to a directory. *)
    | O_EXCL (** Require file to be created as side effect; fail otherwise. *)
    | O_NOATIME (** Do not update file access time. *)
    | O_NOCTTY (** Prevent terminal device from becoming process's controlling terminal. *)
    | O_NOFOLLOW (** Do not follow symlink in basename of path. *)
    | O_PATH (** Limit valid operations on file to path queries. *)
    | O_TMPFILE (** Create unnamed temporary file. *)
    | O_TRUNC (** Truncate to length 0. *)
end

type t
(** An internally immutable token backed by an external, mutable Unix file descriptor. *)

val stdin: t

val stdout: t

val stderr: t

val fd: t -> uns
(** [fd t] returns the Unix file descriptor corresponding to [t]. *)

module Open: sig
  type file = t
  type t
  (* An internally immutable token backed by an external I/O open completion data structure. *)

  val submit: ?flags:Flag.t list -> ?mode:uns -> Path.t -> (t, Errno.t) result
  (** [submit ~flags ~mode path] submits an open operation for a file at [path] with [flags]
      (default [[Flag.R_O]]) Unix file permissions and [mode] (default 0o660) Unix file permissions.
      This operation does not block. Returns a [t] to the open submission or an [Errno.t] if the
      open could not be submitted. *)

  val submit_hlt: ?flags:Flag.t list -> ?mode:uns -> Path.t -> t
  (** [submit ~flags ~mode path] submits an open operation for a file at [path] with [flags]
      (default [[Flag.R_O]]) Unix file permissions and [mode] (default 0o660) Unix file permissions.
      This operation does not block. Returns a [t] to the open submission or halts if the open could
      not be submitted. *)

  val complete: t -> (file, Errno.t) result
  (** [complete t] blocks until given [t] is complete. Returns a [file] or an [Errno.t] if the file
      could not be opened. *)

  val complete_hlt: t -> file
  (** [complete_hlt t] blocks until given [t] is complete. Returns a [file] or halts if the file
      could not be opened. *)
end

val of_path: ?flags:Flag.t list -> ?mode:uns -> Path.t -> (t, Errno.t) result
(** [of_path ~flags ~mode path] opens or creates the file at [path] with [flags] (default
    [[Flag.R_O]]) Unix file access and [mode] (default 0o660) Unix file permissions and returns the
    resulting [t] or an [Errno.t] if the file could not be opened. *)

val of_path_hlt: ?flags:Flag.t list -> ?mode:uns -> Path.t -> t
(** [of_path_hlt ~flags ~mode path] opens or creates the file at [path] with [flags] (default
    [[Flag.R_O]]) Unix file access and [mode] (default 0o660) Unix file permissions and returns the
    resulting [t] or halts if the file could not be opened. *)

module Close: sig
  type file = t
  type t
  (* An internally immutable token backed by an external I/O close completion data structure. *)

  val submit: file -> (t, Errno.t) result
  (** [submit file] submits a close for given [file]. This operation does not block. Returns a
      [t] to the close submission or an [Errno.t] if the close could not be submitted. *)

  val submit_hlt: file -> t
  (** [submit file] submits a close for given [file]. This operation does not block. Returns a
      [t] to the close submission or halts if the close could not be submitted. *)

  val complete: t -> Errno.t option
  (** [complete t] blocks until the given [t] is complete. Returns [None] or an [Errno.t] if file
      could not be closed. *)

  val complete_hlt: t -> unit
  (** [complete_hlt t] blocks until the given [t] is complete. Returns a [unit] or halts if file
      could not be closed. *)
end

val close: t -> Errno.t option
(** [close t] closes the external mutable Unix file descriptor associated with [t] and returns
    [None] or an [Errno.t] if it could not be closed. *)

val close_hlt: t -> unit
(** [close_hlt t] closes the external mutable Unix file descriptor associated with [t] and returns a
    [unit] or halts if it could not be closed. *)

module Read: sig
  type file = t
  type t
  (* An internally immutable token backed by an external I/O read completion data structure. *)

  val submit: ?n:uns -> ?buffer:Bytes.Slice.t -> file -> (t, Errno.t) result
  (** [submit ?n ?buffer file] submits a read for given [file]. If given, [n] is the maximum read
      size and 1024 otherwise. If given, [buffer] is where read bytes are stored and the maximum
      read size is the minumum of [n] and the size of [buffer]. If [buffer] is not given, one will
      be created with size [n]. This operation does not block. Returns a [t] to the read submission
      or an [Errno.t] if the read could not be submitted. *)

  val submit_hlt: ?n:uns -> ?buffer:Bytes.Slice.t -> file -> t
  (** [submit n buffer file] submits a read for given [file]. If given, [n] is the maximum read size
      and 1024 otherwise. If given, [buffer] is where read bytes are stored and the maximum read
      size is the minumum of [n] and the size of [buffer]. If [buffer] is not given, one will be
      created with size [n]. This operation does not block. Returns a [t] to the read submission or
      halts if the read could not be submitted. *)

  val complete: t -> (Bytes.Slice.t, Errno.t) result
  (** [complete t] blocks until the given [t] is complete. Returns the buffer into which bytes were
      read or an error if bytes could not be read. *)

  val complete_hlt: t -> Bytes.Slice.t
  (** [complete_hlt t] blocks until the given [t] is complete. Returns the buffer into which bytes
      were read or halts if bytes could not be read. *)
end

val read: ?n:uns -> ?buffer:Bytes.Slice.t -> t -> (Bytes.Slice.t, Errno.t) result
(** [read ?n ?buffer t] reads from given [t]. If given, [n] is the maximum read size and 1024
    otherwise. If given, [buffer] is where read bytes are stored and the maximum read size is the
    minumum of [n] and the size of [buffer]. If [buffer] is not given, one will be created with size
    [n]. Returns the [Bytes.Slice.t] into which bytes were read or an [Errno.t] if bytes could not
    be read. *)

val read_hlt: ?n:uns -> ?buffer:Bytes.Slice.t -> t -> Bytes.Slice.t
(** [read_hlt ?n ?buffer t] reads from given [t]. If given, [n] is the maximum read size and 1024
    otherwise. If given, [buffer] is where read bytes are stored and the maximum read size is the
    minumum of [n] and the size of [buffer]. If [buffer] is not given, one will be created with size
    [n]. Returns the [Bytes.Slice.t] into which bytes were read or halts if bytes could not be read.
*)

module Write: sig
  type file = t
  type t
  (* An internally immutable token backed by an external I/O write completion data structure. *)

  val submit: Bytes.Slice.t -> file -> (t, Errno.t) result
  (** [submit bytes file] submits a write for of given [bytes] to given [file]. This operation does
      not block. Returns a [t] to the write submission or an [Errno.t] if the write could not be
      submitted. *)

  val submit_hlt: Bytes.Slice.t -> file -> t
  (** [submit bytes file] submits a write for of given [bytes] to given [file]. This operation
      does not block. Returns a [t] to the write submission or halts if the write could not be
      submitted. *)

  val complete: t ->  (Bytes.Slice.t, Errno.t) result
  (** [complete t] blocks until the given [t] is complete. Returns a [Bytes.Slice.t] of remaining
      bytes that were not written (typically empty) or an [Errno.t] if bytes could not be written.
  *)

  val complete_hlt: t -> Bytes.Slice.t
  (** [complete_hlt t] blocks until the given [t] is complete. Returns a [Bytes.Slice.t] of
      remaining bytes that were not written (typically empty) or halts if bytes could not be
      written. *)
end

val write: Bytes.Slice.t -> t -> Errno.t option
(** [write bytes t] writes [bytes] to [t] and returns [None] or an [Errno.t] if bytes could not be
    written. *)

val write_hlt: Bytes.Slice.t -> t -> unit
(** [write_hlt bytes t] writes [bytes] to [t] and returns a [unit] or halts if bytes could not be
    written. *)

val seek: sint -> t -> (uns, Errno.t) result
(** [seek i t] seeks the external mutable Unix file descriptor associated with [t] to point to the
    [i]th byte relative to the current byte position of the file. Returns an [uns] of the new byte
    index relative to the beginning of the file or an [Errno.t] if it could not be changed. *)

val seek_hlt: sint -> t -> uns
(** [seek_hlt i t] seeks the external mutable Unix file descriptor associated with [t] to point to
    the [i]th byte relative to the current byte position of the file. Returns an [uns] of the new
    byte index relative to the beginning of the file or halts if it could not be changed. *)

val seek_hd: sint -> t -> (uns, Errno.t) result
(** [seek_hd i t] seeks the external mutable Unix file descriptor associated with [t] to point to
    the [i]th byte relative to the head of the file. Returns an [uns] of the new byte index relative
    to the beginning of the file or an [Errno.t] if it could not be changed. *)

val seek_hd_hlt: sint -> t -> uns
(** [seek_hd_hlt i t] seeks the external mutable Unix file descriptor associated with [t] to point
    to the [i]th byte relative to the head of the file. Returns an [uns] of the new byte index
    relative to the beginning of the file or halts if it could not be changed. *)

val seek_tl: sint -> t -> (uns, Errno.t) result
(** [seek_tl i t] seeks the external mutable Unix file descriptor associated with [t] to point to
    the [i]th byte relative to the tail of the file. Returns an [uns] of the new byte index relative
    to the beginning of the file or an [Errno.t] if it could not be changed. *)

val seek_tl_hlt: sint -> t -> uns
(** [seek_tl_hlt i t] seeks the external mutable Unix file descriptor associated with [t] to point
    to the [i]th byte relative to the tail of the file. Returns an [uns] of the new byte index
    relative to the beginning of the file or halts if it could not be changed. *)

module Stream : sig
  type file = t

  type t = Bytes.Slice.t Stream.t

  val of_file: file -> t
  (** [of_file file] takes a [file] and returns a [t], a lazily initialized buffer stream that reads
      subsequent chunks of [file] into buffers when forced. *)

  val write: file -> t -> Errno.t option
  (** [write file t] takes an open [file] with write permissions and writes, in order, all buffers
      from [t] to it. Returns an error if not all bytes could be written. *)

  val write_hlt: file -> t -> unit
  (** [write_hlt file t] takes an open [file] with write permissions and writes, in order, all
      buffers from [t] to it. Halts if not all bytes could be written. *)
end

(** Formatters. *)
module Fmt : sig
  val bufsize_default: uns
  (** Default buffer size used by formatters created via [of_t]. *)

  val of_t: ?bufsize:uns -> t -> (module Fmt.Formatter)
  (** [of_t ~bufsize t] returns a buffered formatter which outputs to [t]. To disable buffering,
      specify [~bufsize:0]. *)

  val stdout: (module Fmt.Formatter)
  (** Buffered formatter which outputs to [stdout]. *)

  val stderr: (module Fmt.Formatter)
  (** Unbuffered formatter which outputs to [stderr]. *)

  val sink: (module Fmt.Formatter)
  (** Formatter which discards all input. *)
end
