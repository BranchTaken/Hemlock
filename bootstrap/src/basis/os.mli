open RudimentsInt

(** Operating system interfaces. *)

val argv: Bytes.t array
(** [argv] comprises the command line arguments, where the first element is the path to the program
    being executed. *)

val mkdirat: ?dir:File.t -> ?mode:uns -> Path.t -> Errno.t option
(** [mkdirat ~dir ~mode path] creates a directory at [path] with file mode [mode], which defaults to
    [0o755]. If [path] is relative, the directory corresponding to [dir] is used as the starting
    directory for path resolution. [dir] defaults to the process's current working directory. *)
