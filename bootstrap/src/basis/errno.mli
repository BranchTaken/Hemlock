open Rudiments

(** Operating system error system number. *)

type t =
  | EPERM
  | ENOENT
  | ESRCH
  | EINTR
  | EIO
  | ENXIO
  | E2BIG
  | ENOEXEC
  | EBADF
  | ECHILD
  | EAGAIN
  | ENOMEM
  | EACCES
  | EFAULT
  | ENOTBLK
  | EBUSY
  | EEXIST
  | EXDEV
  | ENODEV
  | ENOTDIR
  | EISDIR
  | EINVAL
  | ENFILE
  | EMFILE
  | ENOTTY
  | ETXTBSY
  | EFBIG
  | ENOSPC
  | ESPIPE
  | EROFS
  | EMLINK
  | EPIPE
  | EDOM
  | ERANGE
  | EDEADLK
  | ENAMETOOLONG
  | ENOLCK
  | ENOSYS
  | ENOTEMPTY
  | ELOOP
  | ENOMSG
  | EIDRM
  | ECHRNG
  | EL2NSYNC
  | EL3HLT
  | EL3RST
  | ELNRNG
  | EUNATCH
  | ENOCSI
  | EL2HLT
  | EBADE
  | EBADR
  | EXFULL
  | ENOANO
  | EBADRQC
  | EBADSLT
  | EBFONT
  | ENOSTR
  | ENODATA
  | ETIME
  | ENOSR
  | ENONET
  | ENOPKG
  | EREMOTE
  | ENOLINK
  | EADV
  | ESRMNT
  | ECOMM
  | EPROTO
  | EMULTIHOP
  | EDOTDOT
  | EBADMSG
  | EOVERFLOW
  | ENOTUNIQ
  | EBADFD
  | EREMCHG
  | ELIBACC
  | ELIBBAD
  | ELIBSCN
  | ELIBMAX
  | ELIBEXEC
  | EILSEQ
  | ERESTART
  | ESTRPIPE
  | EUSERS
  | ENOTSOCK
  | EDESTADDRREQ
  | EMSGSIZE
  | EPROTOTYPE
  | ENOPROTOOPT
  | EPROTONOSUPPORT
  | ESOCKTNOSUPPORT
  | EOPNOTSUPP
  | EPFNOSUPPORT
  | EAFNOSUPPORT
  | EADDRINUSE
  | EADDRNOTAVAIL
  | ENETDOWN
  | ENETUNREACH
  | ENETRESET
  | ECONNABORTED
  | ECONNRESET
  | ENOBUFS
  | EISCONN
  | ENOTCONN
  | ESHUTDOWN
  | ETOOMANYREFS
  | ETIMEDOUT
  | ECONNREFUSED
  | EHOSTDOWN
  | EHOSTUNREACH
  | EALREADY
  | EINPROGRESS
  | ESTALE
  | EUCLEAN
  | ENOTNAM
  | ENAVAIL
  | EISNAM
  | EREMOTEIO
  | EDQUOT
  | ENOMEDIUM
  | EMEDIUMTYPE
  | ECANCELED
  | ENOKEY
  | EKEYEXPIRED
  | EKEYREVOKED
  | EKEYREJECTED
  | EOWNERDEAD
  | ENOTRECOVERABLE
  | ERFKILL
  | EHWPOISON

val of_uns: uns -> t option
(** [of_uns u] creates an error corresponding to [u], or [None] if [u] does not correspond to an
    error number. *)

val of_uns_hlt: uns -> t
(** [of_uns u] creates an error corresponding to [u], or halts if [u] does not correspond to an
    error number. *)

val to_uns: t -> uns
(** [to_uns t] returns the operating system's error number encoding which corresponds to [t]. *)

val to_string: t -> string
(** [to_string t] creates a string description corresponding to [t]. *)

include FormattableIntf.SMono with type t := t
