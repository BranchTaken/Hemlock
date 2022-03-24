open Rudiments

let argv = Array.map Stdlib.Sys.argv ~f:(fun arg ->
  Bytes.of_string_slice (String.C.Slice.of_string arg))

external at_fdcwd_inner: unit -> uns = "hm_basis_os_at_fdcwd_inner"

let at_fdcwd =
  at_fdcwd_inner ()

external mkdirat_inner: uns -> string -> uns -> sint = "hm_basis_os_mkdirat_inner"

let mkdirat ?dir ?(mode=0o755L) path =
  let dirfd = match dir with
    | None -> at_fdcwd
    | Some dir -> File.fd dir
  in
  match mkdirat_inner dirfd (Path.to_string_replace path) mode with
  | 0L -> None
  | errno -> Some (Errno.of_uns_hlt (Uns.bits_of_sint errno))
