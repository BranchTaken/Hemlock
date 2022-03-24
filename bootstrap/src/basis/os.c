#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>

#define CAML_NAME_SPACE
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

CAMLprim value
hm_basis_os_at_fdcwd_inner(value a_unit) {
    return caml_copy_int64(AT_FDCWD);
}

CAMLprim value
hm_basis_os_mkdirat_inner(value a_dirfd, value a_pathname, value a_mode) {
    int dirfd = Int64_val(a_dirfd);
    // XXX Allocate a copy when transitioning to io_uring, since the I/O may be in flight during
    // subsequent GCs.
    const char *pathname = String_val(a_pathname);
    size_t mode = Int64_val(a_mode);

    // XXX Use io_uring once Linux 5.15 is in use.
    int result = 0;
    if (mkdirat(dirfd, pathname, mode) != 0) {
        result = errno;
    }

    return caml_copy_int64(result);
}
