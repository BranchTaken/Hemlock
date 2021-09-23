#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>

int flags_of_hemlock_file_flag[] = {
    /* R_O   */ O_RDONLY,
    /* W     */ O_WRONLY | O_CREAT,
    /* W_A   */ O_WRONLY | O_APPEND | O_CREAT,
    /* W_AO  */ O_WRONLY | O_APPEND,
    /* W_C   */ O_WRONLY | O_CREAT | O_EXCL,
    /* W_O   */ O_WRONLY,
    /* RW    */ O_RDWR | O_CREAT,
    /* RW_A  */ O_RDWR | O_APPEND| O_CREAT,
    /* RW_AO */ O_RDWR | O_APPEND,
    /* RW_C  */ O_RDWR | O_CREAT | O_EXCL,
    /* RW_O  */ O_RDWR,
};

static uint8_t *
deflate_bytes(size_t n, size_t * inflated_bytes) {
    uint8_t * deflated_bytes = (uint8_t *) inflated_bytes;
    for (size_t i = 0; i < n; i++) {
        deflated_bytes[i] = Int64_val(inflated_bytes[i]);
    }
    if (n > 0) {
        deflated_bytes[n] = '\0';
    }
    return deflated_bytes;
}

static size_t *
inflate_bytes(size_t n, uint8_t * deflated_bytes) {
    size_t * inflated_bytes = (size_t *) deflated_bytes;
    for (size_t i = n; i-- > 0;) {
        inflated_bytes[i] = (size_t) caml_copy_int64(deflated_bytes[i]);
    }
    return inflated_bytes;
}

CAMLprim value
hemlock_file_error_to_string_get_length(value a_error) {
    int error = Int64_val(a_error);

    int length = strlen(strerror(error));

    return caml_copy_int64(length);
}

CAMLprim value
hemlock_file_error_to_string_inner(value a_n, value a_bytes, value a_error) {
    size_t n = Int64_val(a_n);
    size_t * bytes = (size_t *) String_val(a_bytes);
    int error = Int64_val(a_error);

    uint8_t * cbytes = (uint8_t *) bytes;
    strncpy(cbytes, strerror(error), n);
    inflate_bytes(n, cbytes);

    return Val_unit;
}

CAMLprim value
hemlock_file_finalize_result(int result) {
    if (-1 == result) {
        result = -errno;
    }

    return caml_copy_int64(result);
}

CAMLprim value
hemlock_file_of_path_inner(value a_flag, value a_mode, value a_i, value a_j, value a_bytes) {
    size_t flag = Long_val(a_flag);
    size_t mode = Int64_val(a_mode);
    size_t i = Int64_val(a_i);
    size_t j = Int64_val(a_j);
    size_t * bytes = (size_t *) String_val(a_bytes);

    int flags = flags_of_hemlock_file_flag[flag];
    size_t n = j - i;
    bytes = &bytes[i];
    uint8_t * cbytes = deflate_bytes(n, bytes);
    // TODO expose mode in API
    int result = open(cbytes, flags, mode);
    inflate_bytes(n, cbytes);

    return hemlock_file_finalize_result(result);
}

CAMLprim value
hemlock_file_stdin_inner(value a_unit) {
    return caml_copy_int64(STDIN_FILENO);
}

CAMLprim value
hemlock_file_stdout_inner(value a_unit) {
    return caml_copy_int64(STDOUT_FILENO);
}

CAMLprim value
hemlock_file_stderr_inner(value a_unit) {
    return caml_copy_int64(STDERR_FILENO);
}

CAMLprim value
hemlock_file_close_inner(value a_fd) {
    int fd = Int64_val(a_fd);

    return hemlock_file_finalize_result(close(fd));
}

CAMLprim value
hemlock_file_read_inner(value a_i, value a_j, value a_bytes, value a_fd) {
    size_t i = Int64_val(a_i);
    size_t j = Int64_val(a_j);
    size_t * bytes = (size_t *) String_val(a_bytes);
    int fd = Int64_val(a_fd);

    size_t n = j - i;
    bytes = &bytes[i];
    uint8_t * cbytes = (uint8_t *) bytes;
    int result = read(fd, cbytes, n);
    if (result > 0) {
        inflate_bytes(result, cbytes);
    }

    return hemlock_file_finalize_result(result);
}

CAMLprim value
hemlock_file_read_into_inner(value a_i, value a_j, value a_bytes, value a_fd) {
    return hemlock_file_read_inner(a_i, a_j, a_bytes, a_fd);
}

CAMLprim value
hemlock_file_write_inner(value a_i, value a_j, value a_bytes, value a_fd) {
    size_t i = Int64_val(a_i);
    size_t j = Int64_val(a_j);
    size_t * bytes = (size_t *) String_val(a_bytes);
    int fd = Int64_val(a_fd);

    size_t n = j - i;
    bytes = &bytes[i];
    uint8_t * cbytes = deflate_bytes(n, bytes);
    int result = write(fd, cbytes, n);
    inflate_bytes(n, cbytes);

    return hemlock_file_finalize_result(result);
}

CAMLprim value
hemlock_file_seek_inner(value a_i, value a_fd) {
    size_t i = Int64_val(a_i);
    int fd = Int64_val(a_fd);

    return hemlock_file_finalize_result(lseek(fd, i, SEEK_CUR));
}

CAMLprim value
hemlock_file_seek_hd_inner(value a_i, value a_fd) {
    size_t i = Int64_val(a_i);
    int fd = Int64_val(a_fd);

    return hemlock_file_finalize_result(lseek(fd, i, SEEK_SET));
}

CAMLprim value
hemlock_file_seek_tl_inner(value a_i, value a_fd) {
    size_t i = Int64_val(a_i);
    int fd = Int64_val(a_fd);

    return hemlock_file_finalize_result(lseek(fd, i, SEEK_END));
}
