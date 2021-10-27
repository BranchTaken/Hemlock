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

CAMLprim value
hm_basis_file_error_to_string_get_length(value a_error) {
    int error = Int64_val(a_error);

    int length = strlen(strerror(error));

    return caml_copy_int64(length);
}

CAMLprim value
hm_basis_file_error_to_string_inner(value a_n, value a_bytes, value a_error) {
    size_t n = Int64_val(a_n);
    uint8_t *bytes = (uint8_t *)Bytes_val(a_bytes);
    int error = Int64_val(a_error);

    strncpy(bytes, strerror(error), n);

    return Val_unit;
}

CAMLprim value
hm_basis_file_finalize_result(int result) {
    if (result == -1) {
        result = -errno;
    }

    return caml_copy_int64(result);
}

CAMLprim value
hm_basis_file_of_path_inner(value a_flag, value a_mode, value a_bytes) {
    size_t flag = Long_val(a_flag);
    size_t mode = Int64_val(a_mode);
    uint8_t *bytes = (uint8_t *)Bytes_val(a_bytes);
    size_t n = caml_string_length(a_bytes);

    int flags = flags_of_hemlock_file_flag[flag];
    int result = open(bytes, flags, mode);

    return hm_basis_file_finalize_result(result);
}

CAMLprim value
hm_basis_file_stdin_inner(value a_unit) {
    return caml_copy_int64(STDIN_FILENO);
}

CAMLprim value
hm_basis_file_stdout_inner(value a_unit) {
    return caml_copy_int64(STDOUT_FILENO);
}

CAMLprim value
hm_basis_file_stderr_inner(value a_unit) {
    return caml_copy_int64(STDERR_FILENO);
}

CAMLprim value
hm_basis_file_close_inner(value a_fd) {
    int fd = Int64_val(a_fd);

    return hm_basis_file_finalize_result(close(fd));
}

CAMLprim value
hm_basis_file_read_inner(value a_bytes, value a_fd) {
    uint8_t *bytes = (uint8_t *)Bytes_val(a_bytes);
    size_t n = caml_string_length(a_bytes);
    int fd = Int64_val(a_fd);

    int result = read(fd, bytes, n);

    return hm_basis_file_finalize_result(result);
}

CAMLprim value
hm_basis_file_write_inner(value a_bytes, value a_fd) {
    uint8_t *bytes = (uint8_t *)Bytes_val(a_bytes);
    size_t n = caml_string_length(a_bytes);
    int fd = Int64_val(a_fd);

    int result = write(fd, bytes, n);

    return hm_basis_file_finalize_result(result);
}

CAMLprim value
hm_basis_file_seek_inner(value a_i, value a_fd) {
    size_t i = Int64_val(a_i);
    int fd = Int64_val(a_fd);

    return hm_basis_file_finalize_result(lseek(fd, i, SEEK_CUR));
}

CAMLprim value
hm_basis_file_seek_hd_inner(value a_i, value a_fd) {
    size_t i = Int64_val(a_i);
    int fd = Int64_val(a_fd);

    return hm_basis_file_finalize_result(lseek(fd, i, SEEK_SET));
}

CAMLprim value
hm_basis_file_seek_tl_inner(value a_i, value a_fd) {
    size_t i = Int64_val(a_i);
    int fd = Int64_val(a_fd);

    return hm_basis_file_finalize_result(lseek(fd, i, SEEK_END));
}
