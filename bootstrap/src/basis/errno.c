#include <string.h>

#define CAML_NAME_SPACE
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

static int
error_of_value(value a_error) {
    // Variant codes start at 0, but error numbers start at 1.
    return Int_val(a_error) + 1;
}

CAMLprim value
hm_basis_errno_to_string_get_length(value a_error) {
    int error = error_of_value(a_error);

    int length = strlen(strerror(error));

    return caml_copy_int64(length);
}

CAMLprim value
hm_basis_errno_to_string_inner(value a_n, value a_bytes, value a_error) {
    size_t n = Int64_val(a_n);
    uint8_t *bytes = (uint8_t *)Bytes_val(a_bytes);
    int error = error_of_value(a_error);

    strncpy((char *)bytes, strerror(error), n);

    return Val_unit;
}
