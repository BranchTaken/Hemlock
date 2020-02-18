#include <stdbool.h>
#include <stddef.h>
#include <unistd.h>
#ifdef __APPLE__
// For getentropy().
#  include <sys/random.h>
#endif
#include <caml/mlvalues.h>
#include <caml/alloc.h>

// Return number of entropy bits written to buf.
static size_t
hemlock_getentropy(uint8_t *buf, size_t nbits_in) {
  size_t nbytes_out = (nbits_in + 7) / 8;
  size_t nbits_out = nbytes_out * 8;

  return (getentropy((void *)buf, nbytes_out) == 0) ? nbits_out : 0;
}

static value
value_of_result(const char *arg) {
  uint64_t entropy = *((uint64_t *)arg);

  return caml_copy_int64(entropy);
}

// The OCaml FFI is quite low-level, such that returning (Int64.t option) is a
// lot of trouble relative to returning an array.  As a consequence this
// function returns an empty array to indicate error.
//
//                                       nbits -> [|entropy...|]
// val hemlock_hash_state_entropy_nbits: usize -> Int64.t array
CAMLprim value
hemlock_hash_state_entropy_nbits(value a_nbits) {
  size_t nbits_in = Unsigned_long_val(a_nbits);
  size_t nbytes_in = (nbits_in + 7) / 8;
  size_t nuint64s = (nbytes_in + 7) / 8;
  uint64_t entropy[nuint64s];
  size_t nbits_out = hemlock_getentropy((uint8_t *)entropy, nbits_in);
  uint64_t error = (nbits_out < nbits_in);

  if (nbits_out == 0) {
    const char *result = {NULL};
    return caml_alloc_array(value_of_result, &result);
  } else {
    uint64_t *result[nuint64s + 1]; // [entropy..., NULL]
    for (size_t i = 0; i < nuint64s; i++) {
      result[i] = &entropy[i];
    }
    result[nuint64s] = NULL;
    return caml_alloc_array(value_of_result, (const char **)result);
  }
}
