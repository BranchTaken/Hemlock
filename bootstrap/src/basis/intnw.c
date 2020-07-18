#include <stddef.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

static value
value_of_int64(const char *arg) {
  uint64_t x = *((uint64_t *)arg);
  return caml_copy_int64(x);
}

static CAMLprim value
constant(unsigned nwords, uint64_t v0, uint64_t vi) {
  uint64_t *result[nwords + 1];
  result[0] = &v0;
  for (size_t i = 1; i < nwords; i++) {
    result[i] = &vi;
  }
  result[nwords] = NULL;
  return caml_alloc_array(value_of_int64, (const char **)result);
}

static CAMLprim value
zero(unsigned nwords) {
  return constant(nwords, 0, 0);
}

static CAMLprim value
one(unsigned nwords) {
  return constant(nwords, 1, 0);
}

static CAMLprim value
neg_one(unsigned nwords) {
  return constant(nwords, 0xffffffffffffffffLU, 0xffffffffffffffffLU);
}

// val hemlock_intnw_icmp: int64 array -> int64 array -> Cmp.t
CAMLprim value
hemlock_intnw_icmp(value a_a, value a_b) {
  // XXX Read inputs as arrays.
  ssize_t a = Long_val(a_a);
  ssize_t b = Long_val(a_b);
  int rel = (a > b) - (a < b);
  // Cmp.t is {Lt,Eq,Gt} = {0,1,2}.
  return Val_long(rel + 1);
}

// val hemlock_intnw_ucmp: int64 array -> int64 array -> Cmp.t
CAMLprim value
hemlock_intnw_ucmp(value a_a, value a_b) {
  // XXX Read inputs as arrays.
  size_t a = Unsigned_long_val(a_a);
  size_t b = Unsigned_long_val(a_b);
  int rel = (a > b) - (a < b);
  // Cmp.t is {Lt,Eq,Gt} = {0,1,2}.
  return Val_long(rel + 1);
}
