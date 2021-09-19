#include <stddef.h>
#include <caml/mlvalues.h>

CAMLprim value
hemlock_u64_bit_pop(value a_x) {
    uint64_t x = Int64_val(a_x);
    return Val_long(__builtin_popcountl(x));
}

CAMLprim value
hemlock_u64_bit_clz(value a_x) {
    uint64_t x = Int64_val(a_x);
    return Val_long((x == 0) ? 64 : __builtin_clzl(x));
}

CAMLprim value
hemlock_u64_bit_ctz(value a_x) {
    uint64_t x = Int64_val(a_x);
    return Val_long((x == 0) ? 64 : __builtin_ctzl(x));
}
