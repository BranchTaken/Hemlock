#include <stddef.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

// val hemlock_intnb_icmp: sint -> sint -> Cmp.t
CAMLprim value
hemlock_intnb_icmp(value a_a, value a_b) {
    ssize_t a = Int64_val(a_a);
    ssize_t b = Int64_val(a_b);
    int rel = (a > b) - (a < b);
    // Cmp.t is {Lt,Eq,Gt} = {0,1,2}.
    return Val_long(rel + 1);
}

// val hemlock_intnb_ucmp: uns -> uns -> Cmp.t
CAMLprim value
hemlock_intnb_ucmp(value a_a, value a_b) {
    size_t a = Int64_val(a_a);
    size_t b = Int64_val(a_b);
    int rel = (a > b) - (a < b);
    // Cmp.t is {Lt,Eq,Gt} = {0,1,2}.
    return Val_long(rel + 1);
}
