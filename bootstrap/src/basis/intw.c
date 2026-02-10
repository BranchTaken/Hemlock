#include <stddef.h>
#include <stdbool.h>
#include <math.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>

#include <assert.h>

// Word.
typedef uint64_t hm_word_t;

// Signed word.
typedef int64_t hm_iword_t;

// Half word.
typedef uint32_t hm_hword_t;

// Bits per word.
static const size_t hm_bpw = sizeof(hm_word_t) << 3;

// Bits per half word.
static const size_t hm_bphw = sizeof(hm_hword_t) << 3;

static size_t
zu_min(size_t a, size_t b) {
    return (a < b) ? a : b;
}

static size_t
zu_max(size_t a, size_t b) {
    return (a > b) ? a : b;
}

static size_t
zu_id(size_t a) {
    return a;
}

static size_t
zu_succ(size_t a) {
    return a + 1;
}

static hm_word_t
hm_word_sl(hm_word_t x, unsigned shift) {
    return (x << shift);
}

static hm_word_t
hm_word_usr(hm_word_t x, unsigned shift) {
    return (x >> shift);
}

static hm_word_t
hm_word_ssr(hm_word_t x, unsigned shift) {
    return ((hm_iword_t)x >> shift);
}

static value
value_of_int64(const char *arg) {
    hm_iword_t x = *((hm_iword_t *)arg);
    return caml_copy_int64(x);
}

static bool
is_signed_min_value(const hm_word_t *a, size_t nw) {
    if (nw == 0) {
        return false;
    }
    for (size_t i = 0; i < nw - 1; i++) {
        if (a[i] != 0) {
            return false;
        }
    }
    return (a[nw - 1] == 0x8000000000000000LU);
}

static bool
is_neg_one(const hm_word_t *a, size_t anw) {
    return (anw > 0 && a[anw-1] == 0xffffffffffffffffLU);
}

static bool
is_neg(const hm_word_t *a, size_t anw) {
    return (anw > 0 && (a[anw-1] & 0x8000000000000000LU) != 0);
}

static bool
is_ineg(const hm_iword_t *a, size_t anw) {
    return (anw > 0 && (a[anw-1] & 0x8000000000000000L) != 0);
}

static void
init_u(hm_word_t u, hm_word_t *r, size_t rnw) {
    if (rnw > 0) {
        r[0] = u;
    }
    for (size_t i = 1; i < rnw; i++) {
        r[i] = 0LU;
    }
}

static void
init_zero(hm_word_t *r, size_t rnw) {
    init_u(0LU, r, rnw);
}

static void
init_one(hm_word_t *r, size_t rnw) {
    init_u(1LU, r, rnw);
}

#if 0
static void
init_i(hm_word_t x, hm_word_t *r, size_t rnw) {
    if ((x & 0x8000000000000000LU) == 0) {
        init_u(x, r, rnw);
        return;
    }
    r[0] = x;
    for (size_t i = 1; i < rnw; i++) {
        r[i] = 0xffffffffffffffffLU;
    }
}
#endif

static void
upad_i(const hm_word_t *a, size_t anw, hm_word_t *r, size_t rnw) {
    assert(anw <= rnw);

    for (size_t i = 0; i < anw; i++) {
        r[i] = a[i];
    }
    hm_iword_t pad = is_neg(a, anw) ? 0xffffffffffffffffLU : 0LU;
    for (size_t i = anw; i < rnw; i++) {
        r[i] = pad;
    }
}

static void
ipad_i(const hm_iword_t *a, size_t anw, hm_iword_t *r, size_t rnw) {
    upad_i((hm_word_t *)a, anw, (hm_word_t *)r, rnw);
}

static void
pad_u(const hm_word_t *a, size_t anw, hm_word_t *r, size_t rnw) {
    assert(anw <= rnw);

    for (size_t i = 0; i < anw; i++) {
        r[i] = a[i];
    }
    for (size_t i = anw; i < rnw; i++) {
        r[i] = 0LU;
    }
}

static void
bit_not(const hm_word_t *a, size_t anw, hm_word_t *r, size_t rnw) {
    size_t min_nw = zu_min(anw, rnw);

    for (size_t i = 0; i < min_nw; i++) {
        r[i] = ~a[i];
    }
    for (size_t i = min_nw; i < rnw; i++) {
        r[i] = 0xffffffffffffffffLU;
    }
}

// 2's complement only works for equal [ab]nw; pad for signed addition.
static void
add(bool signed_, const hm_word_t *a, size_t anw, const hm_word_t *b, size_t bnw, hm_word_t *r,
  size_t rnw) {
    size_t min_nw = zu_min(anw, bnw);
    size_t max_nw = zu_max(anw, bnw);

    hm_word_t carry = 0;
    for (size_t i = 0; i < min_nw; i++) {
        r[i] = a[i] + b[i] + carry;
        carry = ((a[i] & b[i]) | ((a[i] | b[i]) & ~r[i])) >> (hm_bpw - 1);
    }
    hm_word_t ai = (signed_ && is_neg(a, anw)) ? 0xffffffffffffffffLU : 0LU;
    hm_word_t bi = (signed_ && is_neg(b, bnw)) ? 0xffffffffffffffffLU : 0LU;
    if (anw > bnw) {
        for (size_t i = min_nw; i < anw; i++) {
            r[i] = a[i] + bi + carry;
            carry = ((a[i] & bi) | ((a[i] | bi) & ~r[i])) >> (hm_bpw - 1);
        }
    } else {
        for (size_t i = min_nw; i < bnw; i++) {
            r[i] = ai + b[i] + carry;
            carry = ((ai & b[i]) | ((ai | b[i]) & ~r[i])) >> (hm_bpw - 1);
        }
    }
    for (size_t i = max_nw; i < rnw; i++) {
        r[i] = ai + bi + carry;
        carry = ((ai & bi) | ((ai | bi) & ~r[i])) >> (hm_bpw - 1);
    }
}

static void
uadd(const hm_word_t *a, size_t anw, const hm_word_t *b, size_t bnw, hm_word_t *r, size_t rnw) {
    add(false, a, anw, b, bnw, r, rnw);
}

static void
iadd(const hm_word_t *a, size_t anw, const hm_word_t *b, size_t bnw, hm_word_t *r, size_t rnw) {
    add(true, a, anw, b, bnw, r, rnw);
}

// Does not handle truncation of negative values.
static void
neg_helper(const hm_word_t *a, size_t anw, hm_word_t *r, size_t rnw) {
    assert(!is_neg(a, anw) || anw <= rnw);
    hm_word_t a_padded[rnw];
    if (anw <= rnw) {
        upad_i(a, anw, a_padded, rnw);
    } else {
        for (size_t i = 0; i < rnw; i++) {
            a_padded[i] = a[i];
        }
    }
    hm_word_t not_a[rnw];
    bit_not(a_padded, rnw, not_a, rnw);
    hm_word_t one[rnw];
    init_one(one, rnw);
    add(true, not_a, rnw, one, rnw, r, rnw);
}

// Does not handle signed truncation.
static void
dup_helper(bool signed_, const hm_word_t *a, size_t anw, hm_word_t *r, size_t rnw) {
    assert(!signed_ || !is_neg(a, anw) || anw <= rnw);
    size_t min_nw = zu_min(anw, rnw);
    for (size_t i = 0; i < min_nw; i++) {
        r[i] = a[i];
    }
    if (rnw > anw) {
        hm_word_t ext = (signed_ && is_neg(a, anw)) ? 0xffffffffffffffffLU : 0;
        for (size_t i = anw; i < rnw; i++) {
            r[i] = ext;
        }
    }
}

// NB: Negation of the minimum value is a no-op if (rnw == anw); assure that (rnw == anw+1) if
// lossless negation is required.
static void
neg(const hm_word_t *a, size_t anw, hm_word_t *r, size_t rnw) {
    if (is_neg(a, anw) && rnw < anw) {
        hm_word_t a_abs[anw];
        neg_helper(a, anw, a_abs, anw);
        hm_word_t r_abs[rnw];
        dup_helper(true, a_abs, anw, r_abs, rnw);
        neg_helper(r_abs, rnw, r, rnw);
    } else {
        neg_helper(a, anw, r, rnw);
    }
}

static void
dup(bool signed_, const hm_word_t *a, size_t anw, hm_word_t *r, size_t rnw) {
    if (signed_ && is_neg(a, anw) && rnw < anw) {
        // Truncation of a negative number requires special care.
        hm_word_t a_abs[anw];
        neg_helper(a, anw, a_abs, anw);
        neg(a_abs, anw, r, rnw);
    } else {
        dup_helper(signed_, a, anw, r, rnw);
    }
}

// Returns is_neg(a, nw). See neg() for note re: possible truncation.
static bool
is_neg_abs(const hm_word_t *a, size_t anw, hm_word_t *r, size_t rnw) {
    if (is_neg(a, anw)) {
        neg(a, anw, r, rnw);
        return true;
    } else {
        dup(false, a, anw, r, rnw);
        return false;
    }
}

static size_t
trim_trunc(bool signed_, const hm_word_t *a, size_t nw, size_t min_rnw, size_t max_rnw,
  hm_word_t *r) {
    size_t rnw = nw;
    if (signed_) {
        if (rnw > min_rnw && is_neg_one(a, rnw) && is_neg(a, rnw-1)) {
            do {
                rnw--;
            } while (rnw > min_rnw && is_neg_one(a, rnw) && is_neg(a, rnw-1));
        } else {
            // Signed positive values must have at least one leading 0 bit.
            while (rnw > min_rnw && a[rnw-1] == 0LU && !is_neg(a, rnw-1)) {
                rnw--;
            }
        }
    } else {
        while (rnw > min_rnw && a[rnw-1] == 0LU) {
            rnw--;
        }
    }
    rnw = zu_min(rnw, max_rnw);
    dup(signed_, a, nw, r, rnw);
    return rnw;
}

static value
oarray_of_uarray(bool signed_, const hm_word_t *a, size_t anw, size_t min_anw, size_t max_anw) {
    hm_word_t r[anw];
    size_t rnw = trim_trunc(signed_, a, anw, min_anw, max_anw, r);
    const hm_word_t *result[rnw + 1];
    for (size_t i = 0; i < rnw; i++) {
        result[i] = &r[i];
    }
    result[rnw] = NULL;
    return caml_alloc_array(value_of_int64, (const char **)result);
}

static size_t
oarray_length(value a_a) {
    return (size_t)caml_array_length(a_a);
}

static hm_word_t
oarray_get(value a_a, size_t a_i) {
    return (hm_word_t)Int64_val(Field(a_a, a_i));
}

static void
uarray_of_cbs(bool signed_, value a_a, hm_word_t *r, size_t rnw) {
    size_t anw = oarray_length(a_a);
    for (size_t i = 0; i < zu_min(anw, rnw); i++) {
        r[i] = oarray_get(a_a, i);
    }
    if (anw < rnw) {
        hm_word_t pad = (signed_ && is_neg(r, anw)) ? 0xffffffffffffffffLU : 0LU;
        for (size_t i = anw; i < rnw; i++) {
            r[i] = pad;
        }
    }
}

static void
iarray_of_cbs(value a_a, hm_iword_t *r, size_t rnw) {
    return uarray_of_cbs(true, a_a, (hm_word_t *)r, rnw);
}

static int
icmp(const hm_iword_t *a, size_t anw, const hm_iword_t *b, size_t bnw) {
    if (anw == bnw) {
        bool a_neg = is_ineg(a, anw);
        bool b_neg = is_ineg(b, bnw);
        int sign_rel = (int)b_neg - (int)a_neg;
        if (sign_rel != 0) return sign_rel;
        // a and b have the same sign. Unsigned comparison does the right thing under these
        // circumstances.
        for (size_t i = anw; i-- > 0;) {
            hm_word_t a_i = a[i];
            hm_word_t b_i = b[i];
            int rel = (a_i > b_i) - (a_i < b_i);
            if (rel != 0) return rel;
        }
        return 0;
    } else if (anw < bnw) {
        hm_iword_t a_padded[bnw];
        ipad_i(a, anw, a_padded, bnw);
        return icmp(a_padded, bnw, b, bnw);
    } else {
        assert(anw > bnw);
        hm_iword_t b_padded[anw];
        ipad_i(b, bnw, b_padded, anw);
        return icmp(a, anw, b_padded, anw);
    }
}

// val intw_icmp: int64 array -> int64 array -> Cmp.t
CAMLprim value
hm_basis_intw_icmp(value a_a, value a_b) {
    size_t anw = oarray_length(a_a);
    size_t bnw = oarray_length(a_b);
    hm_iword_t a[anw];
    hm_iword_t b[bnw];
    iarray_of_cbs(a_a, a, anw);
    iarray_of_cbs(a_b, b, bnw);

    int rel = icmp(a, anw, b, bnw);
    // Cmp.t is {Lt,Eq,Gt} = {0,1,2}.
    return Val_long(rel + 1);
}

static int
ucmp(const hm_word_t *a, size_t anw, const hm_word_t *b, size_t bnw) {
    if (anw == bnw) {
        for (size_t i = anw; i-- > 0;) {
            int rel = (a[i] > b[i]) - (a[i] < b[i]);
            if (rel != 0) return rel;
        }
        return 0;
    } else if (anw < bnw) {
        hm_word_t a_padded[bnw];
        pad_u(a, anw, a_padded, bnw);
        return ucmp(a_padded, bnw, b, bnw);
    } else {
        assert(anw > bnw);
        hm_word_t b_padded[anw];
        pad_u(b, bnw, b_padded, anw);
        return ucmp(a, anw, b_padded, anw);
    }
}

// val intw_ucmp: int64 array -> int64 array -> Cmp.t
CAMLprim value
hm_basis_intw_ucmp(value a_a, value a_b) {
    size_t anw = oarray_length(a_a);
    size_t bnw = oarray_length(a_b);
    hm_word_t a[anw];
    hm_word_t b[bnw];
    uarray_of_cbs(false, a_a, a, anw);
    uarray_of_cbs(false, a_b, b, bnw);

    int rel = ucmp(a, anw, b, bnw);
    // Cmp.t is {Lt,Eq,Gt} = {0,1,2}.
    return Val_long(rel + 1);
}

static value
binary_op_pad(bool signed_, bool pad, size_t (*rnw_of_anw_bnw)(size_t, size_t),
  void (*op)(const hm_word_t *, size_t, const hm_word_t *, size_t, hm_word_t *, size_t), value a_a,
  value a_b, value a_min_rnw, value a_max_rnw) {
    size_t anw = oarray_length(a_a);
    size_t bnw = oarray_length(a_b);
    if (pad) {
        anw = bnw = zu_max(anw, bnw);
    }
    size_t rnw = rnw_of_anw_bnw(anw, bnw);
    size_t min_rnw = Int64_val(a_min_rnw);
    size_t max_rnw = zu_min(rnw, Int64_val(a_max_rnw));

    hm_word_t a[anw]; uarray_of_cbs(signed_, a_a, a, anw);
    hm_word_t b[bnw]; uarray_of_cbs(signed_, a_b, b, bnw);
    hm_word_t r[rnw]; op(a, anw, b, bnw, r, rnw);

    return oarray_of_uarray(signed_, r, rnw, min_rnw, max_rnw);
}

static value
binary_op(bool signed_, size_t (*rnw_of_anw_bnw)(size_t, size_t),
  void (*op)(const hm_word_t *, size_t, const hm_word_t *, size_t, hm_word_t *, size_t), value a_a,
  value a_b, value a_min_rnw, value a_max_rnw) {
    return binary_op_pad(signed_, false, rnw_of_anw_bnw, op, a_a, a_b, a_min_rnw, a_max_rnw);
}

static value
unary_op(bool signed_, size_t (*rnw_of_anw)(size_t),
  void (*op)(const hm_word_t *, size_t, hm_word_t *, size_t), value a_a, value a_min_rnw,
  value a_max_rnw) {
    size_t anw = oarray_length(a_a);
    size_t rnw = rnw_of_anw(anw);
    size_t min_rnw = Int64_val(a_min_rnw);
    size_t max_rnw = Int64_val(a_max_rnw);

    hm_word_t a[anw]; uarray_of_cbs(signed_, a_a, a, anw);
    hm_word_t r[rnw]; op(a, anw, r, rnw);

    return oarray_of_uarray(signed_, r, rnw, min_rnw, max_rnw);
}

static value
bit_shift_op(bool signed_, size_t (*rnw_of_shift_anw)(unsigned, size_t),
  void (*op)(unsigned, const hm_word_t *, size_t, hm_word_t *, size_t), value a_shift, value a_a,
  value a_min_rnw, value a_max_rnw) {
    unsigned shift = Int64_val(a_shift);
    size_t anw = oarray_length(a_a);
    size_t min_rnw = Int64_val(a_min_rnw);
    size_t max_rnw = Int64_val(a_max_rnw);
    if (min_rnw == max_rnw) {
        assert(anw == min_rnw);
        size_t nbits = anw * hm_bpw;
        shift %= nbits;
    }
    size_t rnw = zu_min(rnw_of_shift_anw(shift, anw), max_rnw);

    hm_word_t a[anw]; uarray_of_cbs(signed_, a_a, a, anw);
    hm_word_t r[rnw]; if (rnw != 0) op(shift, a, anw, r, rnw);

    return oarray_of_uarray(signed_, r, rnw, min_rnw, max_rnw);
}

static value
bit_count_op(unsigned (*op)(const hm_word_t *, size_t), value a_a) {
    size_t nw = oarray_length(a_a);
    hm_word_t a[nw];
    uarray_of_cbs(false, a_a, a, nw);

    return caml_copy_int64(op(a, nw));
}

static void
u_bit_and(const hm_word_t *a, size_t anw, const hm_word_t *b, size_t bnw, hm_word_t *r,
  size_t rnw) {
    size_t min_nw = zu_min(anw, bnw);

    for (size_t i = 0; i < min_nw; i++) {
        r[i] = a[i] & b[i];
    }
    for (size_t i = min_nw; i < rnw; i++) {
        r[i] = 0LU;
    }
}

// val intw_ubit_and: int64 array -> int64 array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_u_bit_and(value a_a, value a_b, value a_min_rnw, value a_max_rnw) {
    return binary_op(false, zu_max, u_bit_and, a_a, a_b, a_min_rnw, a_max_rnw);
}

static void
i_bit_op(void (*u_bit_op)(const hm_word_t *, size_t, const hm_word_t *, size_t, hm_word_t *,
  size_t), const hm_word_t *a, size_t anw, const hm_word_t *b, size_t bnw, hm_word_t *r,
  size_t rnw) {
    if (anw == bnw) {
        u_bit_op(a, anw, b, bnw, r, rnw);
    } else if (anw < bnw) {
        hm_word_t a_ext[bnw];
        dup(true, a, anw, a_ext, bnw);
        u_bit_op(a_ext, bnw, b, bnw, r, rnw);
    } else {
        assert(anw > bnw);
        hm_word_t b_ext[anw];
        dup(true, b, bnw, b_ext, anw);
        u_bit_op(a, anw, b_ext, anw, r, rnw);
    }
}

static void
i_bit_and(const hm_word_t *a, size_t anw, const hm_word_t *b, size_t bnw, hm_word_t *r,
  size_t rnw) {
    i_bit_op(u_bit_and, a, anw, b, bnw, r, rnw);
}

// val intw_ibit_and: int array -> int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_i_bit_and(value a_a, value a_b, value a_min_rnw, value a_max_rnw) {
    return binary_op(true, zu_max, i_bit_and, a_a, a_b, a_min_rnw, a_max_rnw);
}

static void
u_bit_or(const hm_word_t *a, size_t anw, const hm_word_t *b, size_t bnw, hm_word_t *r, size_t rnw) {
    size_t min_nw = zu_min(anw, bnw);
    size_t max_nw = zu_max(anw, bnw);

    for (size_t i = 0; i < min_nw; i++) {
        r[i] = a[i] | b[i];
    }
    if (anw > bnw) {
        for (size_t i = min_nw; i < anw; i++) {
            r[i] = a[i];
        }
    } else {
        for (size_t i = min_nw; i < bnw; i++) {
            r[i] = b[i];
        }
    }
    for (size_t i = max_nw; i < rnw; i++) {
        r[i] = 0LU;
    }
}

// val intw_ubit_or: int array -> int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_u_bit_or(value a_a, value a_b, value a_min_rnw, value a_max_rnw) {
    return binary_op(false, zu_max, u_bit_or, a_a, a_b, a_min_rnw, a_max_rnw);
}

static void
i_bit_or(const hm_word_t *a, size_t anw, const hm_word_t *b, size_t bnw, hm_word_t *r, size_t rnw) {
    i_bit_op(u_bit_or, a, anw, b, bnw, r, rnw);
}

// val intw_ibit_or: int array -> int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_i_bit_or(value a_a, value a_b, value a_min_rnw, value a_max_rnw) {
    return binary_op(true, zu_max, i_bit_or, a_a, a_b, a_min_rnw, a_max_rnw);
}

static void
u_bit_xor(const hm_word_t *a, size_t anw, const hm_word_t *b, size_t bnw, hm_word_t *r,
  size_t rnw) {
    size_t min_nw = zu_min(anw, bnw);
    size_t max_nw = zu_max(anw, bnw);

    for (size_t i = 0; i < min_nw; i++) {
        r[i] = a[i] ^ b[i];
    }
    if (anw > bnw) {
        for (size_t i = min_nw; i < anw; i++) {
            r[i] = a[i];
        }
    } else {
        for (size_t i = min_nw; i < bnw; i++) {
            r[i] = b[i];
        }
    }
    for (size_t i = max_nw; i < rnw; i++) {
        r[i] = 0LU;
    }
}

// val intw_ubit_xor: int array -> int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_u_bit_xor(value a_a, value a_b, value a_min_rnw, value a_max_rnw) {
    return binary_op(false, zu_max, u_bit_xor, a_a, a_b, a_min_rnw, a_max_rnw);
}

static void
i_bit_xor(const hm_word_t *a, size_t anw, const hm_word_t *b, size_t bnw, hm_word_t *r,
  size_t rnw) {
    i_bit_op(u_bit_xor, a, anw, b, bnw, r, rnw);
}

// val intw_ibit_xor: int array -> int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_i_bit_xor(value a_a, value a_b, value a_min_rnw, value a_max_rnw) {
    return binary_op(true, zu_max, i_bit_xor, a_a, a_b, a_min_rnw, a_max_rnw);
}

// val intw_ubit_not: int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_u_bit_not(value a_a, value a_min_rnw, value a_max_rnw) {
    return unary_op(false, zu_id, bit_not, a_a, a_min_rnw, a_max_rnw);
}

// val intw_ibit_not: int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_i_bit_not(value a_a, value a_min_rnw, value a_max_rnw) {
    return unary_op(true, zu_id, bit_not, a_a, a_min_rnw, a_max_rnw);
}

static void
bit_sl(unsigned shift, const hm_word_t *a, size_t anw, hm_word_t *r, size_t rnw) {
    size_t word_shift = shift / hm_bpw;
    size_t subword_shift = shift % hm_bpw;
    size_t upper_shift = subword_shift;
    size_t lower_shift = hm_bpw - subword_shift;

    //              \/      \/      \/      \/      \/
    // a:            33333333222222221111111100000000
    // r: ...........33333333222222221111111100000000.............
    //           /\      /\      /\      /\      /\      /\      /\_
    for (size_t i = 0; i < word_shift; i++) {
        r[i] = 0LU;
    }

    size_t limit = zu_min(rnw, anw + word_shift);
    if (subword_shift == 0) {
        for (size_t i = word_shift; i < limit; i++) {
            r[i] = a[i - word_shift];
        }
        for (size_t i = limit; i < rnw; i++) {
            r[i] = 0LU;
        }
    } else {
        // Lower subword is zeros.
        r[word_shift] = hm_word_sl(a[0], upper_shift);

        for (size_t i = word_shift + 1; i < limit; i++) {
            size_t upper = i - word_shift;
            size_t lower = upper - 1;
            r[i] = hm_word_sl(a[upper], upper_shift) | hm_word_usr(a[lower], lower_shift);
        }
        if (limit < rnw) {
            // Upper subword is zeros.
            r[limit] = hm_word_usr(a[limit - word_shift - 1], lower_shift);

            for (size_t i = limit + 1; i < rnw; i++) {
                r[i] = 0LU;
            }
        }
    }
}

static size_t
bit_sl_rnw(unsigned shift, size_t anw) {
    return ((shift + hm_bpw - 1) / hm_bpw) + anw;
}

// val intw_bit_usl: uns -> int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_u_bit_sl(value a_shift, value a_a, value a_min_rnw, value a_max_rnw) {
    return bit_shift_op(false, bit_sl_rnw, bit_sl, a_shift, a_a, a_min_rnw, a_max_rnw);
}

// val intw_bit_isl: uns -> int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_i_bit_sl(value a_shift, value a_a, value a_min_rnw, value a_max_rnw) {
    return bit_shift_op(true, bit_sl_rnw, bit_sl, a_shift, a_a, a_min_rnw, a_max_rnw);
}

static void
bit_usr(unsigned shift, const hm_word_t *a, size_t nw, hm_word_t *r, size_t rnw) {
    size_t nbits = nw * hm_bpw;
    shift %= nbits;
    size_t word_shift = shift / hm_bpw;
    size_t subword_shift = shift % hm_bpw;
    size_t upper_shift = hm_bpw - subword_shift;
    size_t lower_shift = subword_shift;
    //                  \/      \/      \/      \/      \/
    // a:                33333333222222221111111100000000
    // r:  ..............3333333322222222111
    //    /\       /\      /\      /\      /\_
    if (subword_shift == 0) {
        for (size_t i = 0; i < nw - word_shift; i++) {
            r[i] = a[i + word_shift];
        }
    } else {
     for (size_t i = 0; i < nw - word_shift - 1; i++) {
       size_t lower = i + word_shift;
       size_t upper = lower + 1;
       r[i] = hm_word_sl(a[upper], upper_shift) | hm_word_usr(a[lower], lower_shift);
     }
     // Upper subword is zeros.
     r[nw - word_shift - 1] = hm_word_usr(a[nw - 1], lower_shift);
    }
    for (size_t i = nw - word_shift; i < nw; i++) {
        r[i] = 0LU;
    }
}

static size_t
bit_sr_rnw(unsigned shift, size_t anw) {
    return anw;
}

// val intw_bit_usr: uns -> int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_bit_usr(value a_shift, value a_a, value a_min_rnw, value a_max_rnw) {
    return bit_shift_op(false, bit_sr_rnw, bit_usr, a_shift, a_a, a_min_rnw, a_max_rnw);
}

static void
bit_ssr(unsigned shift, const hm_word_t *a, size_t nw, hm_word_t *r, size_t rnw) {
    size_t nbits = nw * hm_bpw;
    shift %= nbits;
    size_t word_shift = shift / hm_bpw;
    size_t subword_shift = shift % hm_bpw;
    size_t upper_shift = hm_bpw - subword_shift;
    size_t lower_shift = subword_shift;
    hm_word_t msw = hm_word_usr(a[nw - 1], nbits - 1) ? 0xffffffffffffffffLU :
      0LU;
    //                  \/      \/      \/      \/      \/
    // a:                33333333222222221111111100000000
    // r:  ~~~~~~~~~~~~~~3333333322222222111
    //    /\       /\      /\      /\      /\_
    if (subword_shift == 0) {
        for (size_t i = 0; i < nw - word_shift; i++) {
            r[i] = a[i + word_shift];
        }
    } else {
        for (size_t i = 0; i < nw - word_shift - 1; i++) {
            size_t lower = i + word_shift;
            size_t upper = lower + 1;
            r[i] = hm_word_sl(a[upper], upper_shift) | hm_word_usr(a[lower], lower_shift);
        }
        r[nw - word_shift - 1] = hm_word_sl(msw, upper_shift) | hm_word_usr(a[nw - 1], lower_shift);
    }
    for (size_t i = nw - word_shift; i < nw; i++) {
        r[i] = msw;
    }
}

// val intw_bit_ssr: uns -> int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_bit_ssr(value a_shift, value a_a, value a_min_rnw, value a_max_rnw) {
    return bit_shift_op(true, bit_sr_rnw, bit_ssr, a_shift, a_a, a_min_rnw, a_max_rnw);
}

static unsigned
bit_pop(const hm_word_t *a, size_t nw) {
    unsigned pop = 0;
    for (size_t i = 0; i < nw; i++) {
        pop += __builtin_popcountl(a[i]);
    }
    return pop;
}

// val intw_bit_pop: int array -> uns
CAMLprim value
hm_basis_intw_bit_pop(value a_a) {
    return bit_count_op(bit_pop, a_a);
}

static unsigned
bit_clz(const hm_word_t *a, size_t nw) {
    unsigned lz = 0;
    for (size_t i = nw; i-- > 0;) {
        if (a[i] != 0) {
            lz += __builtin_clzl(a[i]);
            return lz;
        }
        lz += hm_bpw;
    }
    return lz;
}

// val intw_bit_clz: int array -> uns
CAMLprim value
hm_basis_intw_bit_clz(value a_a) {
    return bit_count_op(bit_clz, a_a);
}

static unsigned
bit_ctz(const hm_word_t *a, size_t nw) {
    unsigned tz = 0;
    for (size_t i = 0; i < nw; i++) {
        if (a[i] != 0) {
            tz += __builtin_ctzl(a[i]);
            return tz;
        }
        tz += hm_bpw;
    }
    return tz;
}

// val intw_bit_ctz: int array -> uns
CAMLprim value
hm_basis_intw_bit_ctz(value a_a) {
    return bit_count_op(bit_ctz, a_a);
}

static size_t
add_sub_rnw(size_t anw, size_t bnw) {
    return zu_max(anw, bnw) + 1;
}

static value
nz_add(bool signed_, value a_a, value a_b, value a_min_rnw, value a_max_rnw) {
    return binary_op_pad(signed_, signed_, add_sub_rnw, signed_ ? iadd : uadd, a_a, a_b, a_min_rnw,
      a_max_rnw);
}

// val intw_uadd: int array -> int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_u_add(value a_a, value a_b, value a_min_rnw, value a_max_rnw) {
    return nz_add(false, a_a, a_b, a_min_rnw, a_max_rnw);
}

// val intw_iadd: int array -> int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_i_add(value a_a, value a_b, value a_min_rnw, value a_max_rnw) {
    return nz_add(true, a_a, a_b, a_min_rnw, a_max_rnw);
}

// 2's complement only works for equal [ab]nw; pad for signed subtraction.
static void
sub(bool signed_, const hm_word_t *a, size_t anw, const hm_word_t *b, size_t bnw, hm_word_t *r,
  size_t rnw) {
    size_t min_nw = zu_min(anw, bnw);
    size_t max_nw = zu_max(anw, bnw);

    hm_word_t borrow = 0;
    for (size_t i = 0; i < min_nw; i++) {
        r[i] = a[i] - b[i] - borrow;
        borrow = ((~a[i] & b[i]) | ((~a[i] | b[i]) & r[i])) >> (hm_bpw - 1);
    }
    hm_word_t ai = (signed_ && is_neg(a, anw)) ? 0xffffffffffffffffLU : 0LU;
    hm_word_t bi = (signed_ && is_neg(b, bnw)) ? 0xffffffffffffffffLU : 0LU;
    if (anw > bnw) {
        for (size_t i = min_nw; i < anw; i++) {
            r[i] = a[i] - bi - borrow;
            borrow = ((~a[i] & bi) | ((~a[i] | bi) & r[i])) >> (hm_bpw - 1);
        }
    } else {
        for (size_t i = min_nw; i < bnw; i++) {
            r[i] = ai - b[i] - borrow;
            borrow = ((~ai & b[i]) | ((~ai | b[i]) & r[i])) >> (hm_bpw - 1);
        }
    }
    for (size_t i = max_nw; i < rnw; i++) {
        r[i] = ai - bi - borrow;
        borrow = ((~ai & bi) | ((~ai | bi) & r[i])) >> (hm_bpw - 1);
    }
}

static void
usub(const hm_word_t *a, size_t anw, const hm_word_t *b, size_t bnw, hm_word_t *r, size_t rnw) {
    sub(false, a, anw, b, bnw, r, rnw);
}

static void
isub(const hm_word_t *a, size_t anw, const hm_word_t *b, size_t bnw, hm_word_t *r, size_t rnw) {
    sub(true, a, anw, b, bnw, r, rnw);
}

static value
nz_sub(bool signed_, value a_a, value a_b, value a_min_rnw, value a_max_rnw) {
    return binary_op_pad(signed_, signed_, add_sub_rnw, signed_ ? isub : usub, a_a, a_b, a_min_rnw,
      a_max_rnw);
}

// val intw_usub: int array -> int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_u_sub(value a_a, value a_b, value a_min_rnw, value a_max_rnw) {
    return nz_sub(false, a_a, a_b, a_min_rnw, a_max_rnw);
}

// val intw_isub: int array -> int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_i_sub(value a_a, value a_b, value a_min_rnw, value a_max_rnw) {
    return nz_sub(true, a_a, a_b, a_min_rnw, a_max_rnw);
}

/* Compute the number of significant digits in a digit array, i.e. subtract high-order zeros from
 * the array size. */
static size_t
sig_digits(const hm_hword_t *ha, size_t ndigits) {
    for (size_t i = ndigits; i-- > 0;) {
        if (ha[i] != 0) {
            return i + 1;
        }
    }
    return 0;
}

static void
hwords_of_words(const hm_word_t *words, size_t nw, hm_hword_t *hwords) {
    for (size_t i = 0; i < nw; i++) {
        size_t i2 = i << 1;
        hwords[i2] = words[i];
        hwords[i2 + 1] = hm_word_usr(words[i], hm_bphw);
    }
}

static void
words_of_hwords(const hm_hword_t *hwords, size_t nhw, hm_word_t *words, size_t nw) {
    for (size_t i = 0; i < nw; i++) {
        size_t i2 = i << 1;
        hm_hword_t hi = (i2 + 1 < nhw) ? hwords[i2 + 1] : 0;
        hm_hword_t lo = (i2 < nhw) ? hwords[i2] : 0;
        words[i] = hm_word_sl(hi, hm_bphw) | (hm_word_t)lo;
    }
}

static void
mul(const hm_word_t *a, size_t anw, const hm_word_t *b, size_t bnw, hm_word_t *r, size_t rnw) {
    // Decompose inputs into arrays of 32-bit half-words.
    size_t and_ = anw << 1;
    size_t bnd = bnw << 1;
    size_t rnd = rnw << 1;
    hm_hword_t ah[and_];
    hm_hword_t bh[bnd];
    hm_hword_t rh[rnd];
    hwords_of_words(a, anw, ah);
    hwords_of_words(b, bnw, bh);
    for (size_t i = 0; i < rnw; i++) {
        size_t i2 = i << 1;
        rh[i2] = 0;
        rh[i2 + 1] = 0;
    }
    // Trim leading zeros.
    and_ = sig_digits(ah, and_);
    bnd = sig_digits(bh, bnd);
    rnd = zu_min(rnd, and_+bnd);

    /* Use the standard paper method of multi-digit multiplication, but in base 2^32. The full
     * result requires (and_ + bnd) digits, but we calculate/preserve at most rnd digits.
     *
     * The digit arrays are encoded as hm_hword_t, which assures that only significant bits are
     * stored. The intermediate computations use 64-bit math so that two digits fit. */
    for (size_t j = 0; j < bnd; j++) {
        hm_word_t zc = 0; // Carry.
        for (size_t i = 0; i + j < rnd ; i++) {
            if (i < and_) {
                hm_word_t za = ah[i];
                hm_word_t zb = bh[j];
                hm_word_t zr = rh[i + j];

                // 64-bit multiply and add.
                hm_word_t cd = (za * zb) + zr + zc;

                // Extract c and d from result.
                zc = hm_word_usr(cd, hm_bphw);
                hm_hword_t d = cd; // Truncate.
                rh[i + j] = d;
            } else {
                rh[i + j] = zc;
                break;
            }
        }
    }

    words_of_hwords(rh, rnd, r, rnw);
}

static size_t
n_mul_rnw(size_t anw, size_t bnw) {
    return anw + bnw;
}

static size_t
z_mul_rnw(size_t anw, size_t bnw) {
    return anw + bnw + 1;
}

static value
nz_mul(bool signed_, value a_a, value a_b, value a_min_rnw, value a_max_rnw) {
    return binary_op(signed_, signed_ ? z_mul_rnw : n_mul_rnw, mul, a_a, a_b, a_min_rnw, a_max_rnw);
}

// val intw_umul: int array -> int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_u_mul(value a_a, value a_b, value a_min_rnw, value a_max_rnw) {
    return nz_mul(false, a_a, a_b, a_min_rnw, a_max_rnw);
}

// val intw_imul: int array -> int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_i_mul(value a_a, value a_b, value a_min_rnw, value a_max_rnw) {
    return nz_mul(true, a_a, a_b, a_min_rnw, a_max_rnw);
}

static void
u_div_mod(const hm_word_t *a, size_t anw, const hm_word_t *b, size_t bnw, hm_word_t *q, size_t qnw,
  hm_word_t *r, size_t rnw) {
    const hm_word_t base = 1LU << hm_bphw;
    const hm_word_t digit_mask = base - 1;
    size_t and_ = anw << 1;
    size_t bnd = bnw << 1;
    // Avoid {qh,rh} bounds checking during computation. Truncation may still occur when copying
    // results from {qh,rh} to {q,r}.
    size_t qnd = and_;
    size_t rnd = and_;
    // Decompose inputs into arrays of 32-bit half-words.
    hm_hword_t ah[and_]; hwords_of_words(a, anw, ah);
    hm_hword_t bh[bnd]; hwords_of_words(b, bnw, bh);
    /* Compute quotient and remainder using an algorithm similar to the paper long division
     * algorithm, but in base 2^32.
     *
     * The digit arrays are encoded as hm_hword_t, which assures that only significant bits are
     * stored. The intermediate computations use 64-bit math so that two digits fit. */
    size_t m = sig_digits(ah, and_);
    size_t n = sig_digits(bh, bnd);

    assert(n != 0);
    if (m < n) {
        if (q != NULL) {
            init_zero(q, qnw);
        }
        if (r != NULL) {
            dup(false, a, anw, r, rnw);
        }
        return;
    }
    if (n == 1) {
        // Single-digit divisor.
        hm_hword_t qh[qnd];
        for (size_t j = qnd; j-- > m;) {
            qh[j] = 0;
        }
        hm_iword_t carry = 0;
        for (size_t j = m; j-- > 0;) {
            hm_word_t t = hm_word_sl(carry, hm_bphw) + ah[j];
            qh[j] = t / bh[0];
            carry = t - qh[j] * bh[0];
        }
        if (q != NULL) {
            words_of_hwords(qh, qnd, q, qnw);
        }
        if (r != NULL) {
            init_u(carry & digit_mask, r, rnw);
        }
        return;
    }
    /* Choose a normalization power-of-2 multiplier such that the divisor is losslessly shifted left
     * as far as possible. */
    unsigned shift = __builtin_clzl((hm_word_t)bh[n-1]) - hm_bphw;
    // Initialize normalized divisor.
    hm_hword_t vh[n];
    for (size_t i = n; i-- > 1;) {
        vh[i] = hm_word_sl(bh[i], shift) | hm_word_usr(bh[i-1], (hm_bphw - shift));
    }
    vh[0] = bh[0] << shift;
    // Initialize normalized dividend.
    hm_hword_t uh[m+1];
    uh[m] = hm_word_usr(ah[m-1], hm_bphw - shift);
    for (size_t i = m; i-- > 1;) {
        uh[i] = hm_word_sl(ah[i], shift) | hm_word_usr(ah[i-1], hm_bphw - shift);
    }
    uh[0] = ah[0] << shift;
    hm_hword_t qh[qnd];
    hm_hword_t rh[rnd];
    // Main computation.
    for (size_t j = qnd; j-- > m - n + 1;) {
        qh[j] = 0;
    }
    for (size_t j = m - n + 1; j-- > 0;) {
        /* Compute quotient digit estimate and remainder. It is possible that qdigit is one larger
         * than the correct value, in which case subsequent correction code executes. */
        hm_word_t qdigit;
        {
            hm_word_t t = hm_word_sl(uh[j+n], hm_bphw) + (hm_word_t)uh[j+n-1];
            /* / and % are inseparable at the microarchitecture level, so use % to get the
             * already-computed remainder rather than manually computing it based on qdigit. */
            qdigit = t / (hm_word_t)vh[n-1];
            hm_word_t rem = t % (hm_word_t)vh[n-1];

            /* Once the first part of the following conditional is satisfied, qdigit fits in a half
             * word, so it can be safely cast to hm_hword_t as a manual strength reduction
             * optimization. */
            while (qdigit >= base || (hm_hword_t)qdigit * (hm_word_t)vh[n-2] > base * rem +
              (hm_word_t)uh[j+n-2]) {
                qdigit--;
                rem += (hm_word_t)vh[n-1];
                if (rem >= base) break;
            }
        }
        // Multiply and subtract.
        hm_iword_t carry = 0;
        for (size_t i = 0; i < n; i++) {
            hm_word_t p = (hm_hword_t)qdigit * (hm_word_t)vh[i];
            hm_iword_t t = (hm_word_t)uh[i+j] - carry - (p & digit_mask);
            uh[i+j] = t;
            carry = hm_word_usr(p, hm_bphw) - hm_word_ssr(t, hm_bphw);
        }
        hm_iword_t t = (hm_word_t)uh[j+n] - carry;
        uh[j+n] = t;
        // Store quotient digit.
        if (t >= 0) {
            qh[j] = qdigit;
        } else {
            // Subtracted too much; correct.
            qh[j] = qdigit - 1;
            carry = 0;
            for (size_t i = 0; i < n; i++) {
                hm_word_t t = (hm_word_t)uh[i+j] + vh[i] + carry;
                uh[i+j] = t;
                carry = hm_word_ssr(t, hm_bphw);
            }
            uh[j+n] += carry;
        }
    }
    if (q != NULL) {
        words_of_hwords(qh, qnd, q, qnw);
    }
    if (r != NULL) {
        // Denormalize remainder.
        for (size_t i = 0; i < n-1; i++) {
            rh[i] = hm_word_usr(uh[i], shift) | hm_word_sl(uh[i+1], hm_bphw - shift);
        }
        rh[n-1] = hm_word_usr(uh[n-1], shift) | hm_word_sl(uh[n], hm_bphw - shift);
        for (size_t i = n; i < rnd; i++) {
            rh[i] = 0;
        }
        words_of_hwords(rh, rnd, r, rnw);
    }
}

static size_t
div_rnw(size_t anw, size_t bnw) {
    return anw;
}

static void
udiv(const hm_word_t *a, size_t anw, const hm_word_t *b, size_t bnw, hm_word_t *q, size_t qnw) {
    u_div_mod(a, anw, b, bnw, q, qnw, NULL, 0);
}

// val intw_udiv: int array -> int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_u_div(value a_a, value a_b, value a_min_rnw, value a_max_rnw) {
    return binary_op(false, div_rnw, udiv, a_a, a_b, a_min_rnw, a_max_rnw);
}

static size_t
mod_rnw(size_t anw, size_t bnw) {
    return bnw;
}

static void
umod(const hm_word_t *a, size_t anw, const hm_word_t *b, size_t bnw, hm_word_t *r, size_t rnw) {
    u_div_mod(a, anw, b, bnw, NULL, 0, r, rnw);
}

// val intw_umod: int array -> int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_u_mod(value a_a, value a_b, value a_min_rnw, value a_max_rnw) {
    return binary_op(false, mod_rnw, umod, a_a, a_b, a_min_rnw, a_max_rnw);
}

static void
i_div_mod(const hm_word_t *a, size_t anw, const hm_word_t *b, size_t bnw, hm_word_t *q, size_t qnw,
  hm_word_t *r, size_t rnw) {
    hm_word_t a_abs[anw+1];
    bool a_is_neg = is_neg_abs(a, anw, a_abs, anw+1);
    hm_word_t b_abs[bnw+1];
    bool b_is_neg = is_neg_abs(b, bnw, b_abs, bnw+1);
    u_div_mod(a_abs, anw+1, b_abs, bnw+1, q, qnw, r, rnw);
    if (q != NULL && a_is_neg != b_is_neg) {
        hm_word_t q_ext[qnw+1];
        neg(q, qnw, q_ext, qnw+1);
        dup(true, q_ext, qnw+1, q, qnw);
    }
    if (r != NULL && a_is_neg) {
        hm_word_t r_ext[rnw+1];
        neg(r, rnw, r_ext, rnw+1);
        dup(true, r_ext, rnw+1, r, rnw);
    }
}

static void
idiv(const hm_word_t *a, size_t anw, const hm_word_t *b, size_t bnw, hm_word_t *q, size_t qnw) {
    i_div_mod(a, anw, b, bnw, q, qnw, NULL, 0);
}

// val intw_idiv: int array -> int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_i_div(value a_a, value a_b, value a_min_rnw, value a_max_rnw) {
    return binary_op(true, div_rnw, idiv, a_a, a_b, a_min_rnw, a_max_rnw);
}

static void
imod(const hm_word_t *a, size_t anw, const hm_word_t *b, size_t bnw, hm_word_t *r, size_t rnw) {
    i_div_mod(a, anw, b, bnw, NULL, 0, r, rnw);
}

// val intw_imod: int array -> int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_i_mod(value a_a, value a_b, value a_min_rnw, value a_max_rnw) {
    return binary_op(true, mod_rnw, imod, a_a, a_b, a_min_rnw, a_max_rnw);
}

// val intw_ineg: int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_i_neg(value a_a, value a_min_rnw, value a_max_rnw) {
    return unary_op(true, zu_succ, neg, a_a, a_min_rnw, a_max_rnw);
}

static void
abs_(const hm_word_t *a, size_t anw, hm_word_t *r, size_t rnw) {
    is_neg_abs(a, anw, r, rnw);
}

// val intw_iabs: int array -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_i_abs(value a_a, value a_min_rnw, value a_max_rnw) {
    return unary_op(true, zu_succ, abs_, a_a, a_min_rnw, a_max_rnw);
}

static value
of_real(bool signed_, value a_r, value a_min_rnw, value a_max_rnw) {
    size_t min_rnw = Int64_val(a_min_rnw);
    // Results never need more than 1+1024 bits. Limit max_rnw as such to avoid stack overflow.
    size_t max_rnw = zu_min(Int64_val(a_max_rnw), 1 + (1024 / hm_bpw));
    size_t rnw = max_rnw;
    size_t nbits = rnw * hm_bpw;
    size_t sig_bits = rnw * hm_bpw - (signed_ ? 1 : 0);
    union {
        double f;
        uint64_t bits;
    } u;
    u.f = Double_val(a_r);
    hm_word_t r[rnw];
    switch (fpclassify(u.f)) {
    case FP_NORMAL: {
        if (!signed_ && signbit(u.f)) {
            init_zero(r, rnw);
            break;
        }
        int biased_exponent = 0x7ff & (u.bits >> 52);
        if (biased_exponent < 1023) {
            init_zero(r, rnw);
            break;
        }
        unsigned exponent = biased_exponent - 1023;
        if (exponent >= sig_bits + 52) {
            init_zero(r, rnw);
            break;
        }

        hm_word_t t_pos[rnw];
        init_u(0x10000000000000LU | (u.bits & 0xfffffffffffffLU), t_pos, rnw);
        hm_word_t t[rnw];
        bool negative = (signed_ && signbit(u.f));
        if (negative) {
            neg(t_pos, rnw, t, rnw);
        } else {
            dup(false, t_pos, rnw, t, rnw);
        }
        if (exponent >= 52) {
            if (signed_) {
                hm_word_t u[rnw];
                if (negative) {
                    hm_word_t u[rnw];
                    init_one(u, rnw);
                    hm_word_t sign_neg[rnw];
                    bit_sl(nbits-1, u, rnw, sign_neg, rnw);
                    bit_sl(exponent - 52, t, rnw, u, rnw);
                    u_bit_or(sign_neg, rnw, u, rnw, r, rnw);
                } else {
                    bit_sl(exponent - 51, t, rnw, u, rnw);
                    bit_usr(1, u, rnw, r, rnw);
                }
            } else {
                bit_sl(exponent - 52, t, rnw, r, rnw);
            }
        } else {
            bit_ssr(52 - exponent, t, rnw, r, rnw);
        }
        break;
    }
    case FP_SUBNORMAL:
    case FP_ZERO:
    case FP_INFINITE:
        init_zero(r, rnw);
        break;
    case FP_NAN:
    default: assert(false);
    }

    return oarray_of_uarray(signed_, r, rnw, min_rnw, rnw);
}

// val intw_i_of_real: real -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_i_of_real(value a_real, value a_min_rnw, value a_max_rnw) {
    return of_real(true, a_real, a_min_rnw, a_max_rnw);
}

// val intw_u_of_real: real -> uns -> uns -> int64 array
CAMLprim value
hm_basis_intw_u_of_real(value a_real, value a_min_rnw, value a_max_rnw) {
    return of_real(false, a_real, a_min_rnw, a_max_rnw);
}

static value
real_of_parts(bool negative, unsigned exponent, uint64_t mantissa) {
    assert((mantissa & 0xfffffffffffffLU) == mantissa);
    uint64_t sign = negative ? 1 : 0;
    uint64_t biased_exponent = exponent + 1023;
    union {
        double f;
        uint64_t bits;
    } u;
    u.bits = (sign << 63) | (biased_exponent << 52) | mantissa;
    return caml_copy_double(u.f);
}

static value
to_real(bool signed_, value a_a) {
    size_t anw = oarray_length(a_a);
    size_t nbits = anw * hm_bpw;
    hm_word_t a[anw];
    uarray_of_cbs(signed_, a_a, a, anw);
    bool negative = (signed_ && is_neg(a, anw));
    if (negative) {
        if (is_signed_min_value(a, anw)) {
            return real_of_parts(true, nbits - 1, 0);
        }
        hm_word_t t[anw];
        dup(false, a, anw, t, anw);
        neg(t, anw, a, anw);
    }
    unsigned sig_bits = nbits - bit_clz(a, anw);
    if (sig_bits == 0) {
        return caml_copy_double(0.0);
    }
    /* Shift such that the most significant 1 bit is at offset 52 from the least significant bit.
     * The least significant 52 bits become the mantissa (bit 52 will be discarded). */
    unsigned exponent = sig_bits - 1;
    hm_word_t t[anw];
    if (sig_bits < 53) {
        bit_sl(53 - sig_bits, a, anw, t, anw);
    } else {
        bit_usr(sig_bits - 53, a, anw, t, anw);
    }
    uint64_t mantissa = t[0] & 0xfffffffffffffLU;
    return real_of_parts(negative, exponent, mantissa);
}

// val intw_i_to_real: int array -> real
CAMLprim value
hm_basis_intw_i_to_real(value a_a) {
    return to_real(true, a_a);
}

// val intw_u_to_real: int array -> real
CAMLprim value
hm_basis_intw_u_to_real(value a_a) {
    return to_real(false, a_a);
}
