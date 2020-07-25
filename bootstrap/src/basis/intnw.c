#include <stddef.h>
#include <stdbool.h>
#include <math.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

#define NDEBUG
#include <assert.h>

// Word.
typedef uint64_t hl_word_t;

// Signed word.
typedef int64_t hl_iword_t;

// Half word.
typedef uint32_t hl_hword_t;

// Bits per word.
static const size_t hl_bpw = sizeof(hl_word_t) << 3;

// Bits per half word.
static const size_t hl_bphw = sizeof(hl_hword_t) << 3;

static hl_word_t
hl_word_sl(hl_word_t x, unsigned shift) {
  return (x << shift);
}

static hl_word_t
hl_word_usr(hl_word_t x, unsigned shift) {
  return (x >> shift);
}

static hl_word_t
hl_word_ssr(hl_word_t x, unsigned shift) {
  return ((hl_iword_t)x >> shift);
}

static value
value_of_int64(const char *arg) {
  hl_iword_t x = *((hl_iword_t *)arg);
  return caml_copy_int64(x);
}

static value
oarray_of_uarray(const hl_word_t *a, size_t nwords) {
  const hl_word_t *result[nwords + 1];
  for (size_t i = 0; i < nwords; i++) {
    result[i] = &a[i];
  }
  result[nwords] = NULL;
  return caml_alloc_array(value_of_int64, (const char **)result);
}

static void
init_u(hl_word_t u, size_t nwords, hl_word_t *r) {
  r[0] = u;
  for (size_t i = 1; i < nwords; i++) {
    r[i] = 0LU;
  }
}

static void
init_zero(size_t nwords, hl_word_t *r) {
  init_u(0LU, nwords, r);
}

static void
init_one(size_t nwords, hl_word_t *r) {
  init_u(1LU, nwords, r);
}

static void
init_i(hl_word_t x, size_t nwords, hl_word_t *r) {
  if ((x & 0x8000000000000000LU) == 0) {
    init_u(x, nwords, r);
    return;
  }
  r[0] = x;
  for (size_t i = 1; i < nwords; i++) {
    r[i] = 0xffffffffffffffffLU;
  }
}

static void
init_bit_all(size_t nwords, hl_word_t *r) {
  init_i(0xffffffffffffffffLU, nwords, r);
}

static void
uarray_of_oarray(value a_a, size_t nwords, hl_word_t *r) {
  for (size_t i = 0; i < nwords; i++) {
    value v = Field(a_a, i);
    r[i] = (hl_word_t)Int64_val(v);
  }
}

static void
iarray_of_oarray(value a_a, size_t nwords, hl_iword_t *r) {
  uarray_of_oarray(a_a, nwords, (hl_word_t *)r);
}

static int
icmp(const hl_iword_t *a, const hl_iword_t *b, size_t nwords) {
  for (size_t i = nwords; i-- > 0;) {
    int rel = (a[i] > b[i]) - (a[i] < b[i]);
    if (rel != 0) return rel;
  }
  return 0;
}

// val hemlock_intnw_icmp: int64 array -> int64 array -> Cmp.t
CAMLprim value
hemlock_intnw_icmp(value a_a, value a_b) {
  size_t nwords = caml_array_length(a_a);
  hl_word_t a[nwords];
  hl_word_t b[nwords];
  iarray_of_oarray(a_a, nwords, a);
  iarray_of_oarray(a_b, nwords, b);

  int rel = icmp(a, b, nwords);
  // Cmp.t is {Lt,Eq,Gt} = {0,1,2}.
  return Val_long(rel + 1);
}

static int
ucmp(const hl_word_t *a, const hl_word_t *b, size_t nwords) {
  for (size_t i = nwords; i-- > 0;) {
    int rel = (a[i] > b[i]) - (a[i] < b[i]);
    if (rel != 0) return rel;
  }
  return 0;
}

// val hemlock_intnw_ucmp: int64 array -> int64 array -> Cmp.t
CAMLprim value
hemlock_intnw_ucmp(value a_a, value a_b) {
  size_t nwords = caml_array_length(a_a);
  hl_word_t a[nwords];
  hl_word_t b[nwords];
  uarray_of_oarray(a_a, nwords, a);
  uarray_of_oarray(a_b, nwords, b);

  int rel = ucmp(a, b, nwords);
  // Cmp.t is {Lt,Eq,Gt} = {0,1,2}.
  return Val_long(rel + 1);
}

static value
binary_op(value a_a, value a_b,
    void (*op)(const hl_word_t *, const hl_word_t *, size_t, hl_word_t *)) {
  size_t nwords = caml_array_length(a_a);
  hl_word_t a[nwords];
  hl_word_t b[nwords];
  uarray_of_oarray(a_a, nwords, a);
  uarray_of_oarray(a_b, nwords, b);

  hl_word_t r[nwords];
  op(a, b, nwords, r);

  return oarray_of_uarray(r, nwords);
}

static value
unary_op(value a_a, void (*op)(const hl_word_t *, size_t, hl_word_t *)) {
  size_t nwords = caml_array_length(a_a);
  hl_word_t a[nwords];
  uarray_of_oarray(a_a, nwords, a);

  hl_word_t r[nwords];
  op(a, nwords, r);

  return oarray_of_uarray(r, nwords);
}

static value
bit_shift_op(value a_shift, value a_a,
    void (*op)(unsigned, const hl_word_t *, size_t, hl_word_t *)) {
  unsigned shift = Unsigned_long_val(a_shift);
  size_t nwords = caml_array_length(a_a);
  hl_word_t a[nwords];
  uarray_of_oarray(a_a, nwords, a);

  hl_word_t r[nwords];
  op(shift, a, nwords, r);

  return oarray_of_uarray(r, nwords);
}

static value
bit_count_op(value a_a, unsigned (*op)(const hl_word_t *, size_t)) {
  size_t nwords = caml_array_length(a_a);
  hl_word_t a[nwords];
  uarray_of_oarray(a_a, nwords, a);

  return Val_long(op(a, nwords));
}

static void
bit_and(const hl_word_t *a, const hl_word_t *b, size_t nwords, hl_word_t *r) {
  for (size_t i = 0; i < nwords; i++) {
    r[i] = a[i] & b[i];
  }
}

// val hemlock_intnw_bit_and: int64 array -> int64 array -> int64 array
CAMLprim value
hemlock_intnw_bit_and(value a_a, value a_b) {
  return binary_op(a_a, a_b, bit_and);
}

static void
bit_or(const hl_word_t *a, const hl_word_t *b, size_t nwords, hl_word_t *r) {
  for (size_t i = 0; i < nwords; i++) {
    r[i] = a[i] | b[i];
  }
}

// val hemlock_intnw_bit_or: int64 array -> int64 array -> int64 array
CAMLprim value
hemlock_intnw_bit_or(value a_a, value a_b) {
  return binary_op(a_a, a_b, bit_or);
}

static void
bit_xor(const hl_word_t *a, const hl_word_t *b, size_t nwords, hl_word_t *r) {
  for (size_t i = 0; i < nwords; i++) {
    r[i] = a[i] ^ b[i];
  }
}

// val hemlock_intnw_bit_xor: int64 array -> int64 array -> int64 array
CAMLprim value
hemlock_intnw_bit_xor(value a_a, value a_b) {
  return binary_op(a_a, a_b, bit_xor);
}

static void
bit_not(const hl_word_t *a, size_t nwords, hl_word_t *r) {
  for (size_t i = 0; i < nwords; i++) {
    r[i] = ~a[i];
  }
}

// val hemlock_intnw_bit_not: int64 array -> int64 array
CAMLprim value
hemlock_intnw_bit_not(value a_a) {
  return unary_op(a_a, bit_not);
}

static void
bit_sl(unsigned shift, const hl_word_t *a, size_t nwords, hl_word_t *r) {
  size_t nbits = nwords * hl_bpw;
  shift %= nbits;
  size_t word_shift = shift / hl_bpw;
  size_t subword_shift = shift % hl_bpw;
  size_t upper_shift = subword_shift;
  size_t lower_shift = hl_bpw - subword_shift;
  //   \/      \/      \/      \/      \/
  // a: 33333333222222221111111100000000
  // r:             22221111111100000000.............
  //               /\       /\      /\      /\      /\_
  for (size_t i = 0; i < word_shift; i++) {
    r[i] = 0LU;
  }
  if (subword_shift == 0) {
    for (size_t i = word_shift; i < nwords; i++) {
      r[i] = a[i - word_shift];
    }
  } else {
     // Lower subword is zeros.
     r[word_shift] = hl_word_sl(a[0], upper_shift);
     for (size_t i = word_shift + 1; i < nwords; i++) {
       size_t upper = i - word_shift;
       size_t lower = upper - 1;
       r[i] = hl_word_sl(a[upper], upper_shift) |
           hl_word_usr(a[lower], lower_shift);
     }
  }
}

// val hemlock_intnw_bit_sl: uns -> int64 array -> int64 array
CAMLprim value
hemlock_intnw_bit_sl(value a_shift, value a_a) {
  return bit_shift_op(a_shift, a_a, bit_sl);
}

static void
bit_usr(unsigned shift, const hl_word_t *a, size_t nwords, hl_word_t *r) {
  size_t nbits = nwords * hl_bpw;
  shift %= nbits;
  size_t word_shift = shift / hl_bpw;
  size_t subword_shift = shift % hl_bpw;
  size_t upper_shift = hl_bpw - subword_shift;
  size_t lower_shift = subword_shift;
  //                  \/      \/      \/      \/      \/
  // a:                33333333222222221111111100000000
  // r:  ..............3333333322222222111
  //    /\       /\      /\      /\      /\_
  if (subword_shift == 0) {
    for (size_t i = 0; i < nwords - word_shift; i++) {
      r[i] = a[i + word_shift];
    }
  } else {
     for (size_t i = 0; i < nwords - word_shift - 1; i++) {
       size_t lower = i + word_shift;
       size_t upper = lower + 1;
       r[i] = hl_word_sl(a[upper], upper_shift) |
           hl_word_usr(a[lower], lower_shift);
     }
     // Upper subword is zeros.
     r[nwords - word_shift - 1] = hl_word_usr(a[nwords - 1], lower_shift);
  }
  for (size_t i = nwords - word_shift; i < nwords; i++) {
    r[i] = 0LU;
  }
}

// val hemlock_intnw_bit_usr: uns -> int64 array -> int64 array
CAMLprim value
hemlock_intnw_bit_usr(value a_shift, value a_a) {
  return bit_shift_op(a_shift, a_a, bit_usr);
}

static void
bit_ssr(unsigned shift, const hl_word_t *a, size_t nwords, hl_word_t *r) {
  size_t nbits = nwords * hl_bpw;
  shift %= nbits;
  size_t word_shift = shift / hl_bpw;
  size_t subword_shift = shift % hl_bpw;
  size_t upper_shift = hl_bpw - subword_shift;
  size_t lower_shift = subword_shift;
  hl_word_t msw = hl_word_usr(a[nwords - 1], nbits - 1) ? 0xffffffffffffffffLU :
      0LU;
  //                  \/      \/      \/      \/      \/
  // a:                33333333222222221111111100000000
  // r:  ~~~~~~~~~~~~~~3333333322222222111
  //    /\       /\      /\      /\      /\_
  if (subword_shift == 0) {
    for (size_t i = 0; i < nwords - word_shift; i++) {
      r[i] = a[i + word_shift];
    }
  } else {
     for (size_t i = 0; i < nwords - word_shift - 1; i++) {
       size_t lower = i + word_shift;
       size_t upper = lower + 1;
       r[i] = hl_word_sl(a[upper], upper_shift) |
           hl_word_usr(a[lower], lower_shift);
     }
     r[nwords - word_shift - 1] = hl_word_sl(msw, upper_shift) |
         hl_word_usr(a[nwords - 1], lower_shift);
  }
  for (size_t i = nwords - word_shift; i < nwords; i++) {
    r[i] = msw;
  }
}

// val hemlock_intnw_bit_ssr: uns -> int64 array -> int64 array
CAMLprim value
hemlock_intnw_bit_ssr(value a_shift, value a_a) {
  return bit_shift_op(a_shift, a_a, bit_ssr);
}

static unsigned
bit_pop(const hl_word_t *a, size_t nwords) {
  unsigned pop = 0;
  for (size_t i = 0; i < nwords; i++) {
    pop += __builtin_popcountl(a[i]);
  }
  return pop;
}

// val hemlock_intnw_bit_pop: int64 array -> uns
CAMLprim value
hemlock_intnw_bit_pop(value a_a) {
  return bit_count_op(a_a, bit_pop);
}

static unsigned
bit_clz(const hl_word_t *a, size_t nwords) {
  unsigned lz = 0;
  for (size_t i = nwords; i-- > 0;) {
    if (a[i] != 0) {
      lz += __builtin_clzl(a[i]);
      return lz;
    }
    lz += hl_bpw;
  }
  return lz;
}

// val hemlock_intnw_bit_clz: int64 array -> uns
CAMLprim value
hemlock_intnw_bit_clz(value a_a) {
  return bit_count_op(a_a, bit_clz);
}

static unsigned
bit_ctz(const hl_word_t *a, size_t nwords) {
  unsigned tz = 0;
  for (size_t i = 0; i < nwords; i++) {
    if (a[i] != 0) {
      tz += __builtin_ctzl(a[i]);
      return tz;
    }
    tz += hl_bpw;
  }
  return tz;
}

// val hemlock_intnw_bit_ctz: int64 array -> uns
CAMLprim value
hemlock_intnw_bit_ctz(value a_a) {
  return bit_count_op(a_a, bit_ctz);
}

static void
add(const hl_word_t *a, const hl_word_t *b, size_t nwords, hl_word_t *r) {
  hl_word_t carry = 0;
  for (size_t i = 0; i < nwords; i++) {
    r[i] = a[i] + b[i] + carry;
    carry = ((a[i] & b[i]) | ((a[i] | b[i]) & ~r[i])) >> (hl_bpw - 1);
  }
}

// val hemlock_intnw_add: int64 array -> int64 array -> int64 array
CAMLprim value
hemlock_intnw_add(value a_a, value a_b) {
  return binary_op(a_a, a_b, add);
}

static void
sub(const hl_word_t *a, const hl_word_t *b, size_t nwords, hl_word_t *r) {
  hl_word_t borrow = 0;
  for (size_t i = 0; i < nwords; i++) {
    r[i] = a[i] - b[i] - borrow;
    borrow = ((~a[i] & b[i]) | ((~a[i] | b[i]) & r[i])) >> (hl_bpw - 1);
  }
}

// val hemlock_intnw_sub: int64 array -> int64 array -> int64 array
CAMLprim value
hemlock_intnw_sub(value a_a, value a_b) {
  return binary_op(a_a, a_b, sub);
}

static void
mul(const hl_word_t *a, const hl_word_t *b, size_t nwords, hl_word_t *r) {
  // Decompose inputs into arrays of 32-bit half-words.
  size_t ndigits = nwords << 1;
  hl_hword_t ah[ndigits];
  hl_hword_t bh[ndigits];
  hl_hword_t rh[ndigits];
  for (size_t i = 0; i < nwords; i++) {
    size_t i2 = i << 1;
    ah[i2] = a[i];
    ah[i2 + 1] = hl_word_usr(a[i], hl_bphw);

    bh[i2] = b[i];
    bh[i2 + 1] = hl_word_usr(b[i], hl_bphw);

    rh[i2] = 0;
    rh[i2 + 1] = 0;
  }

  /* Use the standard paper method of multi-digit multiplication, but in base
   * 2^32. The full result requires m + n digits, where m and n are the number
   * of input digits in the multiplier and multiplicand. For this function,
   * ndigits=m=n, and we only calculate/preserve the lowest ndigits digits.
   *
   * The digit arrays are encoded as hl_hword_t, which assures that only
   * significant bits are stored. The intermediate computations use 64-bit math
   * so that two digits fit. */
  for (size_t j = 0; j < ndigits; j++) {
    hl_word_t zc = 0; // Carry.
    for (size_t i = 0; i + j < ndigits; i++) {
      // Extend inputs to 64 bits.
      hl_word_t za = ah[i];
      hl_word_t zb = bh[j];
      hl_word_t zr = rh[i + j];

      // 64-bit multiply and add.
      hl_word_t cd = (za * zb) + zr + zc;

      // Extract c and d from result.
      zc = hl_word_usr(cd, hl_bphw);
      hl_hword_t d = cd; // Truncate.
      rh[i + j] = d;
    }
  }

  // Assemble result from half-words.
  for (size_t i = 0; i < nwords; i++) {
    size_t i2 = i << 1;
    r[i] = hl_word_sl(rh[i2 + 1], hl_bphw) | (hl_word_t)rh[i2];
  }
}

// val hemlock_intnw_mul: int64 array -> int64 array -> int64 array
CAMLprim value
hemlock_intnw_mul(value a_a, value a_b) {
  return binary_op(a_a, a_b, mul);
}

/* Compute the number of significant digits in a digit array, i.e. subtract
 * high-order zeros from the array size. */
static size_t
sig_digits(const hl_hword_t *ha, size_t ndigits) {
  for (size_t i = ndigits; i-- > 0;) {
    if (ha[i] != 0) {
      return i + 1;
    }
  }
  return 0;
}

static void
dup(const hl_word_t *a, size_t nwords, hl_word_t *r) {
  for (size_t i = 0; i < nwords; i++) {
    r[i] = a[i];
  }
}

static void
hwords_of_words(const hl_word_t *words, size_t nwords, hl_hword_t *hwords) {
  for (size_t i = 0; i < nwords; i++) {
    size_t i2 = i << 1;
    hwords[i2] = words[i];
    hwords[i2 + 1] = hl_word_usr(words[i], hl_bphw);
  }
}

static void
words_of_hwords(const hl_hword_t *hwords, size_t nwords, hl_word_t *words) {
  for (size_t i = 0; i < nwords; i++) {
    size_t i2 = i << 1;
    words[i] = hl_word_sl(hwords[i2 + 1], hl_bphw) | (hl_word_t)hwords[i2];
  }
}

static void
u_div_mod(const hl_word_t *a, const hl_word_t *b, size_t nwords, hl_word_t *q,
    hl_word_t *r) {
  const hl_word_t base = 1LU << hl_bphw;
  const hl_word_t digit_mask = base - 1;
  size_t ndigits = nwords << 1;
  // Decompose inputs into arrays of 32-bit half-words.
  hl_hword_t ah[ndigits]; hwords_of_words(a, nwords, ah);
  hl_hword_t bh[ndigits]; hwords_of_words(b, nwords, bh);
  /* Compute quotient and remainder using an algorithm similar to the paper long
   * division algorithm, but in base 2^32.
   *
   * The digit arrays are encoded as hl_hword_t, which assures that only
   * significant bits are stored. The intermediate computations use 64-bit math
   * so that two digits fit. */
  size_t m = sig_digits(ah, ndigits);
  size_t n = sig_digits(bh, ndigits);

  assert(n != 0);
  if (m < n) {
    init_zero(nwords, q);
    if (r != NULL) {
      dup(a, nwords, r);
    }
    return;
  }
  if (n == 1) {
    // Single-digit divisor.
    hl_hword_t qh[ndigits];
    for (size_t j = ndigits; j-- > m;) {
      qh[j] = 0;
    }
    hl_iword_t carry = 0;
    for (size_t j = m; j-- > 0;) {
      hl_word_t t = hl_word_sl(carry, hl_bphw) + ah[j];
      qh[j] = t / bh[0];
      carry = t - qh[j] * bh[0];
    }
    words_of_hwords(qh, nwords, q);
    if (r != NULL) {
      init_u(carry & digit_mask, nwords, r);
    }
    return;
  }
  /* Choose a normalization power-of-2 multiplier such that the divisor is
   * losslessly shifted left as far as possible. */
  unsigned shift = __builtin_clzl((hl_word_t)bh[n-1]) - hl_bphw;
  // Initialize normalized divisor.
  hl_hword_t vh[n];
  for (size_t i = n; i-- > 1;) {
    vh[i] = hl_word_sl(bh[i], shift) | hl_word_usr(bh[i-1], (hl_bphw - shift));
  }
  vh[0] = bh[0] << shift;
  // Initialize normalized dividend.
  hl_hword_t uh[m+1];
  uh[m] = hl_word_usr(ah[m-1], hl_bphw - shift);
  for (size_t i = m; i-- > 1;) {
    uh[i] = hl_word_sl(ah[i], shift) | hl_word_usr(ah[i-1], hl_bphw - shift);
  }
  uh[0] = ah[0] << shift;
  hl_hword_t qh[ndigits];
  hl_hword_t rh[ndigits];
  // Main computation.
  for (size_t j = ndigits; j-- > m - n + 1;) {
    qh[j] = 0;
  }
  for (size_t j = m - n + 1; j-- > 0;) {
    /* Compute quotient digit estimate and remainder. It is possible that
     * qdigit is one larger than the correct value, in which case subsequent
     * correction code executes. */
    hl_word_t qdigit;
    {
      hl_word_t t = hl_word_sl(uh[j+n], hl_bphw) + (hl_word_t)uh[j+n-1];
      qdigit = t / (hl_word_t)vh[n-1];
      hl_word_t rem = t - qdigit * (hl_word_t)vh[n-1];
      while (qdigit >= base || qdigit * (hl_word_t)vh[n-2] > base * rem +
          (hl_word_t)uh[j+n-2]) {
        qdigit--;
        rem += (hl_word_t)vh[n-1];
        if (rem >= base) break;
      }
    }
    // Multiply and subtract.
    hl_iword_t carry = 0;
    for (size_t i = 0; i < n; i++) {
      hl_word_t p = qdigit * (hl_word_t)vh[i];
      hl_iword_t t = (hl_word_t)uh[i+j] - carry - (p & digit_mask);
      uh[i+j] = t;
      carry = hl_word_usr(p, hl_bphw) - hl_word_ssr(t, hl_bphw);
    }
    hl_iword_t t = (hl_word_t)uh[j+n] - carry;
    uh[j+n] = t;
    // Store quotient digit.
    if (t >= 0) {
      qh[j] = qdigit;
    } else {
      // Subtracted too much; correct.
      qh[j] = qdigit - 1;
      carry = 0;
      for (size_t i = 0; i < n; i++) {
        hl_word_t t = (hl_word_t)uh[i+j] + vh[i] + carry;
        uh[i+j] = t;
        carry = hl_word_ssr(t, hl_bphw);
      }
      uh[j+n] += carry;
    }
  }
  words_of_hwords(qh, nwords, q);
  if (r != NULL) {
    // Denormalize remainder.
    for (size_t i = 0; i < n-1; i++) {
      rh[i] = hl_word_usr(uh[i], shift) | hl_word_sl(uh[i+1], hl_bphw - shift);
    }
    rh[n-1] = hl_word_usr(uh[n-1], shift) | hl_word_sl(uh[n], hl_bphw - shift);
    for (size_t i = n; i < ndigits; i++) {
      rh[i] = 0;
    }
    words_of_hwords(rh, nwords, r);
  }
}

static void
udiv(const hl_word_t *a, const hl_word_t *b, size_t nwords, hl_word_t *q) {
  u_div_mod(a, b, nwords, q, NULL);
}

// val hemlock_intnw_udiv: int64 array -> int64 array -> int64 array
CAMLprim value
hemlock_intnw_udiv(value a_a, value a_b) {
  return binary_op(a_a, a_b, udiv);
}

static void
umod(const hl_word_t *a, const hl_word_t *b, size_t nwords, hl_word_t *r) {
  hl_word_t q[nwords];
  u_div_mod(a, b, nwords, q, r);
}

// val hemlock_intnw_umod: int64 array -> int64 array -> int64 array
CAMLprim value
hemlock_intnw_umod(value a_a, value a_b) {
  return binary_op(a_a, a_b, umod);
}

static bool
is_neg(const hl_word_t *a, size_t nwords) {
  return ((a[nwords-1] & 0x8000000000000000LU) != 0);
}

static void
neg(const hl_word_t *a, size_t nwords, hl_word_t *r) {
  hl_word_t not_a[nwords];
  for (size_t i = 0; i < nwords; i++) {
    not_a[i] = ~a[i];
  }
  hl_word_t one[nwords];
  init_one(nwords, one);
  add(not_a, one, nwords, r);
}

// Returns is_neg(a, nwords).
static bool
is_neg_abs(const hl_word_t *a, size_t nwords, hl_word_t *r) {
  if (is_neg(a, nwords)) {
    neg(a, nwords, r);
    return true;
  } else {
    dup(a, nwords, r);
    return false;
  }
}

static void
i_extend(const hl_word_t *a, size_t nwords_a, hl_word_t *r, size_t nwords_r) {
  assert(nwords_a <= nwords_r);
  dup(a, nwords_a, r);
  hl_word_t ext = is_neg(a, nwords_a) ? 0xffffffffffffffffLU : 0;
  for (size_t i = nwords_a; i < nwords_r; i++) {
    r[i] = ext;
  }
}

static void
i_trunc(const hl_word_t *a, size_t nwords_a, hl_word_t *r, size_t nwords_r) {
  assert(nwords_a >= nwords_r);
  hl_word_t a_abs[nwords_a];
  bool a_is_neg = is_neg_abs(a, nwords_a, a_abs);
  dup(a_abs, nwords_r, r);
  if (a_is_neg) {
    hl_word_t r_abs[nwords_r];
    dup(r, nwords_r, r_abs);
    neg(r_abs, nwords_r, r);
  }
}

static void
i_div_mod(const hl_word_t *a, const hl_word_t *b, size_t nwords, hl_word_t *q,
    hl_word_t *r) {
  hl_word_t a_abs[nwords];
  bool a_is_neg = is_neg_abs(a, nwords, a_abs);
  hl_word_t b_abs[nwords];
  bool b_is_neg = is_neg_abs(b, nwords, b_abs);
  u_div_mod(a_abs, b_abs, nwords, q, r);
  if (a_is_neg != b_is_neg) {
    hl_word_t q_ext[nwords+1];
    i_extend(q, nwords, q_ext, nwords+1);
    hl_word_t q_abs[nwords+1];
    dup(q_ext, nwords+1, q_abs);
    neg(q_abs, nwords, q_ext);
    i_trunc(q_ext, nwords+1, q, nwords);
  }
  if (r != NULL && a_is_neg) {
    hl_word_t r_ext[nwords+1];
    i_extend(r, nwords, r_ext, nwords+1);
    hl_word_t r_abs[nwords+1];
    dup(r_ext, nwords+1, r_abs);
    neg(r_abs, nwords+1, r_ext);
    i_trunc(r_ext, nwords+1, r, nwords);
  }
}

static void
idiv(const hl_word_t *a, const hl_word_t *b, size_t nwords, hl_word_t *q) {
  i_div_mod(a, b, nwords, q, NULL);
}

// val hemlock_intnw_idiv: int64 array -> int64 array -> int64 array
CAMLprim value
hemlock_intnw_idiv(value a_a, value a_b) {
  return binary_op(a_a, a_b, idiv);
}

static void
imod(const hl_word_t *a, const hl_word_t *b, size_t nwords, hl_word_t *r) {
  hl_word_t q[nwords];
  i_div_mod(a, b, nwords, q, r);
}

// val hemlock_intnw_imod: int64 array -> int64 array -> int64 array
CAMLprim value
hemlock_intnw_imod(value a_a, value a_b) {
  return binary_op(a_a, a_b, imod);
}

// val hemlock_intnw_neg: int64 array -> int64 array
CAMLprim value
hemlock_intnw_neg(value a_a) {
  return unary_op(a_a, neg);
}

static void
abs_(const hl_word_t *a, size_t nwords, hl_word_t *r) {
  is_neg_abs(a, nwords, r);
}

// val hemlock_intnw_abs: int64 array -> int64 array
CAMLprim value
hemlock_intnw_abs(value a_a) {
  return unary_op(a_a, abs_);
}

static value
of_float(bool signed_, value a_nwords, value a_f) {
  size_t nwords = Unsigned_long_val(a_nwords);
  size_t nbits = nwords * hl_bpw;
  size_t sig_bits = nwords * hl_bpw - (signed_ ? 1 : 0);
  union {
    double f;
    uint64_t bits;
  } u;
  u.f = Double_val(a_f);
  hl_word_t r[nwords];
  switch (fpclassify(u.f)) {
  case FP_NORMAL: {
    if (!signed_ && signbit(u.f)) {
      init_zero(nwords, r);
      break;
    }
    int biased_exponent = 0x7ff & (u.bits >> 52);
    if (biased_exponent < 1023) {
      init_zero(nwords, r);
      break;
    }
    unsigned exponent = biased_exponent - 1023;
    if (exponent >= sig_bits + 52) {
      init_zero(nwords, r);
      break;
    }

    hl_word_t t_pos[nwords];
    init_u(0x10000000000000LU | (u.bits & 0xfffffffffffffLU), nwords, t_pos);
    hl_word_t t[nwords];
    bool negative = (signed_ && signbit(u.f));
    if (negative) {
      neg(t_pos, nwords, t);
    } else {
      dup(t_pos, nwords, t);
    }
    if (exponent >= 52) {
      if (signed_) {
        hl_word_t u[nwords];
        if (negative) {
          hl_word_t u[nwords];
          init_one(nwords, u);
          hl_word_t sign_neg[nwords];
          bit_sl(nbits-1, u, nwords, sign_neg);
          bit_sl(exponent - 52, t, nwords, u);
          bit_or(sign_neg, u, nwords, r);
        } else {
          bit_sl(exponent - 51, t, nwords, u);
          bit_usr(1, u, nwords, r);
        }
      } else {
        bit_sl(exponent - 52, t, nwords, r);
      }
    } else {
      bit_ssr(52 - exponent, t, nwords, r);
    }
    break;
  }
  case FP_SUBNORMAL:
  case FP_ZERO:
  case FP_INFINITE:
    init_zero(nwords, r);
    break;
  case FP_NAN:
  default: assert(false);
  }
  return oarray_of_uarray(r, nwords);
}

CAMLprim value
hemlock_intnw_i_of_float(value a_nwords, value a_f) {
  return of_float(true, a_nwords, a_f);
}

CAMLprim value
hemlock_intnw_u_of_float(value a_nwords, value a_f) {
  return of_float(false, a_nwords, a_f);
}

static value
float_of_parts(bool negative, unsigned exponent, uint64_t mantissa) {
  assert(mantissa & 0xfffffffffffffLU == mantissa);
  uint64_t sign = negative ? 1 : 0;
  uint64_t biased_exponent = exponent + 1023;
  union {
    double f;
    uint64_t bits;
  } u;
  u.bits = (sign << 63) | (biased_exponent << 52) | mantissa;
  return caml_copy_double(u.f);
}

static bool
is_signed_min_value(const hl_word_t *a, size_t nwords) {
  for (size_t i = 0; i < nwords - 1; i++) {
    if (a[i] != 0) {
      return false;
    }
  }
  return (a[nwords - 1] == 0x8000000000000000LU);
}

static value
to_float(bool signed_, value a_a) {
  size_t nwords = caml_array_length(a_a);
  size_t nbits = nwords * hl_bpw;
  hl_word_t a[nwords];
  uarray_of_oarray(a_a, nwords, a);
  bool negative = (signed_ && is_neg(a, nwords));
  if (negative) {
    if (is_signed_min_value(a, nwords)) {
      return float_of_parts(true, nbits - 1, 0);
    }
    hl_word_t t[nwords];
    dup(a, nwords, t);
    neg(t, nwords, a);
  }
  unsigned sig_bits = nbits - bit_clz(a, nwords);
  if (sig_bits == 0) {
    return caml_copy_double(0.0);
  }
  /* Shift such that the most significant 1 bit is at offset 52 from the least
   * significant bit. The least significant 52 bits become the mantissa (bit 52
   * will be discarded). */
  unsigned exponent = sig_bits - 1;
  hl_word_t t[nwords];
  if (sig_bits < 53) {
    bit_sl(53 - sig_bits, a, nwords, t);
  } else {
    bit_usr(sig_bits - 53, a, nwords, t);
  }
  uint64_t mantissa = t[0] & 0xfffffffffffffLU;
  return float_of_parts(negative, exponent, mantissa);
}

CAMLprim value
hemlock_intnw_i_to_float(value a_a) {
  return to_float(true, a_a);
}

CAMLprim value
hemlock_intnw_u_to_float(value a_a) {
  return to_float(false, a_a);
}
