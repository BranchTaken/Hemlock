# Types

Hemlock provides a foundational set of types that the compiler has inherent
knowledge of.  All types are built up from this foundation, including various
types provided by the `Basis` library, e.g. `option` and `map`.

The [atomic types](#atomic-types) have dedicated syntax, as well as
corresponding modules in the `Basis` library:

- [Unit](#unit)
- [Boolean](#boolean)
- [Integer](#integer)
- [Float](#float)
- [Codepoint](#codepoint)
- [String](#string)

The [collection types](#collection-types) also have dedicated syntax and
corresponding `Basis` modules:

- [List](#list)
- [Array](#array)

The [composite types](#compsite-types) have dedicated syntax, but no
corresponding `Basis` modules:

- [Tuple](#tuple)
- [Variant](#variant)
- [Record](#record)
- [Function](#function)
- [Module](#module)
- [Functor](#functor)

## Atomic types

The atomic types are monomorphic, immutable, and are the only types which may
have unboxed native hardware formats.  Many features of these types have
explicit syntax, and further functionality is provided by their respective
companion modules in the `Basis` library.

### Unit

The `unit` type has only one possible value, which is written as `()`.
Because there is only one possible `unit` value, it rarely needs a concrete
data representation; rather it logically acts as a placeholder where data would
otherwise go.  For example, a function which has no inputs cannot be called, but
a function with a single `unit` parameter dodges this problem.  `unit` can
also be used to elide record fields; e.g. `('a, 'cmp) set` is implemented as
`('a, unit, 'cmp) map`, and no space is consumed by values in the concrete data
representation.

### Boolean

The `bool` type is either `false` or `true`.

### Integer

Integers are either unsigned or signed (2's complement), and their range
is determined by their bit widths.  In all cases, integer operations which
overflow/underflow silently wrap; there are no undefined behaviors.  Unsigned
and signed integers behave identically at the machine level with the exception
that different instructions are used for comparison, depending on type
signedness.  In many programming languages, sign extension for bit shifting
right also depends on signedness, but Hemlock explicitly provides both right
shifting options regardless of signedness (`bit_usr`/`bit_ssr`), and the
application must choose which to use.

The following integer types are provided:

- Unsigned
  - `u8`/`byte`
  - `u16`
  - `u32`
  - `u64`/`uns`
  - `u128`
  - `u256`
  - `u512`
- Signed
  - `i8`
  - `i16`
  - `i32`
  - `i64`/`int`
  - `i128`
  - `i256`
  - `i512`

`uns` is the default integer type, and should be preferred over other integer
types unless the use case demands a specific signedness or bit width.  Hemlock
fundamentally depends on at least a 64-bit architecture, so `uns`/`int` always
provide at least a 64-bit range.

Integer literals start with an optional sign if signed, followed by the
magnitude, and finally a type suffix (optional for `uns`).  Non-zero
signed integers have an optional `+`/`-` sign prefix; unsigned integers
never have an explicit sign.  Magnitudes can be specified in base 10, 16
(`0x` prefix), or 2 (`0b` prefix).  The type suffix matches the pattern,
`[ui](8|16|32|64|128|256|512)?`.  `_` separators can be arbitrarily internally
interspersed, so long as they come after the base prefix (if present) and before
the type suffix (if present).  Out-of-range literals cause compile-time failure.

`uns` literal examples:

    0
    42
    15u
    0x0123_4567_89ab_cdef
    0b10_0001

`int` literal examples:

    0i
    +42i
    -14i
    -0x_ab__c_i

`byte` literal examples:

    0u8
    0xffu8

### Float

Floating point numbers use the [IEEE
754](https://en.wikipedia.org/wiki/IEEE_754) binary floating point formats and
ubiquitous hardware support for [32-bit
`p32`](https://en.wikipedia.org/wiki/Single-precision_floating-point_format)
and [64-bit
`p64`/`float`](https://en.wikipedia.org/wiki/Double-precision_floating-point_format).
Although [16,128,256]-bit formats are also well defined, they are omitted from
Hemlock because they are not commonly supported in hardware, making them of
limited practical use.

Floating point literals are always syntactically distinct from integers,
due to a decimal point or type suffix if nothing else.  The default `float`
bitwidth is 64; a `p64` type suffix allows the bitwidth to be explicit, and a
`p32` suffix allows specification of 32-bit `p32` constants.  Values can be
specified in decimal or hexadecimal; the latter allows bit-precise specification
of constants, whereas many decimal floating point numbers have no exact binary
representation.

    0.
    1.0
    0p64
    1.5p32
    4.2
    +4.2
    42.
    42.^44
    1^44
    3.2^42
    1_000_000.0
    16^+_42
    16.000_342^-42
    -inf
    inf
    +inf
    nan
    -0.0
    0.0
    +0.0
    4^3
    42.3^-78_p64
    42.3^-78
    42.3^+78
    42.3^78
    -0x1.42
    -0x0.0
    +0x0.0
    0x0.0
    0x4.a3
    +0x4.a3
    0x4a^42
    0x4a^-42
    0x4a^+42
    0x4a.3d2^+42
    0x4a.3d2^+42_p32
    -0x1.ffffffc
    0x1.ffff_ffc
    0x1.ffffffc^-1022
    0x1.ffffffc^1023
    0x1.ffffffc^+1023

### Codepoint

The `codepoint` type encodes a single 21-bit
[Unicode](https://en.wikipedia.org/wiki/Unicode) code point.  Internally,
`codepoint` is stored as a `u32`:

    type codepoint = private u32

However, `codepoint` is intentionally type-incompatible with `u32`, thus
requiring explicit validating conversion.

Hemlock's source code encoding is always
[UTF-8](https://en.wikipedia.org/wiki/UTF-8), so the simplest way to specify a
`codepoint` literal is as UTF-8 inside `'` delimiters, e.g.:

    '<'
    '«'
    '‡'
    '𐆗'

Alternately, codepoints can be specified in hexadecimal:

    '\u{3c}' # '<'
    '\u{ab}' # '«'
    '\u{2021}' # '‡'
    '\u{10197}' # '𐆗'

Various special characters can be specified via `\` escapes:

    '\t' # Tab
    '\n' # Newline
    '\r' # Carriage return
    '\'' # Single quote
    '\\' # Backslash

### String

The `string` type contains a UTF-8-encoded sequence of `codepoint` values.
It is impossible to construct a string with invalid UTF-8 encoding, whether via
string literals or programmatically at run time.  Similarly to `codepoint`
literals, the simplest way to specify a `string` literal is as UTF-8, inside `"`
delimiters, e.g.

    "Hello"
    "A non-ASCII string -- <«‡𐆗"

Codepoints can also be specified in delimited hexadecimal, e.g.:

    "A non-ASCII string -- \u{3c}\u{ab}\u{2021}\u{10197}"

Various special characters can be specified via `\` escapes:

    "\t" # Tab
    "\n" # Newline
    "\r" # Carriage return
    "\"" # Double quote
    "\\" # Backslash

## Collection types

The collection types are polymorphic, and therefore potentially transitively
mutable.  The collection types have rich APIs provided by their respective
companion modules in the `Basis` library.

### List

The `'a list` type provides persistent singly-linked lists, which are ubiquitous
in typical code.  Every element is independently allocated, so traversal
requires pointer chasing, but memory locality tends to be good due to
allocation/copying order.

    [] # 'a list
    [0] # 0 :: [] # uns list
    [0; 1] # 0 :: 1 :: [] # uns list

### Array

The `'a /r array` type provides unresizable contiguous arrays, which may
optionally be directly mutable.  Depending on the type supplied as `'a`,
elements may be boxed (e.g. `string array`) or unboxed (e.g. `codepoint array`).
Unboxed arrays are particularly compelling from a density and performance
perspective.

    [||] # 'a /r array
    [|0; 1|] # uns /r array
    [|"boxed"; "element"; "references"|] # string /r array
    [|'u'; 'n'; 'b'; 'o'; 'x'; 'e'; 'd'|] # codepoint /r array

## Composite types

The composite types are polymorphic, and therefore potentially transitively
mutable.  All common functionality for these types has explicit syntax, so the
composite types do *not* have companion modules in the `Basis` library.

### Tuple

A tuple comprises two or more independently typed values.  Tuples are directly
immutable, but may refer to transitively mutable values.

    (1, "b") # uns * string
    (None, [|'a'; 'b'|], 42) # 'a option * codepoint /r array * uns

### Variant

A variant is a discriminated union that can contain exactly one variant, as
indicated by its discriminator.

    type color =
      | Red
      | Green
      | Blue
      | RGBA of u8 * u8 * u8 * u8

Variants are directly immutable (the discriminator cannot be mutated), but may
refer to transitively mutable values.

    # Transitive non-parametric mutability.
    type &odd =
      | Odd of uns &array
      | Odder of unit array

    # Transitive parametric mutability (mutable if 'a is mutable).
    type 'a option =
      | None
      | Some of * 'a

### Record

A record maps field names to independently typed values.  Each field may be of
fixed or parametric type; all field type parameters are transitively exposed as
record type parameters.

    # Non-parametric mutability.
    type &r1 = {
      &x: uns; # Direct mutability.
      a: codepoint &array; # Indirect mutability.
    }

    # Transitive parametric mutability.
    type 'a r2 = {
      a: 'a array; # Indirect mutability if 'a is transitively mutable.
    }

    # Parametric effect.
    type ('a, >e) r3 {
      f: uns >e-> 'a array
    }

    # All of the above.
    type ('a, >e) &r4 {
      &x: uns;
      a: codepoint &array;
      b: 'a array;
      f: uns >e-> 'a array
    }

Record typess are non-recursive by default, but the `rec` keyword allows
self-referential record typing, e.g.:

    type rec 'a node = {
      sibling: 'a node;
   }

    let a = {sibling=b} and b = {sibling=a} in
    # ...

Mutually recursive records must be specified as `rec` *and* co-declared, e.g.:

    type rec blue = {
      black: black option;
    }
    and black = {
      blue: blue option;
    }

    let a = {black=Some b} and b = {blue=Some a} in
    # ...

### Function

A function takes one or more parameters as input, causes zero or more side
effects (which must be explicit in the function type), and produces a value.
Functions are conceptually curried, i.e. they can be reasoned about as a
series of partial applications, each of which consumes an input and produces
a continuation, and the final application produces the resulting value (and
optionally causes side effects).  In practice, function invocation is monolithic
in the number of parameters provided, so partial application only comes into
play when the program omits parameters from a call.

    # val sq: uns -> uns
    let xx x =
      x * x

    # Transitive parametric effect, depending on the implementation of ~f .
    val init: uns -> f:(uns >-> 'a) >-> 'a /t t

    # Mutation effect on mutable parameter.
    val set_inplace: uns -> 'a -> 'a !&t -> unit

    # Internal environment effect.
    val abort: unit !-> 'a

### Module

XXX

### Functor

XXX
