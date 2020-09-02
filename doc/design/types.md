# Types

Hemlock provides a foundational set of types that the compiler has inherent
knowledge of. All types are built up from this foundation, including various
types provided by the `Basis` library, e.g. `option` and `map`.

The [atomic types](#atomic-types) have dedicated syntax, as well as
corresponding modules in the `Basis` library:

- [Unit](#unit)
- [Boolean](#boolean)
- [Integer](#integer)
- [Real](#real)
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
have unboxed native hardware formats. Many features of these types have explicit
syntax, and further functionality is provided by their respective companion
modules in the `Basis` library.

### Unit

The `unit` type has only one possible value, which is written as `()`. Because
there is only one possible `unit` value, it rarely needs a concrete data
representation; rather it logically acts as a placeholder where data would
otherwise go. For example, a function which has no inputs cannot be called, but
a function with a single `unit` parameter dodges this problem. `unit` can also
be used to elide record fields; e.g. `('a, 'cmp) set` is implemented as `('a,
unit, 'cmp) map`, and no space is consumed by values in the concrete data
representation.

### Boolean

The `bool` type is either `false` or `true`.

### Integer

Integers are either unsigned or signed (2's complement), and their range is
determined by their bit widths. In all cases, integer operations which
overflow/underflow silently wrap; there are no undefined behaviors. Unsigned and
signed integers behave identically at the machine level with the exception that
different instructions are used for comparison, depending on type signedness. In
many programming languages, sign extension for bit shifting right also depends
on signedness, but Hemlock explicitly provides both right shifting options
regardless of signedness (`bit_usr`/`bit_ssr`), and the application must choose
which to use.

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
types unless the use case demands a specific signedness or bit width. Hemlock
fundamentally depends on at least a 64-bit architecture, so `uns`/`int` always
provide at least a 64-bit range. See [integer literal syntax](syntax.md#integer)
for further details.

### Real

Real numbers use the [IEEE
754](https://en.wikipedia.org/wiki/IEEE_754) binary floating point formats and
ubiquitous hardware support for [32-bit
`r32`](https://en.wikipedia.org/wiki/Single-precision_floating-point_format) and
[64-bit
`r64`/`real`](https://en.wikipedia.org/wiki/Double-precision_floating-point_format).
Although [16,128,256]-bit formats are also well defined, they are omitted from
Hemlock because they are not commonly supported in hardware, making them of
limited practical use.

Real number literals are always syntactically distinct from integers, due to a
decimal point or type suffix if nothing else. See [real literal
syntax](syntax.md#real) for further details.

### Codepoint

The `codepoint` type encodes a single 21-bit
[Unicode](https://en.wikipedia.org/wiki/Unicode) code point. Internally,
`codepoint` is stored as a `u32`:

```hemlock
type codepoint = conceal u32
```

However, `codepoint` is intentionally type-incompatible with `u32`, thus
requiring explicit validating conversion. See [codepoint literal
syntax](syntax.md#codepoint) for further details.

### String

The `string` type contains a UTF-8-encoded sequence of `codepoint` values. It is
impossible to construct a string with invalid UTF-8 encoding, whether via string
literals or programmatically at run time. See [string literal
syntax](syntax.md#string) for further details.

## Collection types

The collection types are polymorphic, and therefore potentially transitively
mutable. The collection types have rich APIs provided by their respective
companion modules in the `Basis` library.

### List

The `'a list` type provides persistent singly-linked lists, which are ubiquitous
in typical code. Every element is independently allocated, so traversal requires
pointer chasing, but memory locality tends to be good due to allocation/copying
order.

```hemlock
[] # 'a list
[0] # 0 :: [] # uns list
[0; 1] # 0 :: 1 :: [] # uns list
```

### Array

The `'a array^` type provides unresizable contiguous arrays, which may
optionally be directly mutable. Depending on the type supplied as `'a`, elements
may be boxed (e.g. `string array^_`) or unboxed (e.g. `codepoint array^_`).
Unboxed arrays are particularly compelling from a density and performance
perspective.

```hemlock
[||] # 'a array^
[|0; 1|] # uns array^
[|0; 1|]& # uns array&
[|"boxed"; "element"; "references"|] # string array^
[|'u'; 'n'; 'b'; 'o'; 'x'; 'e'; 'd'|] # codepoint array^
```

## Composite types

The composite types are polymorphic, and therefore potentially transitively
mutable. All common functionality for these types has explicit syntax, so the
composite types do *not* have companion modules in the `Basis` library.

### Tuple

A tuple comprises two or more independently typed values. Tuples are directly
immutable, but may refer to transitively mutable values.

```hemlock
(1, "b") # uns * string
(None, [|'a'; 'b'|], 42) # 'a option * codepoint array^ * uns
```

### Variant

A variant is a discriminated union that can contain exactly one variant, as
indicated by its discriminator.

```hemlock
type color =
    | Red
    | Green
    | Blue
    | RGBA of u8 * u8 * u8 * u8
```

Variants are directly immutable (the discriminator cannot be mutated), but may
refer to transitively mutable values.

```hemlock
# Transitive non-parametric mutability.
type odd& =
    | Odd of uns array&
    | Odder of unit array

# Transitive parametric mutability (mutable if 'a is mutable).
type 'a option =
    | None
    | Some of * 'a
```

Recursive variant types must be explicitly specified as `rec`.

```hemlock
type rec 'a node =
    | Leaf of 'a
    | Node of 'a * 'a node

let leaf = Leaf 2
let child = Node(1, leaf)
let root = Node(0, child)
```

### Record

A record maps field names to independently typed values. Each field may be of
fixed or parametric type; all field type parameters are transitively exposed as
record type parameters.

```hemlock
# Non-parametric mutability.
type r1& =
    x&: uns # Direct mutability.
    a: codepoint array& # Indirect mutability.

# Transitive parametric mutability.
type 'a r2 =
    a: 'a array # Indirect mutability if 'a is transitively mutable.

# Parametric effect.
type ('a, >e) r3
    f: uns >e-> 'a array

# All of the above.
type ('a, >e) r4&
    x&: uns
    a: codepoint array&
    b: 'a array
    f: uns >e-> 'a array
```

Record types are non-recursive by default, but the `rec` keyword allows
self-referential record typing, e.g.:

```hemlock
type rec 'a node =
    child: 'a node option

let leaf = {child=None}
let root = {child=Some leaf}
```

Mutually recursive records must be specified as `rec` *and* use mutation to tie
the knot, e.g.:

```hemlock
type rec blue& =
    black&: black option
also black& =
    blue&: blue option

let a& := {black=None}
let b = {blue=Some a}
a.black := Some b
```

### Function

A function takes one or more parameters as input, causes zero or more side
effects (which must be explicit in the function type), and produces a value.
Functions are conceptually curried, i.e. they can be reasoned about as a series
of partial applications, each of which consumes an input and produces a
continuation, and the final application produces the resulting value (and
optionally causes side effects). In practice, function invocation is monolithic
in the number of parameters provided, so partial application only comes into
play when the program omits parameters from a call.

```hemlock
# val sq: uns -> uns
let xx x =
    x * x

# Transitive parametric effect, depending on the implementation of ~f .
val init: uns -> f:(uns >-> 'a) >-> 'a t^

# Mutation effect on mutable parameter.
val set_inplace: uns -> 'a -> 'a t& >[mut] -> unit

# Internal environment effect.
val abort: unit >{mut}-> 'a
```

### Module

XXX

### Functor

XXX
