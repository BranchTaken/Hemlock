# Types

Hemlock provides a foundational set of types that the compiler has inherent knowledge of. All types
are built up from this foundation, including various types provided by the `Basis` library, e.g.
`option` and `map`.

The [atomic types](#atomic-types) have dedicated syntax, as well as corresponding modules in the
`Basis` library:

- [Unit](#unit)
- [Boolean](#boolean)
- [Integer](#integer)
- [Real](#real)
- [Codepoint](#codepoint)
- [String](#string)

The [collection types](#collection-types) also have dedicated syntax and corresponding `Basis`
modules:

- [List](#list)
- [Array](#array)

The [composite types](#compsite-types) have dedicated syntax, but no corresponding `Basis` modules:

- [Tuple](#tuple)
- [Variant](#variant)
- [Record](#record)
- [Function](#function)
- [Module](#module)

## Parametric types

Every type has a primary component which is always a type, aka primary type. Parametric types may
additionally have subsidiary type/mutability/effect components. For example, `uns` has only a
primary type, whereas `type t 'a 'b ^m >e: t a b m e` has `t` as its primary type, `'a` and `'b` as
subsidiary types, `^m` as a subsidiary mutability, and `>e` as a subsidiary effect.

Type constructors can be reasoned about as functions, for which parametric types have inferred
parameters. The following type declarations are equivalent, though the first form is ergonomically
preferrable.

```hemlock
type t 'a 'b ^m >e: t a b m e
type t: 'a -> 'b -> ^m -> >e -> t a b m e
type t: '(a: type) -> '(b: type) -> ^(m: mutability) -> >(e: effect) -> t a b m e
```

Every type has associated mutability, of which there are three cases:

- **Immutable:** The type is [non-parametrically] immutable, e.g. `type uns: uns`.
- **Mutable:** The type is [non-parametrically] mutable, e.g. the array in `type mut_array 'a:
  &mut_array a = &array a` and the arrays in `val ba: box &mut_array uns`. (There are no
  non-parametrically mutable types in `Basis`, so these examples use a mutable array type based on
  mutability-constrained arrays.)
- **Parametrically mutable:** The type's mutability is a parameter of the type, e.g. the array in
  `val v ^array: ^&array uns`, the boxes in `val v ^box: array ^&box uns`, and the record `r` in
  `val v ^r: ^&r`.

All of the above examples focus on concrete types, but subsidiary mutability can come into play
for abstract types. For example, `val v 'a: array a` denotes an immutable array, but the subsidiary
`'a` elements are parametrically mutable.

## Atomic types

The atomic types are monomorphic, intransitive, immutable, and are the only types which may have
immediate native hardware formats. Many features of these types have explicit syntax, and further
functionality is provided by their respective companion modules in the `Basis` library.

### Unit

The `unit` type has only one possible value, which is written as `()`. Because there is only one
possible `unit` value, it rarely needs a concrete data representation; rather it logically acts as a
placeholder where data would otherwise go. For example, a function which has no inputs cannot be
called, but a function with a single `unit` parameter dodges this problem. `unit` can also be used
to elide record fields; e.g. `set a cmp` is implemented as `map a unit cmp`, and no space is
consumed by values in the concrete data representation.

### Boolean

The `bool` type is either `false` or `true`.

### Integer

Integers are either unsigned or signed (2's complement), and their range is determined by their bit
widths. In all cases, integer operations which overflow/underflow silently wrap; there are no
undefined behaviors. Unsigned and signed integers behave identically at the machine level with the
exception that different instructions are used for comparison, depending on type signedness. In many
programming languages, sign extension for bit shifting right also depends on signedness, but Hemlock
explicitly provides both right shifting options regardless of signedness (`bit_usr`/`bit_ssr`), and
the application must choose which to use.

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

`uns` is the default integer type, and should be preferred over other integer types unless the use
case demands a specific signedness or bit width. Hemlock fundamentally depends on at least a 64-bit
architecture, so `uns`/`int` always provide at least a 64-bit range. See [integer literal
syntax](syntax.md#integer) for further details.

### Real

Real numbers use the [IEEE 754](https://en.wikipedia.org/wiki/IEEE_754) binary floating point
formats and ubiquitous hardware support for [32-bit
`r32`](https://en.wikipedia.org/wiki/Single-precision_floating-point_format) and [64-bit
`r64`/`real`](https://en.wikipedia.org/wiki/Double-precision_floating-point_format). Although
[16,128,256]-bit formats are also well defined, they are omitted from Hemlock because they are not
commonly supported in hardware, making them of limited practical use.

Real number literals are always syntactically distinct from integers, due to a decimal point or type
suffix if nothing else. See [real literal syntax](syntax.md#real) for further details.

### Codepoint

The `codepoint` type encodes a single 21-bit [Unicode](https://en.wikipedia.org/wiki/Unicode) code
point. Internally, `codepoint` is stored as a `u32`:

```hemlock
type codepoint: codepoint = conceal u32
```

However, `codepoint` is intentionally type-incompatible with `u32`, thus requiring explicit
validating conversion. See [codepoint literal syntax](syntax.md#codepoint) for further details.

### String

The `string` type contains a UTF-8-encoded sequence of `codepoint` values. It is impossible to
construct a string with invalid UTF-8 encoding, whether via string literals or programmatically at
run time. See [string literal syntax](syntax.md#string) for further details.

## Collection types

The collection types are polymorphic, and therefore potentially transitively mutable. The collection
types have rich APIs provided by their respective companion modules in the `Basis` library.

### List

The `'a list` type provides persistent singly-linked lists, which are ubiquitous in typical code.
Every element is independently allocated, so traversal requires pointer chasing, but memory locality
tends to be good due to allocation/copying order.

```hemlock
[] # 'a list
[0] # 0 :: [] # uns list
[0; 1] # 0 :: 1 :: [] # uns list
```

### Array

The `type array 'a ^array: ^&array a` type provides unresizable contiguous arrays, which may
optionally be primarily mutable. Depending on the type supplied as `a`, elements may be indirect
(e.g. `_&array string`) or immediate (e.g. `_&array codepoint`). Immediate arrays are particularly
compelling from a density and performance perspective.

```hemlock
[||] # _&array a
[|0; 1|] # _&array uns
[|"element"; "references"|] # _&array string
[|'u'; 'n'; 'b'; 'o'; 'x'; 'e'; 'd'|] # _&array codepoint
```

## Composite types

The composite types are polymorphic, and therefore potentially transitively mutable. All common
functionality for these types has explicit syntax, so the composite types do *not* have companion
modules in the `Basis` library.

### Tuple

A tuple comprises two or more independently typed values. Tuple elements cannot be re-bound, but
elements may transitively refer to mutable values.

```hemlock
type tup: tup = (uns, string)
type tup2: tup2 = bool, uns, list codepoint # Parens not required.
type tup3: &tup3 = (&box uns, codepoint) # Non-parametric transitive primary mutability.
type tup4 'a ^array: tup4 a array = (option a, ^&array codepoint) # Parametric subsidiary
                                                                  # mutability.

(1, "b") # (uns, string)
(None, [|'a'; 'b'|]) # (option a, _&array codepoint)
```

### Variant

A variant is a discriminated union that can contain exactly one variant, as indicated by its
discriminator.

```hemlock
type color: color =
    | Red
    | Green
    | Blue
    | RGBA of (u8, u8, u8, u8)
```

The variant discriminator is immutable, but a variant may nonetheless be primarily mutable due to
transitive non-parametric mutability of variants.

```hemlock
# Non-parametric transitive primary mutability.
type odd: &odd =
    | Odd of &array uns
    | Odder of array unit

# Parametric subsidiary mutability (mutable if 'a is mutable).
type option 'a: option a =
    | None
    | Some of a
```

Recursive variant types must be explicitly specified as `rec`.

```hemlock
type rec node 'a: node a =
    | Leaf of a
    | Node of (a, node a)

let leaf = Leaf 2
let child = Node(1, leaf)
let root = Node(0, child)
```

### Record

A record maps field names to independently typed values. Each field may be of fixed or parametric
type; all field type parameters are transitively exposed as record type parameters.

```hemlock
# Non-parametric primary mutability.
type r1: &r1 =
    &x: uns # Mutable immediate value.
    a: &array codepoint # Mutable indirect value.

# Parametric subsidiary mutability.
type r2 'a: r2 a =
    a: array a

# Parametric effect.
type r3 'a >e: r3 a e =
    f: uns >e-> array a

# All of the above.
type r4 'a >e: &r4 a e =
    &x: uns
    a: &array codepoint
    b: array a
    f: uns >e-> array a

# Parametric primary mutability.
type r5 'a ^r5: ^&r5 a =
    ^r5&x: uns
    l: list a

{x=42; l=["hi"]} # ^&r5 string
{x=43; l=['c']} # ^&r5 codepoint
```

Record types are non-recursive by default, but the `rec` keyword allows self-referential record
typing, e.g.:

```hemlock
type rec node 'a: node a =
    child: option node a

let leaf = {child=None}
let root = {child=Some leaf}
```

Mutually recursive records must be specified as `rec` *and* use mutation to tie the knot, e.g.:

```hemlock
type rec blue: &blue =
    &black: option black
also black: &black =
    blue: &blue

let a = {black=None}
let b = {blue=a}
a.black := Some b
```

### Function

A function takes one or more parameters as input, causes zero or more effects (which must be
explicit in the function type), and produces a value. Functions are conceptually curried, i.e. they
can be reasoned about as a series of partial applications. Each partial application closes on one
input. Additionally, the final application optionally causes effects as it computes the resulting
value. In practice, function invocation is monolithic in the number of parameters provided, so
partial application only comes into play when the program omits parameters from a call.

```hemlock
# val sq: uns -> uns
let sq x =
    x * x

# Transitive parametric effect, depending on the implementation of ~f .
val init 'a ^t >e: uns -> f:(uns >e-> a) >e-> ^&t a

# Mutation effect on mutable parameter.
val set_inplace 'a: uns -> a -> !&t a -> unit

# Halt effect.
val abort 'a: string >hlt-> a
```

### Module

A module is a value which bundles together a set of types and values. Due to type erasure, modules
are little different from records at run time, but the two types provide very different language
capabilities that are best kept conceptually distinct. Module types can be specified from scratch,
or be based on another module type. Constraints and/or extensions are supported when basing one
module type on another.

```hemlock
# Empty module.
type E = ;;

type M: M =
    type t: t

    val f: uns -> t

# M' extends M.
type M': M' =
    include M
    type u: u

    val g: t -> u

type SMono: SMono =
    type t ^t: ^&t
    type elm ^elm: ^&elm
    val to_array ^t ^elm ^outer: ^&t -> ^&elm ^&outer

# The return type is of type SMono, but with constraints relative to input T.
val makeMono: (T : SeqIntf.IMonoDef) : SMono
  with type t ^t: ^&t := T.^&t
  with type elm ^elm: ^&elm := T.^&elm

# makeMono produces a result of type roughly equivalent to type Mono, but it is common to use an
# anonymous type as above in order to specify constraints in terms of function input types.
type Mono = SMono
  with type t ^t: ^&t := SeqIntf.IMonoDef.^&t
  with type elm ^elm: ^&elm := SeqIntf.IMonoDef.^&elm
  |}
```

Module type and value names (lexical bindings) are always capitalized, e.g. `A._B.MType` and
`A._B.M`, but record field tags remain uncapitalized even when they are associated with modules,
e.g. `A.t._u.m`. There are no critical technical reasons to require capitalization of module types
and names in Hemlock, but the distinct naming aids readability to a degree that justifies the strict
naming requirement.

```hemlock
let A = {|
    let _B = {|
        type MType: MType =
            ...
        # M, U, and I are in the same namespace; beware module vs variant constructor name
        # collisions.
        let M = {|
            ...
          |}
        type v =
            | U of uns
            | I of int
      |}

    type t =
        _u:
            m: A._B.MType
  |}
```

Following is a complete example of a module which implements persistent binary trees.

```hemlock
let Tree = {|
    type rec t 'a: t a =
        | Empty
        | Node of
            lchild: t a
            value: a
            rchild: t a

    let empty = Empty

    let is_empty = function
        | Empty -> true
        | Node _ -> false

    let node lchild value rchild =
        {lchild; value; rchild}

    let leaf value =
        node empty value empty

    let lchild = function
        | Empty -> empty
        | Node {lchild; value=_; rchild=_} -> lchild

    let root_value_opt = function
        | Empty -> None
        | Node {lchild=_; value; rchild=_} -> Some value

    let root_value_hlt t =
        match value_opt t with
        | None -> halt "Empty tree"
        | Some value -> value

    let root_value = root_value_hlt

    let rchild = function
        | Empty -> empty
        | Node {lchild=_; value=_; rchild} -> rchild
  |}
```

`Tree` has the following type:

```hemlock
val Tree :
    type rec t a: t a =
        | Empty
        | Node of
            lchild: t a
            value: a
            rchild: t a
    val empty 'a: t a
    val is_empty: t _ -> bool
    val node 'a: t a -> a -> t a -> t a
    val leaf 'a: a -> t a
    val lchild 'a: t a -> t a
    val root_value_opt a: t a -> option a
    val root_value_hlt 'a: t a -> a
    val root_value 'a: t a -> a
    val rchild 'a: t a -> t a

type TreeSig = type of Tree
```

Hemlock infers the type of a module expression `{|...|}` to divulge all type and value bindings.
Often there are portions of the module type which the programmer wants to omit. The simplest way to
accomplish this is to ascribe an explicit type which makes the omitted portion of the original type
opaque. The following example ascribes the module to a type with abstract `t a` and no
`root_value_opt` nor `root_value_hlt` functions.

```hemlock
let TreeConstrained :
    type t 'a: t a
    val empty 'a: t a
    val is_empty: '_ t -> bool
    val node 'a: t a -> a -> t a -> t a
    val leaf 'a: a -> t a
    val lchild 'a: t a -> t a
    val root_value 'a: t a -> a
    val rchild 'a: t a -> t a
  = Tree

type TreeConstrainedSig = type of TreeConstrained
```

Type ascription by itself cannot augment the resulting module type; for that a function can be
combined with type ascription. The following function creates a module equivalent to
`TreeConstrained` above.

```hemlock
# val makeTreeConstrained: (T: TreeSig) -> TreeConstrainedSig
let makeTreeConstrained (T: TreeSig)
  : TreeConstrainedSig with type t 'a: t a := T.t a
  = {|
    type t 'a: t a := T.t a
    let empty = T.empty
    let is_empty = T.empty
    let node = T.node
    let leaf = T.leaf
    let lchild = T.child
    let root_value = T.root_value
    let rchild = T.rchild
  |}

let TreeConstrained = makeTreeConstrained Tree
```

#### Top-level modules

Top-level modules require extra explanation with regard to how they are created. As an example,
suppose that `Tree` is implemented as a top-level module in `Tree.hm`. The file contents (denoted as
`<Tree.hm>`) are treated as if wrapped.

```hemlock
# val makeTree: unit -> type of Tree
let makeTree () = {|
    <Tree.hm>
  |}
```

If there is also a corresponding interface file, `Tree.hmi`, then the file contents (denoted as
`<Tree.hmi>`) are treated as an ascribed type for the module defined by `Tree.hm`.

```hemlock
# val makeTree: unit -> type of Tree
let makeTree () :
    <Tree.hmi>
  = {|
    <Tree.hm>
  |}
```

Note that an `import` expression triggers function application to create the module, and the module
body can of course cause effects when executed. Such effects propagate to all corresponding `import`
expressions, even though the module is created only once.

```hemlock
# val makeM: unit >os-> type of M
let makeM () :
    <M.hmi>
  = {|
    <M.hm>
  |}

# val importM: unit >os-> type of M
let importM () =
    (lazy (makeM ())) |> Lazy.force
```


#### `open`, `include`, and `import`

Outside `Tree`, the visible contents can be referred to via dot notation, e.g. `Tree.value`. It is
also possible to `open` a module's lexical namespace, i.e. shadow the current lexical scope for the
purposes of lookup without creating any new lexical bindings.

```hemlock
open Tree
let tree = node (leaf 0) 1 empty
```

Alternatively, the scope for which the module is opened can be constrained to an expression.

```hemlock
let tree = Tree.(node (leaf 0) 1 empty)
```

The `open` keyword merely shadows the current lexical scope for the purpose of supporting identifier
lookups, whereas the `include` keyword merges bindings into the current lexical scope. The most
common use case for `include` is to create a new module which incorporates part or all of an
`include`d module.

The `import` keyword returns a top-level module. As described earlier, top-level module creation may
cause effects, so `import` can incur effects. Therefore it is generally advisable to `import`
external modules at at the top level of a module, and use `open`/`include` thereafter, in order to
limit transitive effect impacts to the top-level module, thus sparing submodules and functions from
API brittleness.

```hemlock
open import Basis
let Tree = import Tree

let AugmentedTree = {|
    include Tree
    ...
  |}

let triple a b c =
    let open Tree
    node (leaf a) b (leaf c)
```
