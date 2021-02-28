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
    | Some of 'a
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
type r1& = {
    x&: uns # Direct mutability.
    a: codepoint array& # Indirect mutability.
  }&

# Transitive parametric mutability.
type 'a r2 = {
    a: 'a array # Indirect mutability if 'a is transitively mutable.
  }

# Parametric effect.
type ('a, >e) r3 = {
    f: uns >e-> 'a array
  }

# All of the above.
type ('a, >e) r4& = {
    x&: uns
    a: codepoint array&
    b: 'a array
    f: uns >e-> 'a array
  }

# Parametric direct mutability.
type 'a r5^x = {
    x^: uns
    l: 'a list
  }

{x=42, l=["hi"]} # string r5^
{x=43; l=['c']}& # codepoint r5&
```

Record types are non-recursive by default, but the `rec` keyword allows
self-referential record typing, e.g.:

```hemlock
type rec 'a node = {
    child: 'a node option
  }

let leaf = {child=None}
let root = {child=Some leaf}
```

Mutually recursive records must be specified as `rec` *and* use mutation to tie
the knot, e.g.:

```hemlock
type rec blue& = {
    black&: black option
  }
also black& = {
    blue&: blue option
  }

let a& := {black=None}
let b = {blue=Some a}
a.black := Some b
```

### Function

A function takes one or more parameters as input, causes zero or more effects
(which must be explicit in the function type), and produces a value. Functions
are conceptually curried, i.e. they can be reasoned about as a series of partial
applications. Each partial application consumes one input and produces a
continuation. Additionally, the final application optionally causes effects. In
practice, function invocation is monolithic in the number of parameters
provided, so partial application only comes into play when the program omits
parameters from a call.

```hemlock
# val sq: uns -> uns
let sq x =
    x * x

# Transitive parametric effect, depending on the implementation of ~f .
val init: uns -> f:(uns >-> 'a) >-> 'a t^

# Mutation effect on mutable parameter.
val set_inplace: uns -> 'a -> 'a t& >{mut}~> -> unit

# Halt effect.
val abort: string >{hlt}-> 'a
```

### Module

A module is a value which bundles together a set of types and values. Due to
type erasure, modules are little different from records at run time, but the two
types provide very different language capabilities that are best kept
conceptually distinct. Module types can be specified from scratch, or be based
on another module type. Constraints and/or extensions are supported when basing
one module type on another.

```hemlock
# Empty module, not very useful in practice.
type E = {||}

type M = {|
    type t

    val f: uns -> t
  |}

# M' extends M.
type M' = {|M with
    type u

    val g: t -> u
  |}

type SMono = {|
    type t^
    type elm^
    val to_array: t^ -> elm^ outer^
  |}

# The return type is of type SMono, but with constraints relative to input T.
val make_Mono: (T : SeqIntf.IMonoDef) -> {|SMono with
    type t^ := T.t^
    type elm^ := T.elm^
  |}

# make_Mono produces a result of type roughly equivalent to type Mono, but it is
# common to use an anonymous type as above in order to specify constraints in
# terms of function input types.
type Mono = {|SMono with
    type t^ := SeqIntf.IMonoDef.t^
    type elm^ := SeqIntf.IMonoDef.elm^
  |}
```

Module type and value names (lexical bindings) are always capitalized, e.g.
`A._B.MType` and `A._B.M`, but record field tags remain uncapitalized even when
they are associated with modules, e.g. `A.t._u.m`. There are no critical
technical reasons to require capitalization of module types and names in
Hemlock, but the distinct naming aids readability to a degree that justifies the
strict naming requirement.

```hemlock
let A = module
    let _B = module
        type MType = {|
            ...
          |}
        # M, U, and I are in the same namespace; beware module vs variant
        # constructor name collisions.
        let M = module
            ...
        type v =
            | U of uns
            | I of int

    type t = {
        _u: {
            m: A._B.MType
          }
      }
```

Following is a complete example of a module which implements persistent binary
trees.

```hemlock
let Tree = module
    type rec 'a t =
        | Empty
        | Node of
            lchild: 'a t
            value: 'a
            rchild: 'a t

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
```

`Tree` has the following type:

```hemlock
val Tree : {|
    type rec 'a t =
        | Empty
        | Node of
            lchild: 'a t
            value: 'a
            rchild: 'a t
    val empty: 'a t
    val is_empty: '_ t -> bool
    val node: 'a t -> 'a -> 'a t -> 'a t
    val leaf: 'a -> 'a t
    val lchild: 'a t -> 'a t
    val root_value_opt: 'a t -> 'a option
    val root_value_hlt: 'a t -> 'a
    val root_value: 'a t -> 'a
    val rchild: 'a t -> 'a t
  |}

type TreeSig = type of Tree
```

By default a module exports all of its contents, but an explicit module type,
delimited by `{|...|}`, can constrain what module contents are visible by
omitting elements and/or narrowing to less general types than the module
definition can support. The following example demonstrates a module type which
constrains what is externally visible (`'a t` is made abstract, `root_value_opt`
and `root_value_hlt` are omitted).

```hemlock
let TreeConstrained
  : {|
    type 'a t
    val empty: 'a t
    val is_empty: '_ t -> bool
    val node: 'a t -> 'a -> 'a t -> 'a t
    val leaf: 'a -> 'a t
    val lchild: 'a t -> 'a t
    val root_value: 'a t -> 'a
    val rchild: 'a t -> 'a t
  |} = module
    include Tree

type TreeConstrainedSig = type of TreeConstrained
```

`TreeConstrained` conceptually demonstrates an implicit application of a
function with type `TreeSig -> {|...|}`, though the syntax obscures that fact.
The following is equivalent.

```hemlock
# val make_TreeConstrained: (T: TreeSig) -> TreeConstrainedSig
let make_TreeConstrained (T: TreeSig)
  : {|TreeConstrainedSig with type 'a t := 'a T.t|}
  = module
    let empty = T.empty
    let is_empty = T.empty
    let node = T.node
    let leaf = T.leaf
    let lchild = T.child
    let root_value = T.root_value
    let rchild = T.rchild

let TreeConstrained = make_TreeConstrained Tree
```

Implicit function application also commonly occurs for top-level modules. For
example, suppose that `Tree` is implemented as a top-level module in `Tree.hm`.
The file contents (denoted as `<Tree.hm>`) are treated as if wrapped.

```hemlock
let Tree = module
    <Tree.hm>
```

If there is also a corresponding interface file, `Tree.hmi`, then the file
contents (denoted as `<Tree.hmi>`) are treated as if wrapped in combination with
`Tree.hm`.

```hemlock
let Tree
  : {|
    <Tree.hmi>
  |} = module
    <Tree.hm>
```

As seen earlier, this is application of a function with type `() ->
{|<Tree.hmi>|}`. If the module body were to cause effects during execution, the
function type would contain corresponding effects, e.g. `() >{os}->
{|<Tree.hmi>|}`.

#### `open`, `include`, and `import`

Outside `Tree`, the visible contents can be referred to via dot notation, e.g.
`Tree.value`. It is also possible to `open` a module's lexical namespace, i.e.
shadow the current lexical scope for the purposes of lookup without creating any
new lexical bindings.

```hemlock
open Tree
let tree = node (leaf 0) 1 empty
```

Alternatively, the scope for which the module is opened can be constrained to an
expression.

```hemlock
let tree = Tree.(node (leaf 0) 1 empty)
```

The `open` keyword merely shadows the current lexical scope for the purpose of
supporting identifier lookups, whereas the `include` keyword merges bindings
into the current lexical scope. The most common use case for `include` is to
create a new module which incorporates part or all of an `include`d module.

The `import` keyword returns a top-level module. As described earlier, top-level
module creation may cause effects, so `import` can incur effects. Therefore it
is generally advisable to `import` external modules at at the top level of a
module, and use `open`/`include` thereafter, in order to limit transitive effect
impacts to the top-level module, thus sparing submodules and functions from API
brittleness.

```hemlock
open import Basis
let Tree = import Tree

let AugmentedTree = module
    include Tree
    ...

let triple a b c =
    let open Tree
    node (leaf a) b (leaf c)
```
