# Effects and mutability

If Hemlock were a pure functional language, it would be effectless; its output would depend only on
its initial input. Hemlock does have a pure, effectless core, but non-trivial Hemlock programs rely
on effects, which can be roughly categorized as:

- Mutation of mutable data structures. For example, mutable arrays are sometimes the most efficient
  data structure for highly interdependent data and algorithms.
- Messages between actors. A Hemlock program need not run more than a single actor, but managing
  actor lifetimes, sending/receiving messages to/from other actors, etc. is effectful computation.
- Input/output mediated by the host operating system, whether for durable storage, communication
  with other processes and/or computers, etc., comprises effects outside the runtime system.
- Halting prevents subsequent computation, although an actor cannot observe itself halting. In many
  cases it is okay for an actor to ignore whether a computation may halt, but calls to functions
  like `assert` must not be optimized out, even though the calls produce no outputs.

Runtime system effects can be thought of in terms of loading/storing mutable program state. Loading
a mutable value may not seem like it should be considered an effect, but the fact that the mutable
value does not have a stable state means that repeated loads of the same value may return differing
results, thus affecting subsequent computation. Effectful loads must happen at "the right time" to
preserve program correctness, whereas loads of an immutable value return the same result regardless
of timing or repetition. Halting is an effect despite unobservability from the halting actor's
perspective; other actors can observe the effect, and of course halting precludes the actor
performing further computation!

Operating system effects are similar to runtime system effects in that repeating an effectful
operation may produce differing results. However, Hemlock cannot model operating system state
transitions with as much detail as for runtime system effects, so no practical distinction can be
made between operating system load versus store effects.

## Syntax quick reference

For more semantics detail than is provided in the following syntax quick reference, see below.

- Effect (`effect`)
  - + `{}`, `e`, `{e}` `{e|f}`, `{e|os|hlt}`: Effect sets. A set with exactly one member may omit
    the `{...}` enclosure.
  - Select pre-defined effects. See [Effects categories](#effects-categories) for more.
    + `os`: Operating system load-store effect
    + `ld`: Runtime system load effect
    + `mut`: Runtime system load-store effect
    + `hlt`: Runtime system may-halt effect
  - `f >e: ... >e-> ...`: Parametric effect.
  - `f >e\{os|mut}: ... >e-> ...`: Parametric effects constraint; transitively prohibited effects
    are enumerated as `\...` following parametric effect.
  - `(a: type) -> !&t a -> unit`, `(a: type) -> &t !&a >hlt-> unit`: Mutation effect on mutable
    function parameter type prefixed by `!`. The affected type need not be the outermost type, but
    the affected type must be mutable.
  - `(a: type) -> &t a -> &t a`: Load effect on mutable function parameter. A mutable function
    parameter always implies a `ld` effect on the parameter, without which the parameter would be
    useless.
  - `uns >{&t|(!&u)}-> unit`, `!&t >!&t-> unit`: Type-constrained environment `ld`/`mut` effect(s)
    during function application. Parentheses around type-constrained effects are optional, though
    they often improve readability. Note that a `!&t` parameter mutation is distinct from
    `>(!&t)->` environment mutation.
  - `uns >ld-> unit`, `uns >mut-> unit`: Environment `ld`/`mut` effect during function application.
    Type-agnostic environment effects impact mutable values of all types.
  - `~alloc->`, `~{alloc|hlt}->`, `>e~alloc->`: Explicit [never-parametric] concealed effect(s). An
    explicitly pure application is written as `~->`.
- Mutability (`mutability`)
  - `v ^m: t m`, `v ^t ^m: ^t&t m`, `v ^t ^m: ^&t m`: Parametric mutability. `^m` is a subsidiary
    mutability parameter, whereas `^t` comes before the type name and is the primary mutability
    parameter. `^t&t` can be shortened to `^&t`.
  - `_&t`: Mutability is contextually irrelevant, and therefore both immutable and mutable values
    are supported.
  - `&t`: Non-parametric primary mutability. `!&t` indicates a mutation effect on the value, which
    must therefore be mutable, as in `(a: type) -> ... -> !&t a -> ...`.
  - `\&t`: Transitive prohibited mutability constraint.
- Type (`type`)
  - `type a 'a: a`, `type a 'aa ^a ^m >e: ^&a aa m e`: Parametric type.
  - `type t 'a: t a`, `type t 'a ^m >e: t a m e`, ...: Primarily immutable parametric type.
  - `type t 'a: &t a`, `type t 'a ^m >e: &t a m e`, ...: Primarily mutable parametric type.
  - `type t 'a: t &a`: Subsidiarily mutable parametric type.
  - `type t 'a ^t: ^&t a`, `type t 'a ^t ^m: ^&t a m`: Parametric type with parametric primary
    mutability. `^m` is a subsidiary mutability parameter, whereas the primary mutability parameter
    `^t` is always the first type parameter (syntactically prefixing the type name), such that its
    absence and the absence of `&` indicates that the type is primarily immutable.
  - `f 'a ^t: a -> ^&t a`: The function produces an isolated value with parametric mutability. The
    value's mutability is either immutable or mutable as determined by type unification in the
    caller's context.
  - `'a >e\rt: \&t a e`: Parametric type constrained to prohibit runtime system effects (for
    associated functions which have the `>e` effect) and to prohibit transitive mutability.

# Static inference

Hemlock statically determines all effects and mutability transitively associated with a type,
whether primary or subsidiary. Runtime system effects and mutability are prohibited in global and
message values, though operating system effects (i.e. outside the Hemlock execution environment) are
allowed. Runtime system effects are lexically bound to a particular actor's local heap, and
therefore cannot be supported in the global heap. These restrictions are evaluated at compile time,
which means that the programming model is unaffected by type erasure. The absence of dynamic
validation overhead enables fast message passing, as well as fully automated migration of long-lived
immutable values to the global heap.

Effects are inferred prior to any transformations which could cause their elision. For example,
`f` has a `!&box uns` mutation effect even though the mutation is unreachable.

```hemlock
# f: !&box uns -> uns
f b =
    if false
        b.value := 0
    b.value
```

If effect elision were performed, APIs would be extremely brittle. Nearby code changes, changes in
any function in the transitive call graph, and even compiler flag/version changes could impact APIs.
That said, although interfaces are conservatively inferred, the compiler is free to internally
optimize away effectless code which does not produce useful values, whether via constant
propagation, dead code elimination, inlining, etc.

# Mutation of function parameters

See [Parametric types](types.md#parametric-types) for background on parametric types.

Only non-parametrically mutable types can be used in conjunction with mutation effects on function
parameters, as illustrated by the following examples.

```hemlock
# The array is mutable.
f_ok 'a: !&array a -> unit

# The boxes in the array are mutable.
f_ok2 'a: !array (^&box a) -> unit

# The type may not be mutable at all, so a mutation effect doesn't make sense.
f_wat 'a: !array a -> unit
```

## Mutability constraints

Concrete types have fixed effects and mutability, whereas abstract types also have transitive
effects and/or mutability that allow differing parametrizations to be any combination of
effectless/effectful and/or immutable/mutable. For example, `v 'a: array a` is primarily immutable,
but it may be transitively mutable depending on the particulars of the type supplied to the
subsidiary `'a` parameter. Were we to attempt passing `array a` in a message, the compiler would be
unable to prove that the message is compatible with the aforementioned constraints. Therefore we
must specify a transitive immutability constraint for the type, e.g. `v 'a: \&array a`. Similarly,
finalization is only supported for primarily mutable values, e.g. `v 'a: &a`, because referential
transparency of immutable values makes finalization ill-defined.

Type syntax is rather involved in its most general form, but most of its complexities only come into
play with module types. Following are some examples from the `Basis` library.

```hemlock
type list 'a: 'list a
type array 'a ^array: ^&array a
type map 'k 'v 'cmp: map \&k v cmp
type set 'a 'cmp: set \&a cmp

halt 'a: string >hlt-> a
not_reached 'a: unit >hlt-> a
abort 'a: string >hlt-> a

finalize 'a >e: (&a >e-> unit) -> &a >e-> unit

type elm 'a >e: elm a e =
  | Nil
  | Cons of (a, stream a e)
  also stream 'a >e: stream a e = Lazy.t (elm a e)
force 'a >e: lazy_t (elm a e) >e-> a
```

Parametric type `'a` may have its own effect and mutability parameters, e.g. `type a 'aa ^a >e: ^&a
aa e`. Given such a type for `a`, the set type could be instantiated as `set (^&a aa e) cmp`. The
`'a` parameter for `set` is constrained to prohibit mutability, lest key mutation corrupt the set.

Regarding effects versus mutability consider that `&array uns` is a mutable type, but actual
mutation is an effect.

```hemlock
length 'a: _&array a -> uns
set_inplace 'a: uns -> a -> !&array a -> unit
```

The `length` function has no effect even though it can operate on a mutable array, whereas the
`set_inplace` function has a mutation effect on the array, as indicated by `!`.

# Effects categories

Mutation-related effects are categorized as operating/runtime system and load/store. Runtime system
effects are only tracked for mutable data; indeed loading immutable data is effectless, and writing
immutable data is prohibited. Runtime system effects impact mutable program execution state whereas
operating system effects impact state outside the program, e.g. file input/output (I/O). Hemlock
cannot in general accurately model operating system load-only effects (and specific attempts would
be fraught with peril), so all operating system effects are modeled as combined load-store effects.
Hemlock does model runtime system load-only effects, but store effects imply load effects.

- `effect os`: Operating system load-store effect (mnemonic: Operating System)
- `effect ld`: Runtime system load effect (mnemonic: LoaD)
- `effect st`: Runtime system store effect (mnemonic: STore), used only via `mut`
- `effect mut = {ld|st}`: Runtime system load-store effect (mnemonic: MUTate), superset of `ld`
  rather than disjoint so that both `ld` and `mut` effect constraints independently prohibit mutable
  data accesses.
- `effect conceal alloc`: Concealable allocation effect (mnemonic: ALLOCation)
- `effect conceal hlt`: Concealable may-halt effect (mnemonic: HaLT)
- `effect rt = {mut|alloc|hlt}`: Runtime system effect (mnemonic: RunTime)

Effect declarations are always transparent, because the compiler needs full knowledge of effect
interrelationships in order to infer/propagate effects. In other words, if a module implementation
defines e.g. `effect e = {os|hlt}`, the module interface must also specify the full declaration;
`effect e` actually declares an independent effect.

`expose`/`conceal` preceding the name of in an effect declaration indicates 1) that the effect is
concealable, and 2) whether the effect is exposed or concealed at the outermost lexical scope. An
unconcealable effect can incorporate a concealable effect into its declaration. However a
concealable effect cannot incorporate unconcealable effects into its declaration.

Effects are tracked at partial application granularity, though with the exception of optional
parameter defaults, it typically suffices to think in terms of whole function applications.
Furthermore, although types can in principle have sophisticated parametric effects, the need rarely
arises outside of function signatures. Even function signatures rarely require the full power of
parametric effects.

```hemlock
unzip_map 'a 'b 'c 'd >e >f: (a >e-> c) -> (b >f-> d) -> list (a, b) >{e|f}-> (list c, list d)
```

Here `unzip_map` has the combined effects of two callback functions. Although this generalizes to an
arbitrary number of callback functions, the common cases are zero and one.

Parametric mutability exists primarily to constrain function parameter types, e.g. so that an input
type dictates an output type. Consider monomorphic identity function `id`, which subsumes the less
general `id_pure` and `id_mutable` signatures.

```hemlock
id ^t: ^&t -> ^&t
id_pure: t -> t
id_mutable: &t -> &t
```

This basic concept enables more sophisticated parametric mutability within type parameters. For
example, suppose we want a record type to contain two `array a` fields of distinct parametric
mutabilities.

```hemlock
type t 'a ^m ^n: t a m n = {
    m: ^m&array a
    n: ^n&array a
  }
init 'a ^m ^n: ^m&array a -> ^n&array a -> t a m n
```

## Concealable effects (`expose`/`conceal`)

The runtime requires most pre-defined effects to be fully transitive for correctness reasons so
that, for example, it is impossible to hide a mutation effect from the runtime. On the other hand,
some effects can usually be hidden from callers without affecting correctness. Furthermore,
pervasive effects would have huge deleterious impacts on APIs were they allowed to transitively
propagate without limit. Therefore effects can be declared as concealable, with exposure at the
outermost lexical scope either `conceal` or `expose`.

```hemlock
effect         eu # Unconcealable effect.
effect conceal ec # Concealable effect, concealed at outermost scope.
effect expose  ee # Concealable effect, exposed at outermost scope.
```

The `expose` and `conceal` keywords introduce nestable lexical scopes which expose and conceal
specific concealable effects, respectively.

```hemlock
# Outermost scope, with exposure as if the module were wrapped with
#     conceal ec \
#     expose ee \
[...]
expose ec \
[...]
conceal ee
    [...]
    expose ee
        [...]
```

See the following `hlt` and `alloc` documentation for more sophisticated examples of
`expose`/`conceal` usage.

Concealable effects are usually irrelevant, and therefore omitted, in function signatures. However,
low-level runtime code (and perhaps performance-critical application code) sometimes needs complete
knowledge and control over effects. For example, `alloc` effects must be avoided in portions of the
garbage collector. The compiler needs comprehensive transitive knowledge of effects to guarantee
that such code is `alloc`-free. The following example application arrows illustrate explicit
concealable effects syntax. The key syntactic feature of arrows with explicit concealable effects is
the `~` preceding the effect set in `~...->`. `...~->` indicates an empty set of concealed effects.

```hemlock
>{e|os}~{alloc|hlt}->
~{alloc|hlt}->
>{e|os}~->
~->
>e~->
```

A function which makes concealed effects explicit can only call functions which in turn transitively
make concealed effects explicit. This restriction is obviously required for the compiler to be able
to infer concealed effects. In practice, this means that any facility which low-level code requires
must make its concealed effects explicit, and that such low-level code must be carefully written to
avoid undesirable concealed effect propagation.

### May-halt effect (`hlt`)

The `hlt` (may-halt) effect can usually be hidden from callers without affecting correctness, and is
therefore concealed by default.

```hemlock
effect conceal hlt
```

In order for a function type to divulge may-halt effects, all lexical scopes within the function
which contain the effectful code must expose may-halt effects. Following are several examples of a
division function which demonstrate how `expose`/`conceal` affect may-halt effect visibility.

```hemlock
halt_unless 'a: bool >hlt-> a

# div: uns -> uns -> uns
div x y =                           # concealed | C
    halt_unless (y > 0)             # | hlt     | C
    x / y                           # |         | C

# div': uns -> uns >hlt-> uns
div' x y = expose hlt               # exposed | E
    halt_unless (y > 0)             # | hlt   | E
    x / y                           # |       | E

# div'': uns -> uns -> uns
div'' x y = expose hlt              # exposed     | E
    conceal hlt                     # | concealed | C
        halt_unless (y > 0)         # | | hlt     | C
    x / y                           # |           | E

# div''': uns -> uns -> uns
div''' x y = expose hlt             # exposed     | E
    conceal hlt                     # | concealed | C
        expose hlt                  # | | exposed | C
            halt_unless (y > 0)     # | | | hlt   | C
    x / y                           # |           | E
```

The limited need for may-halt effect tracking is a subtle topic. In many cases the lack of may-halt
effect tracking can be masked by associated data dependencies and/or other effects, but there are
some critical cases where code breaks unless may-halt effects are exposed. For example, if we write
a `halt_unless` function as follows, calls to it can be optimized out.

```hemlock
halt_pure 'a: string -> a # Hypothetical effectless halt.
continue 'a: unit -> a

# halt_unless 'a: bool -> a
halt_unless cnd =
    match cnd with
      | false -> halt_pure "Condition false"
      | true -> continue ()

# The result isn't used, so the call can be optimized out!
_ = halt_unless false
```

This could be worked around by causing a mutation.

```hemlock
halt_pure 'a: string -> a # Hypothetical effectless halt.
continue 'a: unit -> a

# halt_unless: bool >mut-> unit
halt_unless cnd =
    match cnd with
      | false ->
        File.Fmt.stdout |> Fmt.fmt "Condition false" |> ignore
        halt_pure "Condition false"
      | true -> continue ()

# div has a transitive mutation effect due to calling halt_unless.
# div: uns -> uns >mut-> uns
div a b =
    halt_unless (b <> 0)
    a / b

x = div 4 2
_ = div 1 0 # Division by 0 will halt.
```

Clearly `halt_unless` needs to have a may-halt effect in order to behave as desired. In fact, this
is also true of any otherwise effectless function which may be called without regard for the result.

```hemlock
halt_unless 'a: cnd -> a

type t: t = {
    field_a: bool
    field_b: uns
  }

# Effectless; calls may be optimized out!
# validate 'a: t -> a
validate t =
    halt_unless t.field_a
    halt_unless (t.field_b > 0)
```

In the following, `halt_unless` has a may-halt effect, because it exposes transitive may-halt
effects, namely those of the `halt` function.

```hemlock
# halt 'a: string >hlt-> a
# continue 'a: unit -> a

# halt_unless 'a: bool >hlt-> a
halt_unless cnd = expose hlt
    match cnd with
      | false -> halt "Condition false"
      | true -> continue ()

# No may-halt effect, even though div can halt!
# div: uns -> uns -> uns
div a b =
    halt_unless (b <> 0) # Always called if div is called.
    a / b

x = div 4 2
_ = div 1 0 # Would halt, but can be optimized away.
```

Note that `div` has no exposed may-halt effect, even though it can halt! That is desirable in this
case because there is no point in failing to divide by zero unless the division actually needs to
happen. But it would be possible to `expose` halts within the function body and change the signature
to `(uns -> uns >hlt-> uns)`.

For most functions which produce non-unit values, concealing may-halt effects is preferable, for
multiple reasons:

- APIs are more stable if the addition/removal of an `assert` does not change the function
  signature.
- Otherwise-effectless functions which produce non-unit values usually need not be called if their
  results are unused. Exposing may-halt effects for such functions forces the compiler to preserve
  such calls even if they do nothing useful, such as calling `assert` even when the assertions are
  statically determined to be true (and therefore optimized out). Even functions with `_hlt` name
  suffixes rarely need to expose may-halt effects.

On the other hand, functions which produce the unit value and call a callback function with
transitive parametric effects should transitively expose the callback's may-halt effects, in order
to avoid an effects constraint on the callback function.

```hemlock
# iter2 'a 'b >e: f:(a -> b >e-> unit) -> _&array a -> _&array b >e-> unit
iter2 ~f a0 a1 = expose hlt
    let open Array
    conceal hlt (assert (|length a0 = length a1|))
    iter (0 .. (length a0)) ~f:(fn i ->
        f (get i a0) (get i a1)
      )

a = [|0; 1; 2|]
b = [|3; 4; 5|]
# ~f's may-halt effect due to calling assert keeps the iter2 call from being optimized out.
iter2 ~f:(fn elm_a elm_b -> assert (|elm_a + 3 = elm_b|)) a b
# ... Dependencies on a and/or b which prevent optimizing them away.

# May-halt effects are prohibited in ~f because of may-halt concealment.
# iter2' 'a 'b >e\hlt: f:(a -> b >e-> unit) -> _&array a -> _&array b >e-> unit
iter2' ~f a0 a1 =
    iter2 ~f a0 a1
```

### Allocation effect (`alloc`)

Implicit allocation is pervasive in Hemlock, whether for explicit composite values like lists and
arrays, or for implicit data structures like closures as they capture free variables from their
environments. The `alloc` effect is concealed by default.

```hemlock
effect conceal alloc
```

Unlike the `hlt` effect, the `alloc` effect is almost never exposed. Instead it is used in
conjunction with explicit concealed effects to assure that low-level code does not allocate. For
example, the garbage collector internally makes its concealed effects explicit and pointedly omits
`alloc` from its effects. If `collect_impl` were to have an `alloc` effect, it would be incompatible
with the interface and a compiler error would result.

```hemlock
# Gc.hmi excerpt.
type t: t

collect: t -> t
```

```hemlock
# Gc.hm excerpt.
type t: t = [...]

T : {
    collect_minor: t ~{gc|hlt}-> t
  } = {
    effect conceal gc
    collect_impl t =
        [...]
  }

collect t =
    T.collect_impl t
```

Conversely, if the implementation were to have no `hlt` effects, the implementation would remain
compatible with the interface. Nonetheless, explicit concealed effects present a brittle API that
can require transitively exposing incidental implementation details. Module interfaces should leave
concealable effects implicit unless functions have legitimate uses in low-level code which requires
complete knowledge of transitive effects, and even then such interfaces should have few, if any,
concealed effects. `Uns.\+` is a prime practical example.

```hemlock
\+: uns ~-> uns ~-> uns
```

## Partial application

Partial application may transform function signatures with regard to effects. The following are
equivalent, but per parameter effects and explicit concealed effects require non-trivial signature
transformation.

```hemlock
f_complete: uns -> uns -> uns
f_partial: uns -> (uns -> uns)
```

### Per parameter effects

Partial application of parameters to a function requires special consideration if applied parameters
have associated effects. Similarly, parametric effects denoted by e.g. `>e->` require special
consideration when a supplied function has parameter-specific effects.

Consider what happens when partial application of a parameter with associated effects produces a
closure. The resulting closure type must merge the parameter's effects into the final application
effects in order to preserve the effect. Although this transformation reduces fidelity, it does not
affect correctness. The following example illustrates how mutation of `f`'s array parameter
preserves the mutation effect in the closure it produces.

```hemlock
# g: !&array uns -> unit -> unit
g arr () =
    Array.set_inplace 0 42 arr

# f: !&array uns -> (unit >(!&array uns)-> unit)
f arr =
    g arr
```

In general, any time a partial application closes on a parameter with effects, those effects are
incorporated into the final application arrow for the resulting closure. In the common case where
all parameters are provided, the relationship upon which this relies is invisible. The `f_precise`
function as defined below can be trivially wrapped and made compatible with the `f_general` and
`f_vague` signatures, but `f_precise` provides more precise information about what is being mutated,
which aids both programmer reasoning and compiler optimization.

```hemlock
f_precise: !&array uns -> uns -> unit
f_general: !&array uns -> uns >(!&array uns)-> unit
f_vague: !&array uns -> uns >mut-> unit
```

Although the compiler will infer the most precise function types possible, Hemlock permits a module
to expose a general function signature for a function implementation with more specific effect
typing. Although such flexibility is of limited utility for simple functions, it is critical to
enabling succinct parametric effect typing for callback functions. Consider the following use of
`iter2` as defined earlier.

```hemlock
iter2 'a 'b >e: f:(a -> b >e-> unit) -> _&t a -> _&t b >e-> unit

a: _&array uns = [|0; 1; 2|]
b: _&array &box uns = [|box 3; box 4; box 5|]

# f: uns -> !&box uns -> unit
f elm_a elm_b =
    elm_b.value := elm_a + elm_b.value

iter2 ~f a b
```

`f` is compatible with the `~f` parameter to `iter2`, even though `~f` only parametrizes effects on
the final application. While the following signature for `iter2` would clearly account for per
parameter effects, the above code works because functions with precise effect typing are compatible
specializations of the more general form.

```hemlock
# Excessive parametric effect typing.
iter2 'a 'b >ea >eb >ef: f:(a >ea-> 'b >{eb|ef}-> unit) -> _&t a -> _&t b >{ea|eb|ef}-> unit
```

Precise effects typing of callback functions is useful for local optimization even if the
intermediary (e.g. `iter2`) is not specialized to take advantage of the precision. And if the entire
call chain is inlined, the precise effect typing enables optimal machine code generation.

### Explicit concealed effects

Partial application requires closure allocation. Functions like `Uns.\+` do not require
allocation, and because addition is critical functionality in low-level code, the API is explicit
regarding (lack of) concealed effects.

```hemlock
\+: uns ~-> uns ~-> uns
```

However, if `+` is partially applied, the compiler transforms the function to one with an `alloc`
effect, with signature equivalent to `+*`.

```hemlock
\+*: uns ~alloc-> (uns ~-> uns)
```

As a consequence, partial application is prohibited within functions like `f`, and allowed within
functions like `g` and `h`.

```hemlock
f: uns ~-> uns ~-> uns
g: uns ~alloc-> (uns ~-> uns)
h: uns ~-> uns ~alloc-> uns
```

# Modules

Module types are special in that they can constrain which internal implementation details are
visible outside the module. Therefore module types must explicitly reveal primary mutability,
regardless of whether visible values make the information redundant.

```hemlock
type SX: &SX = &{&assign: uns >mut-> unit}

# X's type partially reveals why it is mutable (x and assign are mutable).
X
  : &SX
  = {
    &x := 0
    &assign := (fun u -> x := u)
  }
# unit -> &SX

# M's type doesn't reveal why it is mutable (m is mutable).
M
  : &{f: uns >ld-> uns}
  = {
    &m := 42
    f u = u * m
    X.assign := (fun u -> m := u)
  }
# unit >(!&SX)-> &{f: uns >ld-> uns}
```

Note that the right-hand side (RHS) of the module assignment to `M` is actually application of an
effectful function. This is due to mutating `X.assign` within the module body, which occurs during
module creation. Every application of `M` causes a mutation effect on type `&SX`. This effect could
be avoided as follows.

```hemlock
makeM X
  : &{f: uns >ld-> uns}
  = {
    &m := 42
    f u = u * m
    X.assign := (fun u -> m := u)
  }
# makeM: !&SX -> &{f: uns >ld-> uns}

M = makeM X

# Equivalent to the above, but avoids creating the makeM lexical binding.
M = (fn X
  : &{f: uns >ld-> uns}
  -> {
    &m := 42
    f u = u * m
    X.assign := (fun u -> m := u)
  }) X
# !&SX -> &{f: uns >ld-> uns}
```
