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

- Effects
  - `>{>e,>f}`, `>e`, `>`: Parametric effect(s). The `>e` and `>` short forms can be used only if
    there is a single parametric effect.
  - `>_`: Effects are contextually irrelevant, and therefore arbitrarily permitted.
  - Select pre-defined effects. See [Effects categories](#effects-categories) for more.
    + `>{os}`: Operating system load-store effect
    + `>{ld}`: Runtime system load effect
    + `>{mut}`: Runtime system load-store effect
    + `>{hlt}`: Runtime system may-halt effect
  - `>{os,hlt}`: Set of effects.
  - `>{>e,>f}\{os,mut}`, `>\{mut}`: Effects constraint; transitively prohibited effects are
    enumerated as `\{...}` following any effects.
  - `('a t&! -> unit)`, `('a&! t& >{hlt}-> unit)`: Mutation effect on mutable function
    parameter type suffixed by `!`. The affected type need not be the outermost type, but the
    affected type must be mutable.
  - `('a t& -> 'a t)`: Load effect on mutable function parameter. A mutable function parameter
    always implies a `ld` effect on the parameter, without which the parameter would be useless.
  - `(uns >{t&,u&!}-> unit)`, `(t&! >{t&!}-> unit)` : Type-constrained environment `ld`/`mut` effect
    during function application, listed in `>{...}->`. Note that a `t&!` parameter mutation is
    distinct from `>{t&!}->` environment mutation.
  - `(uns >{ld}-> unit)`, `(uns >{mut}-> unit)`: Environment `ld`/`mut` effect during function
    application. Type-agnostic environment effects impact mutable values of all types.
  - `-{alloc}`, `-{alloc,hlt}`: Explicit [never-parametric] concealed effect(s). An explicitly pure
    application is written as `-->`, which is shorthand for `-{}->` (or more verbosely,
    `>{}\{}-{}->`).
- Mutability
  - `^m t`, `^m t ^t`, `^m t^`: Parametric mutability. `^m` is an subsidiary mutability parameter,
    whereas `^t` comes after the type name and is the primary mutability parameter. When otherwise
    unambiguous in a type expression, `t ^t` can be shortened to `t^` (or `t ^`) and the parameter
    takes the same name as the type.
  - `^_ t`, `^_ t^_`: Mutability is contextually irrelevant, and therefore both immutable and
    mutable values are supported.
  - `t&`: Non-parametric primary mutability. `t&!` indicates a mutation effect on the value, which
    must therefore be mutable, as in `... -> 'a t&! -> ...`.
  - `t\&`, `^_ t^_\&`: Transitive prohibited mutability constraint.
- Parametric types:
  - `'a`: Parametric type, whether simple as in `uns` or with its own parameters as in
    `('aa, ^m, >e) 'a^`.
  - `'a t`, `('a, ^m, >e) t`, ...: Primarily immutable parametric type.
  - `'a t&`, `('a, ^m, >e) t&`, ...: Primarily mutable parametric type.
  - `'a& t`: Subsidiarily mutable parametric type.
  - `'a t ^t`, `'a t^`, `('a, ^m) t^`: Parametric type with parametric primary mutability. `^m` is a
    subsidiary mutability parameter, whereas the primary mutability parameter `^t` is distinct from
    the comma-separated `(...)` parameter list, such that its absence and the absence of `&`
    indicates that the type is primarily immutable.
  - `val f: 'a -> 'a t^`: The function produces an isolated value with parametric mutability. The
    value's mutability is either immutable or mutable as determined by type unification in the
    caller's context.
  - `('a, ^m, >e) ['c, 'd] t&`: Primarily mutable type with transitive effects and mutability
    determined by `('a, ^m, >e)`, and additional parameters which have no impact on the type's
    transitive effects and mutability determined by `['c, 'd]`.
  - `('a, >t\{rt}) t\&`: Parametric type constrained to prohibit both runtime system effects (for
    associated functions which have the `>t` effect) and transitive mutability.

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
`f` has a `(uns box&!)` mutation effect even though the mutation is unreachable.

```hemlock
# val f: uns box&! -> uns
let f b =
    if false
        b.value := 0
    b.value
```

If effect elision were performed, APIs would be extremely brittle. Nearby code changes, changes in
any function in the transitive call graph, and even compiler flag/version changes could impact APIs.
That said, although interfaces are conservatively inferred, the compiler is free to internally
optimize away effectless code which does not produce useful values, whether via constant
propagation, dead code elimination, inlining, etc.

# Parametricity and mutability/effects

Every type has a primary component which is always a type, aka primary type. Parametric types may
additionally have subsidiary components, some of which may be subsidiary types. For example, `uns`
has only a primary type, whereas `('a, 'b, ^m) t` has `t` as its primary type, `'a` and `'b` as
subsidiary types, and `^m` as a non-type subsidiary component. Every type has associated mutability,
of which there are three cases:

- **Immutable:** The type is [non-parametrically] immutable, e.g. `uns` and `uns array`.
- **Mutable:** The type is [non-parametrically] mutable, e.g. the array in `uns array&` and the
  boxes in `uns box& array`.
- **Parametrically mutable:** The type's mutability is a parameter of the type, e.g. the array in
  `uns array^`, the boxes in `uns box^ array`, and the record `r` in `r^`.

All of the above examples focus on concrete types, but subsidiary mutability can come into play
for abstract types. For example, `'a array` denotes an immutable array, but the subsidiary `'a`
elements are parametrically mutable.

Only non-parametrically mutable types can be used in conjunction with mutation effects on function
parameters, as illustrated by the following examples.

```hemlock
# The array is mutable.
val f_ok: 'a array&! -> unit

# The boxes in the array are mutable.
val f_ok2: 'a box& array! -> unit

# The type may not be mutable at all, so a mutation effect doesn't make sense.
val f_wat: 'a array! -> unit
```

## Mutability constraints

Concrete types have fixed effects and mutability, whereas abstract types also have transitive
effects and/or mutability that allow differing parametrizations to be any combination of
effectless/effectful and/or immutable/mutable. For example, `'a array` is primarily immutable, but
it may be transitively mutable depending on the particulars of the type supplied to the subsidiary
`'a` parameter. Were we to attempt passing `'a array` in a message, the compiler would be unable to
prove that the message is compatible with the aforementioned constraints. Therefore we must specify
a transitive immutability constraint for the type, e.g. `'a array\&`. Similarly, finalization is
only supported for primarily mutable values, e.g. `'a&`, because referential transparency of
immutable values makes finalization ill-defined.

Type syntax is rather involved in its most general form, but most of its complexities only come into
play with module types. Following are some examples from the `Basis` library.

```hemlock
'a list
'a array^
('k\&, 'v, 'cmp) map
('a\&, 'cmp) set

val halt: string >{hlt}-> 'a
val not_reached: unit >{hlt}-> 'a
val abort: string >{hlt}-> 'a

val finalize: ('a& >-> unit) -> 'a& >-> unit

type 'a elm =
  | Nil
  | Cons of 'a * ('a, >e) stream
also ('a, >e) stream = ('a elm, >e) Lazy.t
val force: ('a, >e) lazy_t >e-> 'a
```

Parametric type `'a` may have its own effect and mutability parameters, e.g. `('aa, >a) 'a^`. For
example, the set type could be `(>a 'a^\&, >cmp 'cmp^) set`. The `'a` parameter is constrained to
prohibit mutability, lest key mutation corrupt the set.

Regarding effects versus mutability consider that `'a array&` is a mutable type, but actual mutation
is an effect.

```hemlock
val length: 'a array^_ -> uns
val set_inplace: uns -> 'a -> 'a array&! -> unit
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

- `effect none = >{}`: Empty set
- `effect os`: Operating system load-store effect (mnemonic: Operating System)
- `effect ld`: Runtime system load effect (mnemonic: LoaD)
- `effect st`: Runtime system store effect (mnemonic: STore), used only via `>{mut}`
- `effect mut = >{ld,st}`: Runtime system load-store effect (mnemonic: MUTate), superset of `>{ld}`
  rather than disjoint so that both `\{ld}` and `\{mut}` effect constraints independently prohibit
  mutable data accesses.
- `effect conceal alloc`: Concealable allocation effect (mnemonic: ALLOCation)
- `effect conceal hlt`: Concealable may-halt effect (mnemonic: HaLT)
- `effect rt = >{mut,hlt}`: Runtime system effect (mnemonic: RunTime)
- `effect all = >{os,rt}`: Set of all effects

Effects are tracked at partial application granularity, though with the exception of optional
parameter defaults, it typically suffices to think in terms of whole function applications.
Furthermore, although types can in principle have sophisticated parametric effects, the need rarely
arises outside of function signatures. Even function signatures rarely require the full power of
parametric effects.

```hemlock
val unzip_map: ('a >e-> 'c) -> ('b >f-> 'd) -> ('a * 'b) list
  >{>e,>f}-> 'c list * 'd list
```

Here `unzip_map` has the combined effects of two callback functions. Although this generalizes to an
arbitrary number of callback functions, the common cases are zero and one. If there is only one
parametric effect in a function signature, the effect parameter name can be omitted, e.g.

```hemlock
val iter2: f:('a -> 'b >-> unit) -> 'a t^_ -> 'b t^_ >-> unit
```

Parametric mutability exists primarily to constrain function parameter types, e.g. so that an input
type dictates an output type. Consider this monomorphic identity function:

```hemlock
val id: >e t^ -> >e t^
```

`id` subsumes several less general signatures, e.g.

```hemlock
val id_pure: t -> t
val id_mutable: t& -> t&
val id_effectful: >e t -> >e t
```

This basic concept enables more sophisticated parametric mutability within type parameters. For
example, suppose we want a record type to contain two `'a array` fields of distinct parametric
mutabilities.

```hemlock
type ('a, ^m, ^n) t =
    m: 'a array^m
    n: 'a array^n
val init: 'a array^m -> 'a array^n -> ('a, ^m, ^n) t
```

In more involved cases, parametric types require parameters which have no direct bearing on the
type's effects nor mutability. Such parameters are segregated into a separate `[...]` parameter
list, e.g.

```hemlock
type 'a outer^ = 'a t^outer
type ('a, ^m, 'b, ^n, >e) ['accum, 'c] t =
    m: 'a outer^m
    n: 'b outer^n
    f: uns -> 'accum -> 'a -> 'b >e-> 'accum * 'c
    index: uns
```

## Concealable effects (`expose`/`conceal`)

The runtime requires most pre-defined effects to be fully transitive for correctness reasons, so
that, for example, it is impossible to hide a mutation effect from the runtime. On the other hand,
some effects can usually be hidden from callers without affecting correctness. Furthermore,
pervasive effects would have huge deleterious impacts on APIs were they allowed to transitively
propagate without limit. Therefore effects can be declared as concealable, with exposure at the
outermost lexical scope either `conceal` or `expose`.

```hemlock
effect         eu   # Unconcealable effect.
effect conceal ec   # Concealable effect, concealed at outermost scope.
effect expose  ee   # Concealable effect, exposed at outermost scope.
```

The `expose` and `conceal` keywords introduce nestable lexical scopes which expose and conceal
specific concealable effects, respectively.

```hemlock
# Outermost scope, with exposure as if the module were wrapped with
#     conceal >{ec} \
#     expose >{ee} \
[...]
expose >{ec} \
[...]
conceal >{ee}
    [...]
    expose >{ee}
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
the `-{...}` effects set syntax, where `-{}` can be abbreviated as `-`.

```hemlock
>{>e,os}\{mut}-{alloc,hlt}->
-{alloc,hlt}->
>{>e,os}\{mut}-{}-> >{>e,os}\{mut}-->
-->
>-->
>e-{}-> >e-->
>{>e}-{}-> >{>e}-->
```

In general, empty effects sets can be optionally omitted, but the `-` sigil for explicit concealable
effects is mandatory. All the following arrows are equivalent.

```hemlock
-->
>{}-->
>{}\{}-->
-{}->
>{}-{}->
>{}\{}-{}->
```

A function which make concealed effects explicit can only call functions which in turn transitively
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
val halt_unless: bool >{hlt}-> 'a

# val div: uns -> uns -> uns
let div x y =                       # concealed | C
    halt_unless (y > 0)             # | >{hlt}  | C
    x / y                           # |         | C

# val div': uns -> uns >{hlt}-> uns
let div' x y = expose >{hlt}        # exposed  | E
    halt_unless (y > 0)             # | >{hlt} | E
    x / y                           # |        | E

# val div'': uns -> uns -> uns
let div'' x y = expose >{hlt}       # exposed     | E
    conceal >{hlt}                  # | concealed | C
        halt_unless (y > 0)         # | | >{hlt}  | C
    x / y                           # |           | E

# val div''': uns -> uns -> uns
let div''' x y = expose >{hlt}      # exposed      | E
    conceal >{hlt}                  # | concealed  | C
        expose >{hlt}               # | | exposed  | C
            halt_unless (y > 0)     # | | | >{hlt} | C
    x / y                           # |            | E
```

The limited need for may-halt effect tracking is a subtle topic. In many cases the lack of may-halt
effect tracking can be masked by associated data dependencies and/or other effects, but there are
some critical cases where code breaks unless may-halt effects are exposed. For example, if we write
a `halt_unless` function as follows, calls to it can be optimized out.

```hemlock
val halt_pure: string -> 'a # Hypothetical effectless halt.
val continue: unit -> 'a

# val halt_unless: bool -> 'a
let halt_unless cnd =
    match cnd with
    | false -> halt_pure "Condition false"
    | true -> continue ()

# The result isn't used, so the call can be optimized out!
let _ = halt_unless false
```

This could be worked around by causing a mutation.

```hemlock
val halt_pure: string -> 'a # Hypothetical effectless halt.
val continue: unit -> 'a

# val halt_unless: bool >{mut}-> unit
let halt_unless cnd =
    match cnd with
    | false ->
        Printf.printf "Condition false"
        halt_pure "Condition false"
    | true -> continue ()

# div has a transitive mutation effect due to calling halt_unless.
# val div: uns -> uns >{mut}-> uns
let div a b =
    halt_unless (b <> 0)
    a / b

let x = div 4 2
let _ = div 1 0 # Division by 0 will halt.
```

Clearly `halt_unless` needs to have a may-halt effect in order to behave as desired. In fact, this
is also true of any otherwise effectless function which may be called without regard for the result.

```hemlock
val halt_unless: cnd -> 'a

type t =
    field_a: bool
    field_b: uns

# Effectless; calls may be optimized out!
# val validate: t -> 'a
let validate t =
    halt_unless t.field_a
    halt_unless (t.field_b > 0)
```

In the following, `halt_unless` has a may-halt effect, because it exposes transitive may-halt
effects, namely those of the `halt` function.

```hemlock
# val halt: string >{hlt}-> 'a
# val continue: unit -> 'a

# val halt_unless: bool >{hlt}-> 'a
let halt_unless cnd = expose >{hlt}
    match cnd with
    | false -> halt "Condition false"
    | true -> continue ()

# No may-halt effect, even though div can halt!
# val div: uns -> uns -> uns
let div a b =
    halt_unless (b <> 0) # Always called if div is called.
    a / b

let x = div 4 2
let _ = div 1 0 # Would halt, but can be optimized away.
```

Note that `div` has no exposed may-halt effect, even though it can halt! That is desirable in this
case because there is no point in failing to divide by zero unless the division actually needs to
happen. But it would be possible to `expose` halts within the function body and change the signature
to `(uns -> uns >{hlt}-> uns)`.

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
# val iter2: f:('a -> 'b >-> unit) -> 'a array^_ -> 'b array^_ >-> unit
let iter2 ~f a0 a1 = expose >{hlt}
    let open Array in
    conceal >{hlt} (assert (length a0 = length a1))
    for i = 0 to pred (length a0) do
        f (get i a0) (get i a1)

let a = [|0; 1; 2|]
let b = [|3; 4; 5|]
# ~f's may-halt effect due to calling assert keeps the iter2 call from being optimized out.
iter2 ~f:(fun elm_a elm_b -> assert (elm_a + 3 = elm_b)) a b
# ... Dependencies on a and/or b which prevent optimizing them away.

# May-halt effects are prohibited in ~f because of may-halt concealment.
# val iter2': f:('a -> 'b >e\{hlt}-> unit) -> 'a array^_ -> 'b array^_ >e\{hlt}-> unit
let iter2' ~f a0 a1 =
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
type t

val collect: t >-> t
```

```hemlock
# Gc.hm excerpt.
type t = [...]

effect conceal gc

let T : {|
    val collect_minor: t >-{gc,hlt}-> t
  |} = module
    effect conceal gc
    let collect_impl t =
        [...]

let collect t =
    T.collect_impl t
```

Conversely, if the implementation were to have no `hlt` effects, the implementation would remain
compatible with the interface. Nonetheless, explicit concealed effects present a brittle API that
can require transitively exposing incidental implementation details. Module interfaces should leave
concealable effects implicit unless functions have legitimate uses in low-level code which requires
complete knowledge of transitive effects, and even then such interfaces should have few, if any,
concealed effects. `Uns.( + )` is a prime practical example.

```hemlock
val ( + ): uns --> uns --> uns
```

## Partial application

Partial application may transform function signatures with regard to effects. The following are
equivalent, but per parameter effects and explicit concealed effects require non-trivial signature
transformation.

```hemlock
val f_complete: uns -> uns -> uns
val f_partial: uns -> (uns -> uns)
```

### Per parameter effects

Partial application of parameters to a function requires special consideration if applied parameters
have associated effects. Similarly, parametric effects denoted by e.g. `>->` require special
consideration when a supplied function has parameter-specific effects.

Consider what happens when partial application of a parameter with associated effects produces a
closure. The resulting closure type must merge the parameter's effects into the final application
effects in order to preserve the effect. Although this transformation reduces fidelity, it does not
affect correctness. The following example illustrates how mutation of `f`'s array parameter
preserves the mutation effect in the closure it produces.

```hemlock
# val g: uns array&! -> unit -> unit
let g arr () =
    Array.set_inplace 0 42 arr

# val f: uns array&! -> (unit >{uns array&!}-> unit)
let f arr =
    g arr
```

In general, any time a partial application closes on a parameter with effects, those effects are
incorporated into the final application arrow for the resulting closure. In the common case where
all parameters are provided, the relationship upon which this relies is invisible. The `f_precise`
function as defined below can be trivially wrapped and made compatible with the `f_general` and
`f_vague` signatures, but `f_precise` provides more precise information about what is being mutated,
which aids both programmer reasoning and compiler optimization.

```hemlock
val f_precise: uns array&! -> uns -> unit
val f_general: uns array&! -> uns >{uns array&!}-> unit
val f_vague: uns array&! -> uns >{mut}-> unit
```

Although the compiler will infer the most precise function types possible, Hemlock permits a module
to expose a general function signature for a function implementation with more specific effect
typing. Although such flexibility is of limited utility for simple functions, it is critical to
enabling succinct parametric effect typing for callback functions. Consider the following use of
`iter2` as defined earlier.

```hemlock
val iter2: f:('a -> 'b >-> unit) -> 'a t^_ -> 'b t^_ >-> unit

let a: uns array^_ = [|0; 1; 2|]
let b: uns box& array^_ = [|box 3; box 4; box 5|]

# val f: uns -> uns box&! -> unit
let f elm_a elm_b =
    elm_b.value := elm_a + elm_b.value

iter2 ~f a b
```

`f` is compatible with the `~f` parameter to `iter2`, even though `~f` only parametrizes effects on
the final application. While the following signature for `iter2` would clearly account for per
parameter effects, the above code works because functions with precise effect typing are compatible
specializations of the more general form.

```hemlock
# Excessive parametric effect typing.
val iter2: f:('a >a-> 'b >{>b,>f}-> unit) -> 'a t^_ -> 'b t^_ >{>a,>b,>f}-> unit
```

Precise effects typing of callback functions is useful for local optimization even if the
intermediary (e.g. `iter2`) is not specialized to take advantage of the precision. And if the entire
call chain is inlined, the precise effect typing enables optimal machine code generation.

### Explicit concealed effects

Partial application requires closure allocation. Functions like `Uns.( + )` do not require
allocation, and because addition is critical functionality in low-level code, the API is explicit
regarding (lack of) concealed effects.

```hemlock
val ( + ): uns --> uns --> uns
```

However, if `+` is partially applied, the compiler transforms the function to one with an `alloc`
effect, with signature equivalent to `+*`.

```hemlock
val ( +* ): uns -{alloc}-> (uns --> uns)
```

As a consequence, partial application is prohibited within functions like `f`, and allowed within
functions like `g` and `h`.

```hemlock
val f: uns --> uns --> uns
val g: uns -{alloc}-> (uns --> uns)
val h: uns --> uns -{alloc}-> uns
```

# Modules

Module types are special in that they can constrain which internal implementation details are
visible outside the module. Therefore module types must explicitly reveal primary mutability,
regardless of whether visible values make the information redundant.

```hemlock
type SX& = {|val assign&: uns >{mut}-> unit|}&

# X's type partially reveals why it is mutable (x and assign are mutable).
let X
  : SX&
  = module
    let x& := 0
    let assign& := (fun u -> x := u)
# unit -> SX&

# M's type doesn't reveal why it is mutable (m is mutable).
let M
  : {|val f: uns >{ld}-> uns|}&
  = module
    let m& := 42
    let f u = u * m
    X.assign := (fun u -> m := u)
# unit >{SX&!}-> {|val f: uns >{ld} -> uns|}&
```

Note that the right-hand side (RHS) of the module assignment to `M` is actually application of an
effectful function. This is due to mutating `X.assign` within the module body, which occurs during
module creation. Every application of `M` causes a mutation effect on type `SX&`. This effect could
be avoided as follows.

```hemlock
let make_M X
  : {|val f: uns >{ld}-> uns|}&
  = module
    let m& := 42
    let f u = u * m
    X.assign := (fun u -> m := u)
# val make_M: SX&! -> {|val f: uns >{ld} -> uns|}&

let M = make_M X

# Equivalent to the above, but avoids creating the make_M lexical binding.
let M = (fun X
  : {|val f: uns >{ld}-> uns|}&
  -> module
    let m& := 42
    let f u = u * m
    X.assign := (fun u -> m := u)
  ) X
# SX&! -> {|val f: uns >{ld} -> uns|}&
```

Module creation functions behave typically with regard to effects, but this fact is obscured by the
`let M : {|...|} = module ...` syntax if the programmer is unaware of its duality with the more
general function application syntax.
