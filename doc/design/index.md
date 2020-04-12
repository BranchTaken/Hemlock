# Hemlock

[Hemlock](https://github.com/BranchTaken/Hemlock) is a systems programming
language, but not all systems are alike.  Hemlock is intentionally constrained
to excel for a (large) subset of possible systems, at the expense of
unsuitability for some problems.  In particular, Hemlock relies on runtime
facilities that are not straightforward to implement in bare-metal environments
like operating system kernels.  The benefit is that outside such constrained
environments Hemlock provides a robust, elegant, high-performance foundation on
which applications can be confidently built.

Hemlock is a synthesis of existing programming language technology, yet it has
powerful emergent properties due to how its features combine.  For example,
mutability is precisely specified in the type system so that the programmer and
the compiler share a clear model of safe/viable optimizations.  This interacts
with automatic memory management to create the emergent property that GC is
trivial for pure code.  As another emergent property due to precise mutability
control, consider that Hemlock safely embeds nested data structures in many
common cases, which thanks to reduced dependent pointer chasing substantially
reduces the performance gap to careful manual memory layout that is possible in
languages like C.  Hemlock goes even further by providing data types like
bitwidth-specific integers and statically sized arrays which enable dense data
representations without requiring algorithmic abstraction compromises.

## Opinionated

Hemlock is distinctive in part due to strongly opinionated design principles.
Reasonable people may disagree in part or in whole, but these opinions
illuminate Hemlock's internal consistency:

- As mentioned above, Hemlock implements automatic memory management, aka
  garbage collection (GC).  GC is liberating and empowering far in excess of its
  potential for degenerate performance.  Allocation lifetimes may not be
  consistently related to associated computations, which means manual memory
  management requires disparate programming logic.  This incidental complexity
  eats into the intentional complexity budget.

- Programs that have transitioned to incorrect computation should fail
  immediately.  The contrary opinion holds that the program should defer
  termination, kick the can down the road, and continue onward as well as
  possible, with the optimistic hope that the error will dissipate or be
  properly resolved at a higher level.  This strategy has a critical flaw in
  that error tolerance commonly induces a combinatorial complexity burden that
  is nearly impossible to adequately test.  Hemlock takes an extreme position:
  actor execution *halts* at the first sign of incorrect computation, and the
  only mechanism for recovery is a supervisor actor spawning a new actor.  Note
  that this approach enables extreme fault tolerance, despite intolerance of
  incorrect execution.

- Parallel computation based on shared mutable state is so excruciatingly
  difficult to get right that it is not generally worth the trouble.  Hemlock
  does strongly support parallel computation, but only via asynchronous sharing
  of immutable data.

- Most (but not all!) computation can be clearly and efficiently expressed in
  terms of effectless computation on immutable data.  When practical, pure
  functional computation is preferable because [referential
  transparency](https://en.wikipedia.org/wiki/Referential_transparency)
  simplifies reasoning about the system.  However, programs typically need to
  interact with the outside world, and mutation is often the best way to model
  such interaction.  For that matter, some algorithms are far less efficient
  when written using only pure computation.  Mutation should be both explicit
  and well supported, so that there is a natural way to mutate where necessary,
  yet compartmentalize mutation such that its effects do not pervade the system
  as a whole.

- Compiler optimization heroics are anathema.  Optimization technology is quite
  mature, but in many cases heroics are required to successfully apply
  optimizations, because the language semantics are overly permissive,
  under-specified, and/or broken.  This is a bad situation both for the compiler
  and the programmer.  Optimization is hard, and the programmer can't have a
  clear mental model of what the compiler is capable of optimizing.  The Hemlock
  *compiler* does not aspire to be [sufficiently
  smart](https://wiki.c2.com/?SufficientlySmartCompiler) to prevail in a hostile
  environment; rather the Hemlock *language* aspires to be sufficiently explicit
  to allow the compiler and the programmer to share a model of computation that
  is both highly optimizable and straightforward to reason about.

- Complexity *abstraction* is an immensely powerful tool.  However, complexity
  *hiding* or *masquerading* is fraught with peril.  Operators like `+` should
  signify addition, not string catenation, not set union, not frobnification.
  Hemlock never implicitly converts between types, it minimizes what can happen
  in the cracks between expressions, and it generally strives to make
  semantically different code look different.

- Unsigned integers are preferable to signed integers unless negative numbers
  are part of the intended domain.  As such, Hemlock's default integer type is
  unsigned.

## Practical

As mentioned earlier, Hemlock is a synthesis of existing language technology;
its innovations are overwhelmingly engineering-based over science-based.  To a
first approximation, Hemlock is [OCaml](http://ocaml.org/), with
[Skip's](http://skiplang.com/) mutability control and
[Erlang's](https://erlang.org/) actor-based parallelism.  To a second
approximation, Hemlock is OCaml minus the "O", a unified parametric
effects/mutability/type system, plus an enhanced derivative of Erlang's
actor-based parallelism that supports incremental sharing of immutable
persistent data.  To a third approximation, add to that support for [modular
implicits](http://ocamllabs.io/doc/implicits.html) (a proposed OCaml
enhancement) to make the type profusion wieldy, and a work-stealing scheduler to
automatically parallelize execution.  If you are familiar with these concepts,
you now have a reasonably complete overview of Hemlock.

Hemlock is a mostly pure strict (i.e. not lazy) functional language.  Think of
the functional purity as an opt-out regime.  It may be surprising that Hemlock
was preceded by years of attempts at increasingly comprehensive opt-in
functional purity.  These attempts were almost entirely motivated by
high-performance computing, but the switch from opt-in to opt-out also brought
algorithmic clarity and program reliability benefits to Hemlock.

## Design

- [Types](types.md)
- [Effects and mutability](effects_mutability.md)
- [XXX Actors](actors.md)
- [XXX Syntax](syntax.md)
- [XXX Compiler](compiler.md)
- [Runtime](runtime.md)
- [XXX Documentation](documentation.md)
- [XXX Language server](language_server.md)
