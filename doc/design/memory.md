# Memory management

Hemlock depends on automatic memory management, also known as garbage collection
(GC).  The memory management subsystem implements precise tracking of memory
references, and its GC algorithms are capable of collecting all unreachable
data, given sufficient time.

Hemlock's memory management subsystem has to support parallel execution of as
many runtime executors as there are CPUs, and on the order of millions of actors
multiplexed onto those executors.  The GC algorithms employed by Hemlock (and
certain design choices in the language itself) are chosen such that GC overhead
typically increases in proportion to how much computation the application
performs.  Were internal system memory bandwidth requirements to increase
non-linearly, the memory subsystem would saturate well before adding
CPU-executor pairs would otherwise hit diminishing throughput returns.

Actor computation is conceptually single-threaded, with the exception of
tree-structured parallel computation, which is described below.  Most actor data
are immutable, but (possibly cyclic) mutable data are well supported.  No
synchronization primitives are required during actor execution (ignoring mailbox
synchronization, write barriers which order global value refcounting, etc.),
because the executor is the only other context in which actor data can be
accessed, and executor/actor execution are inherently serialized because they
share an operating system thread.  Each actor has a local heap which is
independent of all other actors.

Regarding tree-structured parallel computation, some code constructs permit
parallel execution on input data which the computation does not modify.  In such
cases the runtime may optionally create work stealing opportunities for other
executors.  If an executor steals work, it creates an ephemeral actor with its
heap layered on top of that of the actor from which work is being stolen.  The
underlying actor's heap is not modified for the duration of the parallel
computation; upon completion the result is written to the underlying actor's
heap where the result would have been written had the work never been stolen.
The net effect is parallel computation on a shared heap, but with such strict
limits on mutation that the results are indistinguishable from single-threaded
computation.  This is the only exception to the rule that actor heaps are
independent.

Actors collaborate by sending and receiving immutable message data, optionally
with references to global heap values.  The reference graph induced by immutable
data is a directed acyclic graph (DAG); it is impossible to create immutable
cycles in Hemlock.  Global GC is greatly simplified by the combination of
immutability and acyclicity.

Process memory is laid out as shown in the following diagram to facilitate
efficient implementation, in particular:

- Code and literal data reside low in memory, ideally below 4 GiB, so that
  addresses can be compactly encoded in machine code.
- All global data reside below all local data, so that global and local values
  can be bipartitioned using a single address comparison.  This is especially
  important to efficient GC.

```
/-------------\ address 0xffff_ffff_ffff_ffff
|             |
|             |
|             |
| local heaps |
|             |
|             |
|             |
|-------------|
|             |
|             |
|             |
| global heap |
|             |
|             |
|             |
|-------------|
|    data     |
|-------------|
|    code     |
|-------------|
|    ...      |
\-------------/ address 0x0000_0000_0000_0000
```

## Data/memory model

Memory is managed as pages of naturally aligned 64-bit words.  Although
individual integral values may have sizes which are not multiples of 64 bits,
and such values can be contiguously packed within an outer value (e.g. a `u8
array`), the memory manager always operates on values which are whole words.

Hemlock strongly favors contiguous allocation over segregated allocation; it
copies/compacts rather than reusing freed interspersed memory.  Although some
non-moving allocators such as [jemalloc](http://jemalloc.net/) are typically
capable of adequately limiting [external
fragmentation](https://en.wikipedia.org/wiki/Fragmentation_(computing)), their
effectiveness is extremely dependent on 4 KiB virtual memory pages.  CPUs use
translation lookaside buffers
([TLBs](https://en.wikipedia.org/wiki/Translation_lookaside_buffer)) to cache
mappings between virtual and physical addresses, but the number of entries in a
TLB is severely constrained by very large scale integration
([VLSI](https://en.wikipedia.org/wiki/Very_Large_Scale_Integration)) engineering
considerations.  Because the TLB for 4 KiB pages cannot be made much larger,
modern CPUs have an additional TLB for "huge" 2 MiB pages.  Large-memory
high-throughput applications starve for data unless they maintain high data
locality at the same time as heavily utilizing huge pages.

All Hemlock program types are implemented in terms of self-identifying typed
values.  Hemlock's execution model does not require run-time type information
([RTTI](https://en.wikipedia.org/wiki/Run-time_type_information)), but RTTI is
used by the runtime itself to enable efficient memory layout in conjunction with
GC (and polymorphic code).

Values can be physically nested.  Every outer value is independently allocated
and can contain an arbitrarily deep nesting of inner values, but outer values do
not nest.  The outer/inner qualification is typically left unspecified except
when contextually significant.

Every outer value has a metadata header, followed by one or more words of data.
The GC can introspect header metadata to distinguish the data words.  Although
type metadata provide additional detail, only the following data/metadata
distinctions matter to the GC:

- Nested metadata associated with inner values.
- Reference to another value.
- Non-reference data.

Inner values may or may not have metadata headers, depending on type
declarations.  Record fields and array elements may be any of the following, the
first two of which are relevant to inner value metadata:

- `val`: Nested value with no metadata header.  All type metadata are integrated
  into the outer type's metadata.  The value is not independently
  self-identifying, which precludes interior references to the value (copy-in,
  copy-out).
- `box`: Nested value with metadata header.  Type metadata are directly
  available in the box's header, as well as indirectly via the outer value's
  header.
- `ref`: Reference to separately allocated value (untagged pointer).
- `data`: Arbitrary data.

The least significant bits encode `ipw` to distinguish metadata header words:

- **i:** Inner/outer value.
  - 0: Outer value.
  - 1: Inner value.
- **p:** Forwarding pointer?
  - 0: Normal value.
  - 1: Forwarding pointer written on top of metadata header.  Normal header is
    now in tospace along with a complete copy of the value.
- **w:** Header word index.
  - 0: First word, not always present.
  - 1: Second word, adjacent to value data, always present.

Metadata headers for outer values are one word for values 15 words or smaller,
two words otherwise.  Compact outer value headers are intended for the fast-path
common case, and therefore they directly contain all metadata required by the
GC, even if the metadata could be derived from just the type identifier.
```
Compact outer value:                                                  ipw

  rrrrrrrr rrrrrrrd aatttttt tttttttt tttttttt tttttttt tttttmcc czzzz001

Large outer value:

  zzzzzzzz zzzzzzzz zzzzzzzz zzzzzzzz zzzzzzzz zzzzzzzz zzzzzzzz zzzzz000
  ........ ....aaaa aatttttt tttttttt tttttttt tttttttt tttttmcc c0000001

Inner value:

  hhhhhhhh hhhhhhhh hhtttttt tttttttt tttttttt tttttttt tttttmcc czzzz101

Compact forwarding pointer:

  ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff fffff011

Large forwarding pointer:

  ........ ........ ........ ........ ........ ........ ........ .....000
  ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff fffff011
```
- **r:** Bitmap of which words within the value are references.
- **d:** If true, dynamically determine the reference map rather than using the
  reference bitmap.
- **t:** Type identifier, with which type metadata can be looked up.
- **m:** Transitively mutable type if 1, transitively immutable type otherwise.
- **c:** Cohort if in the minor heap, white/marked GC state if in the major
  heap.  White/marked state is redundant with respect to the mark table, but is
  simpler to access during marking and amortizes repeated marking of the same
  value.
- **z:** Size.
  - 0x0: Size exceeds 15 words.
  - [0x1..0xf]: Size in words of value.
- **a:** Biased lg alignment, [8, 16, 32, ..., 2<sup>62</sup>]
  (lg(alignment)-3).  Only [8, 16, 32, 64] are encodable in compact headers, but
  this covers all practical use cases, given the limited value size (≤ 120
  bytes).
- **h:** Non-zero unsigned (negative) outer value header offset in words, 0 if
  offset is too large to encode (in which case the crossing table is used to
  find the value header).
- **f:** Forwarding pointer, used only for local heap copying.
- **.:** Unused.

## Global heap management

Many aspects of global heap management rely on a global monotonic clock.
Precise wall time is not important, but the relative ordering of times is
critical, because "happens before" relationships provide the foundation on which
to build self-consistent and safe (i.e. no dangling/corrupt pointers)
time-dependent views into the global heap.  The following definitions are
integral:

- **Time:** Time as measured by a global monotonic clock.  Time passes, and
  individual timestamps are records of time at the moment of clock observation.
- **Present:** Time at least as advanced as any time observed thus far.
- **Future:** Time more advanced than any time observed thus far.
- **Exposure:** Contiguous time span, `[start..end]`, during which observations
  of heap state may have occurred.  As a concrete example of exposure, consider
  that an actor's exposure ends at the time it most recently sent or received a
  message.  Two total orderings are defined on exposures:
  - Start-ordered: An exposure with earlier start time precedes an exposure with
    later start time, as does an exposure which contains a shorter exposure with
    equal start time.
  - End-ordered: An exposure with earlier end time precedes an exposure with
    later end time, as does an exposure which is contained by a longer exposure
    with equal end time.

  Set relations are defined on exposures as if each exposure were a contiguous
  ordered set of times.  The two total orderings can be used together to
  efficiently compute set relations on exposure sets.  Disjoint exposures are of
  primary interest, because they are the basis of "happens before"
  relationships.  Note that given a set of exposures, if an exposure is disjoint
  from its immediate successor in the start ordering, and from its immediate
  predecessor in the end ordering, it is disjoint from *all* exposures in the
  set.  In the following figure, E is the only exposure disjoint from all
  others.

  ```
  --> time
    Start-ordered           |   End-ordered
  --------------------------+--------------------------
  A |-----|                 | C   |---|
  B   |------|              | A |-----|
  C   |---|                 | B   |------|
  D         |----|          | D         |----|
  E               |-|       | E               |-|
  F                  |----| | G                   |--|
  G                   |--|  | F                  |----|
  ```

- **Advancement:** Actors and spaces are capable of advancing their exposures to
  `[present..advance_finish]`, i.e. a span of time starting at present and
  ending as soon as advancement finishes.  Advancement is the key operation
  which makes it possible to exposure-isolate obsolete data and destroy them.
  In the following figure, `space1` is obsolete, but `space2` and all the actors
  have transitively intersecting exposure, as described later, and they must all
  advance before `space1` can be destroyed.

  ```
  --> time                                  present
                                               |
                                               v
  |-gen0-----|
              |-gen1---------|
                              |-gen2-----------|
               |-space0'-|
              |-space1-------|    |-space1'-|
                              |-space2---------|
                          |-actor0----|
                          |-actor1----------|
                                    |-actor2---|
  ```

The global heap is independent of the local heaps, with the critical caveat that
the global root set is contained in the local heaps.  The details of how local
heaps manage global roots is covered in the [Local Heap
Management](#local-heap-management) section; from the global heap's perspective
it is important to know only a few details:

- Each local heap maintains its global root set such that root
  additions/removals can only occur as side effects of message send/receive or
  local GC.
- Each local heap tracks its exposure.
- An actor can be compelled to advance its global root set to refer to the
  newest available copies of globals.

### Executor heaps

Although there is conceptually a single global heap, it is actually implemented
as the union of per executor heaps, which in turn are ordered sets of
generations, in each of which resides one or more spaces.  The youngest
generation in each executor heap is a nursery except during transitions as one
nursery space fills and a new nursery is created.  The nursery is the only place
in each executor heap to which new values are written, with the exception of
values created by forced lazy suspensions; this exception is described in more
detail later, and can typically be ignored.

```
/----------------- Global heap ------------------\
|                                                |
|  /--- Exec 0 heap ---\  /--- Exec 1 heap ---\  |  ^ increasing address
|  |                   |  |                   |  |  |
|  |  /--- Gen 0 ---\  |  |  /--- Gen 0 ---\  |  |  |
|  |  | space       |  |  |  | fromspace   |  |  |
|  |  |             |  |  |  |  |          |  |  |
|  |  |             |  |  |  |  v          |  |  |
|  |  |             |  |  |  | tospace     |  |  |
|  |  \-------------/  |  |  \-------------/  |  |
|  |                   |  |                   |  |
|  |  /--- Gen 1 ---\  |  |  /--- Gen 1 ---\  |  |
|  |  | fromspace   |  |  |  | space       |  |  |
|  |  |  |          |  |  |  |             |  |  |
|  |  |  v          |  |  |  |             |  |  |
|  |  | tospace     |  |  |  |             |  |  |
|  |  \-------------/  |  |  \-------------/  |  |
|  |                   |  |                   |  |
|  |   .............   |  |   .............   |  |
|  |                   |  |                   |  |
|  |  /-- Nursery --\  |  |  /-- Nursery --\  |  |
|  |  | space       |  |  |  | space       |  |  |
|  |  \-------------/  |  |  \-------------/  |  |  |
|  |                   |  |                   |  |  |
|  \-------------------/  \-------------------/  |  v younger values
|                                                |
\------------------------------------------------/
```

Value allocation order is always preserved within each space.  The spaces within
each executor's generations can be thought of as a single contiguous heap for
the purposes of value ordering.  Spaces exist only so that they can be
copy-collected independently of each other.  Space copying incurs cost
proportional to space size, regardless of global heap size.  Per space
utilization data drive incremental space copy-collection such that young values
are typically collected much more often than old values, thus exploiting the
generational hypothesis.

### Space copy-collection

Space exposure is defined as the time span during which values are written to
the space.  Generation exposure is fixed as that of the nursery space which
originally constituted the generation; subsequent copy-collection and/or
advancement results in spaces of more advanced exposure, but the generation's
exposure remains fixed.

The young->old value reference DAG induces a generation DAG with similar
structure, and the generation DAG guarantees that younger generations cannot be
referred to by exposure-disjoint older generations.  The generation DAG allows
young generations to be copy-collected with complete disregard for most of the
global heap.  (Note that within each executor heap, all generations are
exposure-disjoint, but this does not necessarily hold true for generations
across multiple executor heaps.  When we refer to dependent generations, we mean
younger generations *and* any such exposure-intersecting generations.)

Once a fromspace->tospace copy is complete, the fromspace is obsolete and
contains only redundant data.  However, the fromspace cannot be destroyed until
its exposure is disjoint from all spaces within dependent generations, and its
exposure is disjoint from all actors' exposures.  We leave forwarding pointers
in fromspaces, so that it is possible for dependent spaces and local heaps to
rewrite their references and thereby advance their exposures.  For any given
reference, exposure can be trivially advanced by following the value's
forwarding pointer, if any.  To advance a set of references, advance them all
individually; upon completion the set's exposure starts at the time advancement
was initiated, and ends at the present.

A key aspect of limited-overhead exposure advance is to prohibit operations
which would expand exposure backward in time.  An actor's exposure is expanded
to incorporate the exposure of any message it receives.  Were messages to be
allowed arbitrary exposure, message receipt could arbitrarily expand actors'
exposures backwards, and it would be infeasible to advance all actors' exposures
without temporarily suspending all messaging.  Therefore we transitively advance
the exposure of message payloads during message creation.

Given that exposure only expands forward in time, the following steps are
sufficient to make an obsolete fromspace's exposure disjoint:

- Advance all exposure-intersecting spaces associated with dependent
  generations.  Compute the union of these spaces' exposures (truncated at the
  obsolete fromspace's exposure start time) to determine transitive exposure to
  the obsolete fromspace, as seen from the actors' perspective.  Note that
  nursery spaces may continue to be written to in parallel during advancement,
  but since all newly written values are already advanced, they can be ignored
  if they are written after space advancement begins.
- Advance all actors with intersecting exposure to the transitive dependent
  space exposure.  Note that isolated actors may have disjoint exposure
  preceding the fromspace's exposure.  At the other end, active actors that had
  intersecting exposure may have already advanced, whether as a side effect
  major local collection or due to having advanced to enable destruction of some
  other obsolete fromspace.  We prefer to delay actor advancement for as long as
  possible, since actor advancement is often a side effect of actor execution,
  and even when not, the later the advancement the better.

### Space layout

```
^ increasing address
|
|

/--- Space ----\. . . . . /--------------------------------------------\
|   metadata   |          | exposure: Time exposure.                   |
|--------------|          | fr/to: Fromspace/tospace pointers.         |
|xxxxxxxxxxxxxx|.         | gen: Pointer to containing generation.     |
|xxxxxxxxxxxxxx| .        | lazy: List of spaces containing values     |
|xxx values xxx|  .       |       created by lazy suspensions.         |
|xxxxxxxxxxxxxx|   ./-----| frontier: Pointer to boundary between      |
|xxxxxxxxxxxxxx|<--/.     |           allocated values and wilderness. |
|              |     .    | unreachable: Stack of deallocated values   |
|              |      .   |              that still retain references. |
|  wilderness  |       .  | size: Space size, excluding metadata.      |
|              |        . | allocated: Number of bytes currently       |
|              |         .|            allocated.                      |
\--------------/          \--------------------------------------------/
```

The above diagram depicts an individual space.  The same space data structure
serves for the entire lifetime of a space, whether it is a nursery, fromspace,
tospace, or lazy space.

- The `exposure` is initialized to `[present..future]` when the first value is
  allocated in the space, and the end time is advanced to the present when the
  last value is allocated in the space.  The resulting exposure encompasses all
  allocation times.
- A non-`NULL` `to` pointer to the associated tospace indicates the space is a
  fromspace; a non-`NULL` `fr` pointer to the associated fromspace indicates the
  space is a tospace.
- A non-`NULL` `lazy` pointer indicates one or more lazy suspensions stored in
  the space were forced, and the resulting values are stored in the lazy spaces.
  These values are considered to be the same ages as the suspensions which
  generated them, and they are post-order recursively copied into place with
  their creators during copy-collection in order to preserve the value ordering
  invariant.
- The `frontier` pointer demarcates the boundary between values and wilderness.
  Total bytes allocated is the difference between the metadata base address and
  `frontier`.
- The `unreachable` stack of deallocated values is used for incremental
  decrefing.  Threading a per space stack through unreachable deallocated values
  allows us to deallocate one value at a time and incrementally perform
  cascading decrefs.
- The `allocated` number of bytes is increased to incorporate newly allocated
  values at the same time as `frontier` is bumped, and it is decreased as values
  are deallocated as a side effect of decrefing.  If `allocated` drops to 0 and
  the space has no associated fromspace, it can be destroyed as soon as its
  unreachable stack is empty.  In practice this seldom occurs because
  copy-collection commences well before the space becomes empty.
- The space is capable of containing `size` bytes of value data.  Space
  utilization is `allocated / size`; copy-collection commences when utilization
  drops below a chosen threshold.

### Global value layout

Global and local values have the same layout, except that global value headers
are prefixed with an additional word for refcounting metadata.  Following are
the valid states for the global value header word.

Global value header bit encodings:

- **c:** Reference count.
- **z:** Finalizable if 1.  The finalizer function is a low-level runtime
  function associated with the type, and resurrection is impossible.  Immutable
  value finalization is intended primarily for runtime implementation of file
  descriptor management and other similar use cases, rather than as a Hemlock
  language facility.
- **s:** Non-`NULL` per space `unreachable` stack linkage.
- **f:** Forwarding pointer, overlaid with reference count so that the
  transition from refcounting to forwarding is atomic.

**Global value header word:**

```
Non-zero refcount:

  cccccccc cccccccc cccccccc cccccccc cccccccc cccccccc cccccccc ccccc0z1

Forwarding pointer:

  ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff fffff000

Zero refcount, deallocated, in space's unreachable value stack:

  ssssssss ssssssss ssssssss ssssssss ssssssss ssssssss ssssssss sssss0z0

Zero refcount, deallocated, decrefing complete:

  00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000
```

### Space metadata lookup

As described above, various space metadata updates occur during deallocation and
lazy suspension execution.  This requires a function that returns a pointer to
the associated space metadata, given a value pointer.  A global radix tree
maintains adequate state to perform this operation.  In order to limit radix
tree overhead, spaces conform to special size and alignment constraints.  The
space allocation quantum is a chosen power of 2, e.g. 2 MiB (2<sup>21</sup>).
All spaces are sized as a whole multiple of the quantum, and they are aligned to
a quantum boundary.  This allows the radix tree to ignore the least significant
lg(quantum) address bits, and each space typically only requires one entry in
the radix tree.  (The extremely unusual exception is for a multi-quantum space
that contains a huge value, and interior references to that value allow
references to span multiple quanta.)

### Generation coalescing

The nursery generation grows until its space cannot serve an allocation request,
at which point it becomes an aging generation.  Aside from allocation via lazy
suspension execution, a generation cannot grow after it transitions to aging.
Meanwhile new generations continue to stack up as nurseries transition to aging.
As generations age, they tend to shrink, but in many cases they will linger due
to long-lived allocations.  We coalesce adjacent aging generations once they
shrink below sizes that allow two generations to fit in the space that one
generation originally required, and their combined utilization drops below the
copy-collection threshold.  This assures that the generation count remains a
function of global heap size, rather than growing indefinitely as a function of
global allocation volume.

### Incremental work

Global collection is a continuous process enabled by various incremental work,
some explicit, and some that requires intentional asynchronous scheduling.  As
an example of explicit work, recall that value allocation (a side effect of
message send) requires the payload to be transitively advanced.  Asynchronous
incremental work can be hooked to various events, especially:

- Nursery creation
- Actor switch
- Value allocation (message send)
- Value deallocation

There are several types of incremental work:

- Process an element in an `unreachable` value stack.
- Copy a value fromspace->tospace.
- Advance an actor.
- Advance a generation's space(s).

There is no obvious fundamental principle on which to base pace of advancement,
except perhaps nursery creation rate and/or impending memory exhaustion.  On the
other hand, decrefing and copying can be driven at a rate proportional to
allocation rate that is sufficient to guarantee this work does not fall
hopelessly behind allocation.

Copying is triggered at a fixed utilization threshold, so the mean number of
times a value can be copied is fixed.  For example, if minimum utilization is
0.5, the total number of bytes copied per byte allocated is approximately the
integral of 0.5<sup>x</sup> in the range 0..∞, which is ~1.44.  Therefore the
rate at which decref and copying work must be performed depends on the minimum
utilization, but is independent of current heap size.  The above
allocation-to-copying relationship is bounded by a much simpler relationship
that is trivial to compute, and sufficiently close to be practical: 1/(1-u).

## Local heap management

Each actor is managed via a fixed-size actor context (`actx`), which contains
critical data structures, such as unique identifier, saved register state when
not executing, and pointers to local heap structures.  While an actor is
executing, a dedicated machine register maintains a pointer to the `actx`, which
makes all heap data structures readily accessible.

Each actor has its own local heap which contains all of the actor's data
structures, including its dedicated execution stack (which is traditionally
considered distinct from the heap).  The local heap is initialized with a fixed
layout at actor creation time, and that layout imposes an immutable size limit.
The fixed heap size limit is not an onerous constraint since Hemlock only
targets 64-bit platforms, yet the finality of the maximum heap size makes
operational considerations regarding heap growth and out-of-memory (OOM)
conditions simpler to reason about and provision for.

The local heap primarily comprises a major heap and a minor heap, which together
support generational GC.  The major heap is collected using the
[Compressor](https://dl.acm.org/doi/10.1145/1133255.1134023) incremental
mark-compact algorithm.  The minor heap is collected using
[Cheney](https://dl.acm.org/doi/10.1145/362790.362798)-style atomic single-pass
semispace copying, with cohort aging to delay promotion from the minor heap.

Each heap comprises nine virtual memory regions, each of which has size equal to
the heap size limit.  This layout ensures that once the local heap is
initialized, given enough physical memory the heap size limit can be reliably
reached (or exceeded by a constant factor of less than six) regardless of the
memory usage ratio between major/minor heaps, execution stack, and mailbox.  The
following figure logically depicts these regions side by side; their actual
relative positioning is detailed later.

```
                              /-----------------------\
                             /     Metadata tables     \
/---------+---------+-------+---+---+---+---+---+---+---+---------+---------+---------\
| Major A | Major B | actx  | G | C | C | M | R | F | S | Minor A | Minor B | Mailbox |
| |  :  | | |  :  | |-------| r | a | r | a | e | i | t |    |    |    |    |    |    |
| |  :  | | |  :  | | Stack | a | r | o | r | l | r | a |    |    |    |    |    |    |
| v  :  v | v  :  v |   |   | y | d | s | k | o | s | t |    v    |    v    |    v    |
|    :    |    :    |   |   | / |   | s |   | c | t | u |         |         |         |
|    :    |    :    |   v   | m |   | i | b | a |   | s |         |         |         |
|    :    |    :    |       | o |   | n | i | t | v |   |         |         |         |
|    :    |    :    |       | v |   | g | t | i | a |   |         |         |         |
|    :    |    :    |       | e |   |   |   | o | l |   |         |         |         |
|    :    |    :    |       |   |   |   |   | n | u |   |         |         |         |
|    :    |    :    |       | s |   |   |   |   | e |   |         |         |         |
|    :    |    :    |       | t |   |   |   |   |   |   |         |         |         |
|    :    |    :    |       | a |   |   |   |   |   |   |         |         |         |
|    :    |    :    |       | c |   |   |   |   |   |   |         |         |         |
|    :    |    :    |       | k |   |   |   |   |   |   |         |         |         |
|    :    |    :    |       |   |   |   |   |   |   |   |         |         |         |
|    :    |    :    |       |   |   |   |   |   |   |   |         |         |         |
\---------+---------+-------+---+---+---+---+---+---+---+---------+---------+---------/
```

The metadata stack/table overheads relative to the actor memory limit sum to
less than 1, and are as follows:

- Gray/move stack: 2<sup>-1</sup>
- Card tables: 2(2<sup>-10</sup> + 2<sup>-20</sup> + 2<sup>-30</sup>)
- Crossing table: 2<sup>-10</sup>
- Mark bit table: 2<sup>-6</sup>
- Relocation table: 2<sup>-7</sup>
- First value table: 2<sup>-18</sup>
- Compaction status table: 2<sup>-24</sup>

Regarding the actor memory limit and the maximum physical memory that can
possibly be consumed, the hypothetical maximum physical memory usage is less
than six times the memory limit.  The actor memory limit should be thought of as
a failsafe mechanism for keeping runaway actors from taking the whole process
down, rather than as a quota system.  The interactions between subsystems are
non-linear, but with that caveat in mind, the following enumerates worst case
(maximum) physical memory usage for semi-independent actor heap regions:

- The major heap comprises four distinct memory-limit-sized virtual memory
  regions, but total maximum instantaneous physical memory usage for the major
  heap is incrementally (~2 MiB) more than the actor memory limit.
- The execution stack can in principle consume nearly as much physical memory as
  the actor memory limit, but in practice the execution stack is small unless
  there is an application error which triggers infinite recursion.
- The metadata tables use physical memory proportional to the peak major heap
  memory usage.  Their total physical memory usage sums to less than that of the
  major heap.
- The minor heap may in principle have instantaneous physical memory usage that
  is twice that of the memory limit, but in practice, the minor heap typically
  utilizes at most a few MiB of physical memory, except when young large
  monolithic data structures exist.
- The mailbox may in principle consume as much physical memory as the actor
  memory limit.  However, actors in well-behaved programs rarely have mailboxes
  of significant size.

The major heap semispaces are double-mapped so that access by the application
can be disabled via page protection on a per page basis during incremental
compaction, and the runtime can initialize pages on demand (i.e. a `SIGSEGV`
signal) using the secondary mapping, until incremental compaction completes and
all pages are again accessible to the application.  The major heap compacts when
it copies, and preservation of allocation order allows fromspace pages to be
incrementally deallocated when all their reachable values have been copied to
the tospace.  Therefore the peak major heap memory usage is approximately
bounded by the heap size limit.

The `actx` and the machine stack reside below the major heap semispaces, and
above the minor heap semispaces.  The `actx` must be interposed between the
major/minor semispaces so that comparison between a value address and the `actx`
address determines whether the value is in the major versus minor heap; all
other ordering between heap data structures is inconsequential.  The write
barrier must distinguish major versus minor values, and because the write
barrier is inline machine code generated at every mutation code site, it must be
compact and fast.

As mentioned above, the execution stack is allowed to reach, but not exceed, the
entire virtual memory limit assigned to it.  It is critical to application
stability that Hemlock not allow the execution stack to overflow, lest other
actor data be overwritten.  The simplest practical (if imperfect) way to prevent
stack overflow is to place an inaccessible stack guard past the maximum extent
of the stack, such that any attempt to read or write past the limit triggers a
`SIGSEGV` signal.  However, Hemlock purports to support millions of actors, and
every stack guard is a distinct virtual memory area (VMA), which is
unfortunately a precious operating system resource in some cases.  Linux in
particular allows a maximum of 65530 VMAs by default.  This presents a conundrum
for Hemlock.  One possibility would be to halt the process if the VMA limit is
reached, and require the user to tune the VMA limit as needed.  However, Hemlock
takes a more resilient approach that takes advantage of the VMA limit without
hard-failing if the limit is approached:

- Query the initial number of VMAs required by the process prior to creating
  actor heaps, and assume a fudge factor for VMAs outside Hemlock's direct
  control (created by e.g. `dlopen(3)`).
- Track the current number of VMAs under Hemlock's management.
- When creating a new actor heap, interpose a stack guard only if the VMA limit
  has not been reached.
- If a newly scheduled actor does not have a guard VMA, before allowing it to
  execute scavenge one or more VMAs from least recently run actors by removing
  their guard VMAs, and create a guard VMA for the scheduled actor.

The various tables map metadata to the major heaps.  The gray/move stack is
sized proportionally to the heap size such that it is always large enough for
all use cases, but it does not linearly map metadata to the major heaps as the
tables do.

Only one of the minor heap semispaces is in use at any given time, except for
the brief times during which minor GC is occurring.  During such times, only the
fromspace counts toward the heap size limit, therefore instantaneous memory
usage for the minor heap can be approximately twice as much as the heap size
limit.  This is an intentional design decision that has various advantages, e.g.
the ability to allocate a single value that exceeds half of the heap size limit.
However, it does add some nuance to heap size limit tuning.

The mailbox contains unread messages.  When a message is read, it is moved to
the minor heap.  Messages are always read in the order they are received, so in
the common case both message allocation and deallocation are little more than
atomic head and tail pointer adjustments, respectively.  If the mailbox empties,
its frontier is re-initialized to the beginning (top) of the virtual memory
range.  In the unlikely event that a message cannot be delivered because the end
of the virtual memory range is too close, the mailbox contents are moved en
masse.  Any subsequent failure is due to exceeding the heap size limit.

Message sends occur in a critical section.  In other words, once a message
sender allocates space in the recipient's heap, the sender cannot be preempted
until the message is fully written.  The critical section prevents long-lived
mailbox holes, which allows the recipient to busy-read partial incoming messages
without danger of livelock.

Immediately before an actor first starts executing, the entirety of the heap is
uninitialized, though some of the tables (card, mark bit, compaction status)
need to be initialized with zeros if/when first used.  The simple approach is to
assure that all memory is zeroed upon heap creation, though optimizations are
possible, and may eventually be warranted to minimize overhead for short-lived
actors.

The virtual memory for an actor is in three disjoint address ranges.  This
unusual layout enables double mapping with only two distinct virtual memory
areas (VMAs).  Furthermore, the executor allocates such VMA pairs in
exponentially increasing sizes and sub-allocates the actor heaps, so that even
with millions of actors, the total number of VMAs is modest.  In the following
figure, four local heaps are suballocated from one VMA pair.  The major heaps
are separate from the other data so that only they are double mapped.

```
  /-------------\
0 |      :      | \
0 |      :      |  \
1 | Major heaps |   |
1 |      :      |    > Secondary mapping (partial)
2 |xxxxxxxxxxxxx|   |
2 |xxxxxxxxxxxxx|   |
3 |      :      |  /
3 |      :      | /
  |-------------|
0 |      :      | \
0 |      :      |  \
1 | Major heaps |   |
1 |      :      |   |
2 |xxxxxxxxxxxxx|   |
2 |xxxxxxxxxxxxx|   |
3 |      :      |   |
3 |      :      |   |
  |-------------|   |
0 |      :      |   |
0 |      :      |   |
0 | actx/Stack  |   |
0 | Tables      |   |
0 | Minor heaps |   |
1 | Mailboxes   |    > Primary mapping
1 |      |      |   |
1 |      :      |   |
1 |      :      |   |
1 |      :      |   |
2 |xxxxxxxxxxxxx|   |
2 |xxxxxxxxxxxxx|   |
2 |xxxxxxxxxxxxx|   |
2 |xxxxxxxxxxxxx|   |
2 |xxxxxxxxxxxxx|   |
3 |      :      |   |
3 |      :      |   |
3 |      :      |   |
3 |      :      |  /
3 |      :      | /
  \-------------/
```

### Tri-color value segregation

Hemlock's GC uses the standard tri-color nomenclature for distinguishing between
values during tracing, with the following meanings:

- **black:** Reached, references traced
- **gray:** Reached, references untraced
- **white:** Unreached

Upon completion of tracing, no gray values remain, black values are reachable,
and white values are unreachable, i.e. garbage.

### Write barrier

If the local heap were atomically GCed, there would be no need for a write
barrier.  However, Hemlock's local GC is incremental in two distinct ways that
require write barrier cooperation:

- The minor heap is often GCed independently of the major heap, but the major
  heap may contain major->minor references that must be gathered as roots during
  minor GC.  Scanning the entire major heap for minor heap roots would often be
  cost-prohibitive, so Hemlock uses a three-level card table to narrow down
  which regions of the major heap may contain roots.  Every time a major->minor
  reference is written to the major heap, the corresponding card is "dirtied" so
  that the card is scanned for roots and summarized during the next minor GC.
- The major heap is marked incrementally, i.e. marking is interleaved with
  application code execution.  As a result the major heap reference graph can
  change over the course of marking.  No correctness issues arise if values are
  marked, and then become unreachable; this just causes retention of unreachable
  values that will be collected during the next major GC.  However, failure to
  mark all live values is a critical failure.  If both of the following
  conditions result from reference graph mutation, we fail to mark all reachable
  values:

  1. A black->white reference is created.
  2. The last gray->white*->white path is destroyed.  (The white chain can be
     arbitrarily long.)

  Hemlock uses a deletion barrier to prevent (2).  This is commonly referred to
  as maintaining the "weak tri-color invariant".  The deletion barrier marks the
  target of a major->major reference prior to mutation.

As mentioned earlier, `actx` is strategically placed between the major and minor
heaps in order to facilitate the write barrier.  Following is an approximation
of the write barrier logic which provides the necessary GC support.

```c
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

typedef uint64_t hl_word_t;

typedef struct {
    // ...
} hl_actx_t;

void
major_mark(hl_actx_t *actx, const hl_word_t *val);
void
card_dirty(hl_actx_t *actx, const hl_word_t *val);

inline bool __attribute__ ((always_inline))
is_major(const hl_actx_t *actx, const hl_word_t *val) {
    return ((uintptr_t)val > (uintptr_t)actx);
}

void __attribute__ ((noinline))
write_barrier_slow(hl_actx_t *actx, const hl_word_t *fr, const hl_word_t *to) {
    if (is_major(actx, fr)) {
        if (is_major(actx, to)) {
            major_mark(actx, to);
        } else {
            card_dirty(actx, fr);
        }
    }
}

void
write_barrier(hl_actx_t *actx, const hl_word_t *fr, const hl_word_t *to) {
    if (is_major(actx, fr)) {
        write_barrier_slow(actx, fr, to);
    }
}
```

As written, the inline write barrier compiles to ~9 bytes of
[x64](https://en.wikipedia.org/wiki/X86-64) machine code.  Were there no
deletion barrier, `card_dirty()` might better be called unconditionally, in
which case the write barrier would be ~15 bytes.

### Minor heap

The minor heap is logically organized into similarly aged cohorts of
allocations, and each cohort is promoted to the major heap after sufficient
aging.  The effect of adding cohorts is similar to adding generations, but with
lower algorithmic complexity at the cost of increased value copying.  The
following definitions are used to describe the algorithm:

- **cycle:** One round of bump allocation followed by copy-collection.
- **epoch:** One or more cycles.  In the implementation there is only one cycle
  per epoch, but that is not a requirement of the algorithm.
- **cohort:** An accreted set of values which are allocated during the same
  epoch.
- **nursery:** The currently accreting cohort.
- **aging:** A cohort which originated in an earlier epoch.  The implementation
  supports a maximum of 7 aging cohorts, numbered [n, n-1, n-2, ..., 0], where
  cohort n+1 is the nursery cohort.  Every time a cohort is copied, the tospace
  cohort's number is one less than that of the fromspace cohort.

The number of cycles per epoch can in principle vary independently of the number
of aging cohorts.  The following table shows how many times a value must be
copied prior to promotion to the major heap, as a function of cycles per epoch
(CPE) versus cohort aging length (CAL), computed as [CPE\*(CAL-1)+1 ..
CPE\*CAL].  Hemlock only implements CPE=1, because it minimizes the per cohort
copying variation, yet it provides adequate promotion delay.

```
   \ CAL
CPE \     1          2          3            4            5            6            7
-----\------------------------------------------------------------------------------------
   1 | [1 .. 1] | [2 .. 2] | [3 ..  3] | [ 4 ..  4] | [ 5 ..  5] | [ 6 ..  6] | [ 7 ..  7]
   2 | [1 .. 2] | [3 .. 4] | [5 ..  6] | [ 7 ..  8] | [ 9 .. 10] | [11 .. 12] | [13 .. 14]
   3 | [1 .. 3] | [4 .. 6] | [7 ..  9] | [10 .. 12] | [13 .. 15] | [16 .. 18] | [19 .. 21]
   4 | [1 .. 4] | [5 .. 8] | [9 .. 12] | [13 .. 16] | [17 .. 20] | [21 .. 24] | [25 .. 28]
```

The following figure depicts one epoch in the steady state (i.e. minor GC has
occurred enough times that all aging cohorts are populated).  Note in
particular:

The following figure depicts one epoch for CAL=3 in the steady state (i.e. minor
GC has occurred enough times that all aging cohorts are populated).  Note in
particular:

- Although the cohorts are depicted as segregated, this is an expositional
  fiction; the values' cohorts are stored in each header, which makes their
  relative order in the minor heap irrelevant.
- The 'aging 0' cohort is promoted out of the minor heap upon copying.
- The semispaces are resized (both increase and decrease are possible) upon
  completion of minor GC, in order to accommodate the aging cohorts and maintain
  the desired nursery size.  The nursery size is perhaps 2 MiB by default.
- Not shown: Immutable data in the 'aging 0' cohort are copied to the global
  heap rather than to the major local heap.  Thus the major heap contains *only*
  mutable data.

  ```
  --> time
  |- cycle ------------------>|- cycle ----->...
  |- epoch ------------------>|- epoch ----->...

  /-------------\             /-------------\
  |xxx major xxx|             |xxx major xxx|
  |xxxxxxxxxxxxx|      _______|xxxxxxxxxxxxx|
  \-------------/     /       |xxxxxxxxxxxxx|
                     /        |xxxxxxxxxxxxx|
                    / /------/\-------------/
                   / /
  /-------------\-/ / /-------/-------------\
  |xx aging 0 xx|  / /        |xx aging 0 xx|
  |xxxxxxxxxxxxx| / /         |xxxxxxxxxxxxx|
  |xxxxxxxxxxxxx|/ /    /-----|-------------|
  |-------------|_/    /      |xx aging 1 xx|
  |xx aging 1 xx|     /       |xxxxxxxxxxxxx|
  |xxxxxxxxxxxxx|    /        |xxxxxxxxxxxxx|
  |xxxxxxxxxxxxx|   /     /---|-------------|
  |xxxxxxxxxxxxx|  /     /    |xx aging 2 xx|
  |-------------|-/     /     |xxxxxxxxxxxxx|
  |xx aging 2 xx|      /      |xxxxxxxxxxxxx|
  |xxxxxxxxxxxxx|     /       |xxxxxxxxxxxxx|
  |xxxxxxxxxxxxx|    /    /---|-------------|
  |xxxxxxxxxxxxx|   /    /    |  x nursery x|
  |xxxxxxxxxxxxx|  /    /     |    xxxxxxxxx|
  |-------------|-/    /      |      xxxxxxx|
  |  x nursery x|     /       |        xxxxx|
  |    xxxxxxxxx|    /        |          xxx|
  |      xxxxxxx|   /         |            x|
  |        xxxxx|  /          |-------------|
  |          xxx| /           |             |
  |            x|/            |             |
  |-------------|             |             |
  |             |             |             |
  |             |             |             |
  ```

#### Finalizer registry

As mentioned in the [Global Heap Management](#global-heap-management) section,
the global root set is contained in the local heaps.  However, the global heap
utilizes reference counting GC, whereas the local heaps utilize tracing GC.  The
finalizer registry's primary purpose is to cohere these disparate approaches.

Actor execution routinely creates new references by copying values, but such
copying has no impact on global value reference counts (i.e. there is no read
barrier).  Instead, the finalizer registry retains a covering set of singular
references to global heap values, such that it is impossible for a local value
to refer to a global value unless it is also reachable via one or more of the
finalizer registry items.  Global value references are merged into the finalizer
registry under any of the following conditions:

- A message is moved from the mailbox to the minor heap.
- A local->global reference is traced, whether during minor or major GC.
- A local value is moved to the global heap, whether during promotion from the
  oldest aging cohort, or as a root of a message to be sent.

The finalizer registry maintains finalizers for all registered values which
require finalization after they become unreachable.  References to global values
are the most common example of finalizable values, but any mutable value can
also be finalized.  Native Hemlock types typically have little use for local
finalization (file descriptors are wrapped by the runtime as global opaquely
finalized values), but foreign function interface (FFI) integrations commonly
require explicit resource cleanup, and finalizers are an ideal solution.

Note that immutable values are excluded from finalization for multiple reasons:

- Referential transparency means that the number of copies of a reachable
  immutable value is arbitrary.  We would have to explicitly track value
  equivalence sets in order to finalize each value equivalence set only once.
- Finalization is of little practical use unless it is effectful, but
  effectfulness means impurity that taints the finalizable value.  Such values
  could not reside in the global heap, which would fit very poorly with
  Hemlock's overall design.

The finalizer registry is actually two intertwined data structures:

- Every time an item is added to the registry, it is prepended to a singly
  linked age-ordered list of registry items.  Registry items are heap-allocated
  and they contain:
  - Finalizable value address.
  - List linkage.
  - Balanced tree linkage (see below).
- A random access lookup table, keyed by value address.  This is actually
  implemented as the union of distinct major/minor heap lookup tables, where the
  major heap lookup table manages the portion of the age-ordered list which has
  been promoted to the major heap.  The lookup tables are separate because they
  must be rebuilt every time a major/minor collection happens, respectively; if
  they were unified, then the table rebuilding work would have to be done for
  all finalizer registry items even during minor collections.

  The table is implemented as a hash table with 4-element buckets, with a per
  bucket balanced tree fallback in lieu of secondary hashing.  A bucket is 64
  bytes, and since memory is typically fetched 64 bytes at a time, linear bucket
  scanning is cheap relative to memory fetch overhead.  Each bucket element
  stores both key (finalizable value address) and value (registry item); it
  would be possible store only the value and retrieve the key in the registry
  item, but doing so would commonly incur cache misses, thus drastically
  reducing lookup performance.

  If a bucket contains more than 4 items, the last element is the root of a
  balanced tree, ordered by value address.  The table is initialized with a load
  factor in (0.25 .. 0.5], which limits memory overhead should the table
  experience little growth between GCs; as items are inserted into the table,
  performance gracefully degrades from O(1) to O(lg n).

  The table is allocated in the nursery, but its size does not count toward the
  nursery size limit.  Were the table size to count toward the nursery size
  limit, it would be possible for large cohorts of aging finalizable values to
  prematurely trigger minor GC, thus degrading performance for several minor GC
  cycles and causing premature promotion to the major heap.

Finalizer registry items are marked/copied during GC in distinct ways, depending
on whether they are global root references:

- Global root references are marked/copied upon tracing reachable values which
  refer to the corresponding globals.  In other words, as local->global
  references are traced, the corresponding finalizer registry item is looked up
  (created on the fly if necessary) and marked/copied.
- Other finalizable values are uniquely associated with their finalizer registry
  items,and are explicitly referenced (item->value).  During finalizer registry
  rebuilding, if an item's associated value was not reached during tracing, it
  is finalized.

Finalization occurs after marking/copying is complete for the major/minor heap,
respectively.  The finalizer registry list and table are rebuilt by iterating
from newest to oldest in the age-ordered list, and for each item either
finalizing or incorporating into the new finalizer registry.  Finalizers may
trivially resurrect values (they are newly allocated).  Note that iteration
trivially terminates for minor collection upon first encountering an item which
resides in the major heap.

#### Pure computation nursery reset optimization

Hemlock's type system definitively distinguishes pure computations, such that
the compiler can statically determine that a function depends only on its
inputs, causes no memory side effects, and only its return value escapes the
computation.  Subject to the following limitations (not all of which are
strictly required), the runtime may copy out the result of a pure computation
and reset the nursery frontier to incorporate only the result:

- No GC has occurred since function entry.
- The result type is non-recursive (and therefore size can be computed in
  constant time).
- The result size is either fixed and small, or the result size is small in
  proportion to the memory allocated since function entry.

#### Copy collection

Minor heap copy collection is conceptually simple.  Live values are copied
fromspace->tospace as they are traced, and forwarding pointers are written to
their obsolete fromspace headers to enable reference updates.  Live values that
have been copied to tospace are linearly scanned for references to fromspace,
which in turn trigger fromspace->tospace copying of any values which have not
already been copied, followed by updating the fromspace references to tospace
references.  Once all roots have been traced, and all tospace->fromspace
references have been traced, copying is complete.

Tracing starts at the roots, which are found in the following places:

- Execution stack.
- Major->minor references.

Root scanning triggers immediate copying, so that the root references can be
immediately updated to tospace pointers rather than during a second pass.

The linear process of scanning tospace values for references to fromspace can be
visualized as a "two finger" algorithm.  One finger tracks linear scanning
progress, and the other finger tracks the tospace frontier.  When the two
fingers meet, copy-collection is complete.  Note that because some values may be
promoted to the major heap, copy-collection actually performs this linear
scanning on both the minor heap tospace *and* the newly promoted values in the
major heap.

During scanning, every time a minor->major reference is encountered, the major
heap value constitutes a major heap root and it must be marked, in support of
incremental major GC.  Marking a major heap value requires setting the marked
bits (one bit in the metadata header, two bits in the mark table), and if not
already marking, pushing a reference to the value onto the gray stack.

The major heap cards corresponding to newly promoted values are marked as dirty,
because they may contain major->minor references that must be incorporated into
the root set during the next minor GC.

Transitively immutable values are promoted to the global heap rather than the
major heap.  This presents an extra challenge, in that global values must be
allocated in a valid post-order (i.e. older values must be allocated before
dependent younger values).  Post-order promotion requires a stack for
maintaining DAG traversal state; this ephemeral "move" stack is placed on top of
the gray stack.  Note that burying the gray stack during local->global promotion
is always safe because the transitively immutable value(s) being promoted never
contain references to the major heap, which can only contain transitively
*mutable* values.

##### Major heap overflow

Major GC depends on minor GC for finding the major heap root set, which means
that completion of minor GC immediately prior to the final stages of incremental
marking is an absolute requirement.  If the major heap overflows during minor
GC, minor heap values which would have been promoted to the major heap must
instead remain in the minor heap.  If this condition ever arises, steps such as
the following should be taken to either resolve the memory pressure or halt the
actor, lest the actor suffer increasingly degraded performance due to minor heap
expansion.

1. Trigger synchronous completion of major heap marking, so that subsequent
   major heap allocation will occur in a newly minted tospace.
2. Trigger minor GC (optionally without cohort advancement), so that all values
   which could not be promoted during the previous minor GC are promoted.  If
   the major heap overflows, halt.  Note that this regime double-promotes at
   least one cohort, but this is inconsequential in the context of impending
   major heap overflow.

### Major heap

The major heap primarily grows as a side effect of minor collection, by
incorporating any mutable values which survive long enough to be promoted from a
maximally aged minor heap cohort.  If the generational hypothesis holds for the
application workload, most values will be collected before promotion to the
major heap, and once promoted, major heap values will become unreachable at a
low rate relative to the nursery allocation rate.  Nonetheless, the major heap
must be periodically GCed, and we use incremental marking and compaction to
intersperse the work with application computation, so that even a very large
major heap can be GCed without causing long pauses.

The major heap secondarily grows due to direct allocation of values which are
too large to be efficiently allocated in the minor heap.  The minor heap is
relatively small, and premature promotion of small values can occur if large
values are intermingled.  Aside from the benefits of excluding large values from
the minor heap, the minor heap is fundamentally incapable of supporting interior
references more than 2 MiB past the beginning of a value.  This imposes a hard
allocation size limit in the minor heap regardless of other considerations.

The mark-compact algorithm performs three steps, which are detailed in the
following sections:

1. Incrementally mark reachable values
2. Compute relocation and first value tables
3. Incrementally compact reachable values

#### Incremental work

The goal of incremental GC is to break the potentially large task of marking and
compacting all reachable values into smaller units of work that are not overly
disruptive to the application.  These increments are interspersed with
application execution, ideally at a steady pace throughout each major GC cycle.
Frequency of major GC is the lesser of two frequencies controlled by tunable
parameters:

- Relative major heap growth.  Suppose the parameter is set at 1, in which case
  major GC completes every time the global heap doubles relative to its size
  just after the previous GC.
- Absolute major heap growth.  Suppose the parameter is set at 16 MiB and the
  actor has just started executing, in which case major GC completes when the
  heap grows to 16 MiB.

#### Marking

Incremental marking work is measured in terms of reachable values marked, but
the more values that are unreachable (i.e. garbage), the sooner marking will
complete.  If this condition occurs, then the only values yet to be marked have
not yet been promoted to the major heap; they will be incrementally marked at a
slower rate than the initial marking rate.

The mechanism for triggering incremental marking is tightly integrated with
nursery allocation.  The nursery has a hard limit that triggers minor GC.  There
is also a soft limit that triggers incremental marking.  The nursery allocation
fast path adjusts the nursery frontier ("bump allocates") so long as the
frontier remains within the soft limit.  If allocation would exceed the soft
limit, slow path code triggers incremental marking, computes a new soft limit
(or triggers minor GC if equal to the hard limit), then retries the allocation.

Marking increment scheduling actually lags major heap growth by one minor heap
cycle.  This is because we don't know how much the major heap will grow until a
minor heap cohort is actually promoted and moved to the major heap.  In other
words, the major heap growth which triggers GC takes a full minor cycle to be
fully acted upon.

Marking increments are chosen to limit allocation overhead; the soft limit is
large enough to amortize the slow path overhead.  Marking tends to have
unpredictable memory access patterns (locality is low), and the marking code has
a small icache impact, so there are no other significant constraints on marking
increments other than that they be small enough to avoid impacting application
interactivity.

##### Gray/move stack

Marking utilizes a "gray stack", which at any given time contains references to
all values which have been marked, but their contents not yet processed to
follow references to other values.  Presence of a reference in the gray stack is
the sole distinction between a black versus gray value in the standard tri-color
scheme.

In a non-incremental GC system, the first step would be to push all roots onto
the gray stack, and marking would be complete as soon as the gray stack were
empty.  However, in this incremental system, roots are pushed onto the gray
stack as a side effect of minor collection, and because multiple minor
collections may occur during a single major heap marking phase, it is possible
for the gray stack to be intermittently empty without indicating that marking is
complete.

The smallest possible incremental marking quantum consists of the following
steps:

1. Pop a value *V* off the gray stack.
2. Trace *V*'s references to other values, and for any white values, mark them
   and push them onto the gray stack.

The marking work done in one such quantum is accounted for in terms of *V*'s
size.  A marking increment repeats the above until sufficient marking work has
been completed.  If the last quantum is larger than necessary to satisfy the
increment, then the next nursery soft limit is increased proportionally, such
that incremental marking maintains the intended pace rather than creeping ahead
of pace.

There are two contributing factors to the maximum gray stack depth, both of
which are dynamically dependent on the application and runtime:

- Number of roots reported by minor collection that have not yet been traced.
- Sum of reference breadths along the longest reference chain in the reference
  graph.

In the worst case, the gray stack could contain one element per major heap
value, but the common case is far less costly.  Sufficient virtual memory is
provisioned to handle the worst case even if the heap is at its total memory
usage limit.  In practice the stack space is under-utilized, but the stack
management complexity reduction is compelling.

When a value is transitively moved to the global heap, it must be post-order
traversed, such that the oldest transitively reachable values are moved to the
global heap first.  An ephemeral "move stack" is required during a move to
maintain traversal state, and rather than having dedicated memory for move
stack, the move stack is placed on top of the gray stack.  Whereas the worst
case for the gray stack involves a broad reference graph, the worst case for the
move stack involves a deep reference graph.  The gray/move stack sizing is
always sufficient to hold both the gray and move stacks, because they cannot
simultaneously face worst case conditions.

##### Mark bit table

Compaction is destructive, such that it is not possible to use forwarding
pointers as the basis for updating references.  Three out-of-band data
structures provide the foundation for reference updates:

- Mark bit table
- Relocation table, described later
- First value table, described later

The mark table contains one corresponding bit for each word in the local heap.
When a value is marked, two bits are set in the mark table, corresponding to the
first and last words of the value, metadata header included.  By setting two
bits, we can use the mark table to compute value size, assuming we know which
bit denotes value start versus end (facilitated by the relocation table).  Note
that because two distinct mark bits must be set for each reached value, values
must be at least two words large, metadata header included.  The only exception
to this minimum size constraint is pads, which are unreachable and therefore
never marked.

##### Relocation table

The relocation table enables relocation computation.  Although the major heap
has virtual memory semispaces, the relocation table is best understood in terms
of a single space that is compacted, where each reachable value has an offset
between its pre-compaction and post-compaction locations.  A relocation can be
computed by processing at most two relocation table entries and one card's mark
bit table entries.

The relocation table has one entry per major heap card.  The relocation table is
computed in a single pass, starting at the base of the heap.  Each relocation
table entry typically records:

- Tospace address for the first reachable value in the card.  (This information
  is equivalent to compaction offset, but with fromspace-tospace translation
  amortized.)
- Inside sense bit.  True if card starts inside a reachable value, false
  otherwise.  This makes it possible to skip past a card-spanning tail when
  finding the first reachable value within the card.

The table entry makes relocation lookup for the first reachable value which
starts in each card trivial.  Relocations for subsequent reachable values in the
card must be derived from the table entry by subtracting the sum of the sizes of
all interposed reachable values, as well as the size of the value itself (recall
that the heap grows downward in memory), as computed using the mark table.  The
card size bounds the overhead; 1024-byte cards have only 128 corresponding bits
in the mark table.

The above describes the typical case for the relocation table, but a different
encoding is required for an entry which does not contain the beginning of a
reachable value.  In this case the card may contain a portion of a reachable
value, and the relocation table records the *card offset* to the nearest lower
card which contains the beginning of a reachable value (recall that the heap
grows downward in memory).  Such table entries are critical to supporting
relocation computation for interior references.

The following figure depicts a relocation table corresponding to marked
reachable values, using hypothetical 64-byte cards to make the figure size
tractable.

```
        64-byte cards
mark    (reachable
table    values)       relocation table

| |     _|        |_                             ^ higher
| |      |        |                              | address
| |      |        |                              |
| |      |        |
|m|      |xxxxxxxx|
|m|      |xxxxxxxx|
|m|      |xxxxxxxx|    reachable value offset=2
| |     _|xxxxxxxx|_   compaction offset=42
| |      |xxxxxxxx|
| |      |xxxxxxxx|
| |      |xxxxxxxx|
| |      |xxxxxxxx|
| |      |xxxxxxxx|
| |      |xxxxxxxx|
| |     _|xxxxxxxx|_   card offset=2
| |      |xxxxxxxx|
| |      |xxxxxxxx|
| |      |xxxxxxxx|
| |      |xxxxxxxx|
| |      |xxxxxxxx|
| |      |xxxxxxxx|
| |     _|xxxxxxxx|_   card offset=1
| |      |xxxxxxxx|
|m|      |xxxxxxxx|
| |      |        |
|m|      |xxxxxxxx|
|m|      |xxxxxxxx|
| |      |        |
|m|      |xxxxxxxx|    reachable value offset=0
|m|     _|xxxxxxxx|_   compaction offset=44
| |      |        |
```

The relocation table must preserve minimum alignment of compacted values,
leaving room for pads as needed.  This requirement does not fundamentally impact
algorithmic complexity; the relocation table is still computed in a single pass.
Similarly, compaction must insert pads as necessary, but compaction still only
requires a single pass.  The following figure depicts a simple example in which
compaction results in a new pad between adjacent reachable values.  More
involved circumstances have similar consequences regarding pad insertion.

```
     Pre-compaction                     Post-compaction

|          ...           |         |          ...           |  ^ higher
|          ...           |         |          ...           |  | address
|          ...           |     ____|________________________|  |
|          ...           |    /    |  value                 |
|          ...           |   /  ___|__hdr lg_align=3, sz=8__|
|________________________|__/  / __|__pad sz=8______________|
|  value                 |    / /  |  value                 |
|__hdr lg_align=3, sz=8__|___/_/ __|__hdr lg_align=4, sz=8__|
|  value                 |      /  |          ...           |
|__hdr lg_align=4, sz=8__|_____/   |          ...           |
|          ...           |         |          ...           |
|          ...           |         |          ...           |
```

##### First value table

The first value table enables per tospace page demand compaction.  Compaction is
incremental, which means that the application may attempt to access tospace
pages before they have been compacted via incremental work scheduling.  As
described in more detail later, an attempt to access an uncompacted tospace page
triggers compaction.  The first value table contains one entry per tospace page
and specifies the fromspace address of the first reachable value to copy.  The
first value is computed in a single pass in conjunction with the relocation
table.

##### Card tables

As mentioned earlier, major->minor references constitute minor heap roots that
must be traced during every minor collection.  The simple-but-slow method for
finding such roots would be to linearly scan the entire major heap.  Hemlock
avoids this overhead by maintaining a three-level card table, where cards are
"dirtied" by the write barrier.  Card dirtying is implemented approximately as
follows.

```c
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

typedef uint64_t hl_word_t;

typedef struct {
    // ...
    hl_word_t *major_base; // Base of current major heap.
    // ...
    uint8_t *card_tab0; // Current primary card table (1024 B per card).
    uint8_t *card_tab1; // Current secondary card table (2^10 cards per entry).
    uint8_t *card_tab2; // Current tertiary card table (2^20 cards per entry).
} hl_actx_t;

void
card_dirty(hl_actx_t *actx, const hl_word_t *val) {
    size_t word_ind = val - actx->major_base;
    size_t card_ind = word_ind >> 7;
    actx->card_tab0[card_ind] = 1;
    actx->card_tab1[card_ind >> 10] = 1;
    actx->card_tab2[card_ind >> 20] = 1;
}
```

Each level of the card table incurs a write barrier overhead of an independently
written byte.  Note that this cost is mitigated by the fact that each level of
the card table is independent, and that the writes have no read dependencies.
Assuming sufficient memory bandwidth, all three writes can arbitrarily overlap
mainline execution without incurring dynamic data dependencies.

Incremental compaction of the major heap adds a complication to the card tables.
Upon transitioning from marking to copy-compaction, the major heap tospace needs
an accompanying card table which prevents any major->minor references going
unnoticed during the next minor GC.  Again, the simple-but-slow approach would
be to scan the entire major heap tospace, but that would defeat the purpose of
incremental copy-compaction.  Instead, a separate card table is synchronously
constructed based on the fromspace's card table to conservatively capture all
major->minor references in the major heap tospace.  The tospace card table is
computed in a single pass in conjunction with the relocation table.

The tospace's card table is conservative in that it may contain spuriously dirty
cards due to unreachable fromspace values near reachable fromspace values having
contained major->minor references.  Although unnecessary card dirtying triggers
extra work during the next minor GC, no correctness issues arise, nor are any
special code paths required.

###### Card summarization

The card size is chosen to be small enough that when a major heap write occurs
and the card is dirtied, only a small portion of the major heap must be scanned
for major->minor references during the next minor GC.  Even so, card scanning is
a significant minor GC overhead, and card summarization amortizes the cost of
scanning infrequently mutated cards.

A clean card is represented in the card table as 0, and when dirtied by the
write barrier, the card table is unconditionally overwritten with a 1.  That
leaves values [2..255] as card summary indices, that serve as lookups into a
254-element summary table that is directly embedded in `actx`.  Each card
summary table entry contains one bit per card word, i.e. 128 bits.  The
corresponding summary table bit is set to 1 if the card contains a major->minor
reference.  This expedites minor heap root gathering; parsing the major heap
(see [crossing table](#crossing-table) documentation) and conditionally
extracting roots based on reference target address is much more work than
referring to the card summary bitmap.  Incidentally, the secondary and tertiary
card tables can contain summary table indices if there is only one dirty card
within the range of cards represented at that level of the card table.  This
encoding further reduces the amount of card table scanning needed for sparsely
dirtied regions of the major heap.

The extremely limited summary table size puts downward pressure on nursery size,
because the larger the minor heap is, the larger the summary table needs to be
in order to summarize all dirtied cards.  However, a larger summary table
requires correspondingly larger card table entries, as well as more
sophisticated summary table entry allocation algorithms.  Furthermore, larger
card table entries would have to be carefully encoded, lest they impose
additional write barrier overhead.

Summary table allocation is managed via a 254-bit bitmap, where a 1 bit
indicates that the summary table entry is in use.  During minor GC root
gathering, a fresh bitmap is used to mark which entries remain used.  Any table
entries which were unused prior to root gathering are available for card
summarization during the current collection, whereas the entries which are
reclaimed during the current GC become available only once root gathering has
completed (i.e. a delay of one minor GC).  Were major->minor references
rewritten during a second pass, it would be useful to maximize summary table use
during the second pass, but minor GC is a single-pass algorithm wherein the
major->minor references are updated to refer to minor tospace as they are
discovered.

##### Crossing table

Arbitrary pointers into the major heap arise in two contexts.  In both cases,
such pointers must be contextualized by their containing value's header in order
to be of use.

- Minor heap root finding of major->minor references in the major heap requires
  that card contents can be parsed independently of the rest of the major heap.
  Card table scanning begins at the beginning of each dirty card.  The first
  word within a card may or may not be in the interior of a value.  Given the
  location of the header for the first (lowest address) value in a card, all
  card contents can be parsed in a single pass.
- Boxed values in e.g. arrays may be more than 2 MiB past the beginning of the
  outer value, which means that the inner value header cannot encode the offset
  to the outer value header.  Major heap marking tracks only outer values, so
  the inner reference must be mapped to the outer value.

In both cases the value header associated with an address may be in the same
card, or arbitrarily many cards lower in memory.  The crossing table provides
the necessary information to quickly locate the value header.

First a note about value addressing: the canonical base address for a value is
at the boundary between the header and the value data.  This is important
because value headers may be one or two words; were the beginning of the header
used as the canonical value address, data access would require conditional
logic.  In any discussion of header addresses, we actually mean the address
immediately following the end of the header (i.e. the base address of the value
data), rather than the base address of the header.

If a card contains the beginning of one or more values, the crossing table
stores the offset in words to the lowest value.  Such offsets are in [0..0x7f].
Otherwise the crossing table stores -floor(lg(-offset)) for the next crossing
table entry to consult.  Such offsets are in -[0..53] (corresponding to major
heap offsets in [2<sup>0</sup> .. 2<sup>(63-10)</sup>]), encoded as
[0x80..0xb5].  If a value straddles more than two cards, the interior cards'
crossing table entries encode optimally short chains of entries to consult on
the way to the value's header.  For example, if the crossing table entry of
interest precedes the current one by 13 (0x1101), the consulted entries will
contain, in order, (3, 2, 0), i.e. (-2<sup>3</sup>, -2<sup>2</sup>,
-2<sup>0</sup>).

Note that given a value which ends in the current card, the crossing table
provides enough information to find the card in which the value begins.
However, that predecessor card may contain multiple values, so the contents of
the predecessor card may have to be parsed in order to find its *last* value.
Though it is tempting to store the last value offset rather than the first value
offset, doing so would break down (the first value would be impossible to find
using only the crossing table) if the lowest card in the heap were only
partially allocated (at the heap's frontier).  An alternative strategy would be
to handle the lowest card specially; doing so would increase code complexity,
but reduce card parsing overhead.

#### Incremental compaction

Incremental compaction progresses from oldest to newest values, and is driven by
the nursery allocation soft limit mechanism described in the [Marking](#marking)
section.  Compaction requires a single pass starting at the base of the heap.
As described in the [Relocation table](#relocation-table) section, the
relocation table encodes sufficient information to allow skipping over cards
which contain only unreachable values.  The mark table encodes the remainder of
the information necessary for compacting values into their new locations, with
pads interposed as needed, as determined by the alignment constraints recorded
in metadata headers.

##### Compaction status table

The compaction status table maintains one bit per page to track whether a
tospace page has been compacted, where 0 indicates "uncompacted" and 1 indicates
"compacted".  At the beginning of the compaction phase, all tospace pages are
inaccessible, i.e. any attempt to read/write tospace pages before they are
compacted results in a `SIGSEGV` signal.  Each time a tospace page is compacted,
its page protection is updated such that subsequent read/write access succeeds,
and the corresponding compaction status table bit is set to 1.  The compaction
status table ensures coherent interleaved incremental and demand compaction;
repeated compaction of a page would potentially overwrite updated values with
obsolete versions, not to mention inefficiency of repeated compaction.

### Advancement

As mentioned in the global heap management section, an actor can be compelled to
advance its exposure to obsolete global heap values.  Global references may
reside in three distinct areas of the local heap, each of which manages
advancement distinctly:

- Major heap.  All local references are updated during compaction; so too are
  references to global forwarded values.
- Minor heap.  Local references are updated during tracing; so too are
  references to global forwarded values.
- Mailbox.  A single pass over the mailbox suffices to rewrite references to
  global forwarded values, but this is only done when the actor is explicitly
  required by the runtime to advance.  However, the minor and major heaps may
  have more advanced exposure than the mailbox, so when messages are moved to
  the heaps, references to global forwarded values are proactively updated.
  Thus the heaps' exposures never expand backward.

Note that the minor heap's exposure is always at least as advanced as the major
heap's exposure (because major collection always coincides with a minor
collection), so only the major heap's exposure needs to be tracked.  The
mailbox's exposure could be tracked accurately by labeling all messages with
timestamps, but such overhead is not warranted.  Instead, mailbox exposure is
reset under any of the following circumstances:

- The mailbox is emptied (last message removed).
- The runtime explicitly advances the actor's exposure.

In practice this almost always suffices to keep the mailbox sufficiently
advanced that it does not impede global GC.
