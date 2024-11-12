# Work stealing

Pure computations are commonly expressible as [embarrassingly
parallelizable](https://en.wikipedia.org/wiki/Embarrassingly_parallel) algorithms. Anywhere the
continuation of a pure computation can be expressed as the merge of pure sibling continuations, the
siblings can safely be executed in parallel. The following example is so trivial as to not warrant
the overhead of parallel execution, but it meets all the requirements for work stealing.

```hemlock
# Array signature excerpt.
Array = {
    type t 'a ^t: ^&t a

    map 'a 'b ^t >e: f:(a >e-> b) -> _&t a >e-> ^&t b
  }

sum = Array.map ~f:(fun x -> x * x) [|1; 2; 3; 4; 5|]
```

Internally, the `Array.map` function performs recursive binary decomposition of the input, such that
at the top level, the two halves of the array are composable sibling map operations. Even for an
array with many elements, as long as `f` is pure, the map operation decomposes into a tree of
independent map operations that write the resulting array elements. The recursive structure of the
map code induces a tree of map operations which compose to form the top-level map result.

```
    [|1, 2, 3, 4, 5|]
  ->
    [|1, 4, 9,16,25|]
      |  |  |   \  \
      |  |  |    \  \
    [|1, 4, 9|] [|16,25|]
      |  |   \     |  |
      |  |    \    |  |
    [|1, 4|]   9  16 25
      |   \
      |    \
      1     4
```

The `Array.[map_]reduce[_hlt]` functions also perform recursive binary decomposition, but many
algorithms require no special implementation considerations to be naturally conducive to parallel
execution. Practical examples abound; the following are simple illustrative cases.

- Binary operators like `Uns.\+` are in principle merges, so work stealing can kick in if the
  inputs are computed via expensive independent continuations. The same is true of any pure function
  with two or more independent inputs. If there are more than two non-trivial inputs, the compiler
  induces a binary tree structure on the continuations such that at each point where the function
  can be suspended, there is at most one stealable continuation.
- The `OrdMap.t` type is a prime data structure example since it is implemented in terms of [AVL
  trees](https://en.wikipedia.org/wiki/AVL_tree). Operations on binary nodes commonly look something
  like `OrdMap.reduce`:

  ```hemlock
  reduce ~f t =
      let rec fn ~reduce2 = function
        | Leaf {k=_; v} -> v
        | Node {l; k=_; v; n=_; h=_; r=Empty} -> reduce2 (fn ~reduce2 l) v
        | Node {l=Empty; k=_; v; n=_; h=_; r} -> reduce2 (fn ~reduce2 r) v
        | Node {l; k=_; v; n=_; h=_; r} -> reduce2 v (reduce2 (fn ~reduce2 l) (fn ~reduce2 r))
        | Empty -> not_reached ()
      match t.root with
        | Empty -> None
        | _ -> Some (fn ~reduce2:f t.root)
  ```

  Note that even trinary reductions (two children plus current node) are implemented as binary
  reductions, which is the simplest implementation strategy *and* optimal for interaction with work
  stealing.

# Terminology

The work stealing algorithms are somewhat involved, though they rest on a limited set of concepts.
The following terminology introduces those concepts; algorithmic details follow.

- **Left/right continuations:** Two pure sibling continuations are referred to as "left"/"right"
  continuations, where the choice of left versus right is arbitrary but stable for any sibling pair.
- **Fork/merge:** A pure computation that can be decomposed into two pure sibling continuations can
  be "forked" into parallel computation of the left/right continuations, then "merged" to a final
  result.
- **Tree-structured parallelism:** Fork/merge induces a tree structure on recursively pure
  computations. At any given time during computation of the tree, a path into the tree is implicitly
  traced by the execution stack. Suspended call frames may be waiting for the left continuation to
  complete, in which case the right continuation is a target for work stealing.
- **Victim:** An actor which has had a right continuation stolen from it is a victim of work
  stealing. A victim remains suspended until all its delegates merge, because GC is prohibited for
  as long as delegates have data dependencies on the victim's heap.
- **Delegate:** An executor may decide to victimize an actor and steal a right continuation, in
  which case it hands the right continuation off to a "delegate" actor. Delegate actors are
  special in that they do not exist independently of their victims. Note that a delegate can in turn
  be a victim as a consequence of recursive work stealing, and it remains a victim until all its
  delegates merge.
- **Posse:** A victim may have one or more delegates, which can in turn be victims. The directed
  acyclic graph of victim-delegate pairs induces a dependency tree, which as a whole is referred to
  as a "posse".
- **Cabal:** The executor set responsible for a posse is referred to as a "cabal". Multiple cabals
  may exist simultaneously (at most one per independent actor), and an executor may be a member of
  multiple cabals.

# Algorithms

Embarrassingly parallelizable computations can trigger terrible performance degeneration if all
parallelizable work is blindly scheduled to execute in parallel. The scheduling overheads can of
course be problematic, but the critical limitation is memory. Every parallel computation requires
its own memory context until its result is merged, and even if each memory context is tiny,
aggregate memory overhead can overwhelm the system if the available parallelism is exponentially
bounded. Hemlock mitigates the risk of memory exhaustion two main ways.

- Posse members which reside on the same executor are all stacked into a single actor context, which
  means that their aggregate memory usage is limited to the most basal victim's memory limit, `L`.
  There can be one actor context per executor associated with the posse, so in the worst case,
  memory usage is limited to `E × L`, where `E` is the number of executors. Memory usage can be
  reduced via actor memory limits and/or by limiting the cabal size.
- An executor decides whether to steal work from an actor only once per scheduling quantum, and in
  order for work to be stolen, the same right continuation must have been suspended for the entire
  quantum. This hysteresis mechanism assures that in the common case, work stealing only happens
  when there is more than one quantum of work to steal. Furthermore it assures that scheduling
  overhead is bounded to the same algorithmic complexity as ordinary actor scheduling overheads.

Work stealing is initiated by the executor. When the executor preempts an actor at the end of a
fully utilized scheduling quantum (i.e. the actor did not block for any reason), it may [introspect
the actor's execution stack](execution_stack.md#work_stealing) to look for right continuations in
pure functions that were created prior to the scheduling quantum. If two or more such right
continuations exist and the executor is able to select an underutilized executor without a runnable
delegate in the same posse, the executor steals two right continuations, delegates one to the
selected executor, and self-delegates one since the victim cannot run until all delegates have
merged.

There is no point in stealing a single right continuation from a runnable delegate, since doing so
would result in a linearly dependent posse that could only make use of a single executor at a time.
However, a victim may have more than two right continuations, only two of which were initially
stolen. The victim's remaining right continuations may be stolen without impact to the currently
runnable delegate, in which case it is preferable to steal a single right continuation from an
existing victim and keep the delegate runnable. It would in principle be possible to steal more than
two right continuations during a theft, but doing so would increase risk of a [cache
stampede](https://en.wikipedia.org/wiki/Cache_stampede).

In the typical case, more basal (older) right continuations are expected to require more computation
than less basal (younger) right continuations, under the assumption that sibling continuations have
correlated execution costs. This is true even across posse members, where the victim-delegate
dependency tree can be thought of as connecting the constituent execution stacks. Therefore, given a
choice of right continuations among the stack of posse members managed by the same executor, work
stealing always chooses the most basal right continuation(s).

Due to the per executor stacking of posse members into a single actor context, only the least basal
delegate (topmost on the stack) can be runnable, even though more basal delegates may have completed
merging. This restriction is simply due to the adjacency in memory of posse members; were the
runtime to use a separate actor context for each posse member, the restriction would no longer
exist, though enabling multiple runnable delegates per executor would be of dubious utility, with
the possible exception of enabling delegate migration.

A victim resumes computation once all its pending merges complete. The root victim may of course
continue past the most basal merge, but any other posse member terminates once it completes its most
basal merge, i.e. its entry point. When this occurs the executor attempts to seamlessly steal a
right continuation from a more basal victim that it manages, and spawn a new delegate which
immediately receives the remainder of the scheduling quantum. In practice, the terminated posse
member's context can be reinitialized for use by the newly spawned delegate, so spawn overhead is
very low.

## Synchronization

Minimal synchronization is required to implement work stealing as designed, because actors never run
at the same time they are being stolen from. However, executors need to orchestrate delegation and
merging. At least the following mechanisms are required.

- Each executor must maintain load average statistics which inform whether the executor is
  sufficiently under-utilized to warrant delegation. These are single-writer multiple-reader
  statistics for which precise write-read order is not critical, so memory fences should suffice.
- An asynchronous message containing sufficient context to spawn a delegate must be sent from the
  initiating executor to the receiving executor. This message queue probably needs more stringent
  reliability guarantees than those of actors. For example, it should not be possible for the queue
  to overflow
- Each victim needs to track the number of pending merges using an atomic counter. Furthermore, when
  a merge occurs, the pending merge decrement must trigger the victim to switch from a suspended
  state to a runnable state if it is the least basal victim and in a suspended state.
- A merge requires that the result of the right continuation be written to the victim's execution
  stack. The result must be written prior to the pending merge count being decremented. A memory
  fence between the stack write and the pending merge decrement suffices.

# XXX TBD

This design is currently underspecified in at least the following ways.

- Execution stacks need to support low-overhead introspection that allows discovery of suspended
  right continuations. Furthermore, there needs to be bookkeeping somewhere regarding which right
  continuations have already been stolen, though this may be better done on a per victim basis
  outside the execution stack.
- References are typically either to local heap values, or to global heap values. Delegates have
  references to victim heaps, which introduces a third kind of reference. There are at least two
  possible ways to distinguish references to victim heaps (the space radix tree and the finalizer
  registry), but the GC design document needs to acknowledge the possibility of such references lest
  they cause failure in the implementation.
- Composite results from right continuations may be of arbitrary size. Although writing a reference
  to such a result in the victim's execution stack presents little difficulty, the result must be
  allocated somewhere. One approach is to allocate the result in the global heap, but the victim
  needs to have the result in its root set. One way to accomplish this would be to send the victim a
  message, but somehow hide the message from the application, convert it to a finalizer registry
  entry immediately after resumption, or something similar.
- Each normal actor has a memory limit. Each victim/delegate stack needs to have a memory limit
  equal to the complement of its direct ancestral lineage's memory use, lest work stealing
  cause/avoid OOM failures relative to what would have happened in the absence of work stealing.
  Implementing this will require slightly more sophistication in memory limit initialization, since
  delegate actors will typically have differing memory limits for the execution stack, major/minor
  heaps, and mailbox.
- Victims can be suspended for an arbitrary length of time, and therefore local GC may be
  indefinitely prohibited. The global GC critically requires that actors be able to advance their
  exposure on demand, yet local GC is currently the only mechanism for doing so. Actors should do
  GC-independent in-place advancement on demand, in addition to advancement occurring as a side
  effect of local GC.
- Actors can be migrated between executors in order to balance executor load. Work stealing can also
  balance executor load. The scheduling algorithms which control these two mechanisms should be
  tightly integrated, but how exactly this should work is subtle.
