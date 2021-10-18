# Executors

Executors are responsible for coordinating actor execution, global garbage collection, and managing
I/O operations. During runtime, a Hemlock process has an on-to mapping of executor to CPU core. The
number of executors is a dynamic property of a Hemlock process. In cases where enough
parallelization exists, the runtime creates enough executors to utilize all cores, making it a
one-to-one mapping as well.

## Lookup array

Live executors are discoverable via a global array. The array is large enough to hold one pointer
for each of the possible executors. The current number of executors `n` indicates that executors
`[0,n)` in the array are live. Upon initialization, live exectors are atomically emplaced in the
list at `executors[n]` while `n` is incremented.

Hemlock load balancing algorithms randomly inspect executors within this array, so there is no need
for mechanisms to look up specific executors.

## CPU affinity

When an executor must select a CPU to pin a new thread on, it prioritizes in the following order.
- We discussed three options and need to figure out what's best.
  - Depth-first
    - Any unclaimed CPU within the NUMA group of the current executor.
    - Any unclaimed CPU within the NUMA group of another executor.
    - Any unclaimed CPU.
  - Breadth-first
    - ...
  - Random
    - ...

## Signal handling

## Inter-executor communication

Inter-executor messaging has exactly-once delivery semantics.

TODO - solidify this part of the design. The following is our current undestanding of how this will
work.

Executors allocate message contents within their own `ectx`. The message is sent to another executor
in the form of a message pointer to the message contents. The message pointer is written to the
receiving executor's mailbox.

Upon receipt of the message, the receiving executor copies relevant message contents into its own
memory space (TBD exactly where). The receiving executor then sends the same message pointer back
to the sending executor's mailbox to indicate that the message was received and that the memory for
the message contents may be reclaimed.

## GC advancement

Executors may send inter-executor messages to compel other executors to advance heap exposures.

## Scheduling actors

Actors are scheduled using a scheduling wheel algorithm. The wheel is split into segments. The
executor rolls the wheel, processing each segment in turn. Actors should ideally run for one full
quantum every turn of the wheel. Each segment of the wheel contains two data structures.
1. Runnable queue of actors.
2. Idle, timed-out set of actors.

When any actor yields, it is rescheduled for later. The rescheduling mechanism is as follows.
- If the actor is Runnable, it used its entire quantum which is its entire fair share of CPU on this
  turn of the wheel. It is placed into the runnable queue `N` segments forward.
- If the actor is Idle, it is waiting on its mailbox and it may have used much less than its entire
  quantum. To give it a fair opportunity to use more of its allocated quantum on this turn of the
  wheel. It is placed into the idle, timed-out set `N * (fraction of quantum used)` segments forward and
  preserves its remaining reduction limit.

The executor maintains a set of idle actors. Any actor in this set that becomes runnable is given
priority and run at the first opportunity. While no actors in the idle set are runnable, the
executor processes each segment of the scheduler wheel in turn. Immediately upon turning to a new
segment, any idle, timed-out actors that have become runnable are run again, but only with the
preserved remaining reduction limit. The idle, timed-out actors that are still idle are joined with
the executor's set of idle actors. Then, each actor in the runnable queue is run in turn for a
maximum of a single quantum.

## Load statistics

Load statistics are recorded in the local `ectx`. Any executor may inspect another executor's load
statistics at any time.

Jason and I chatted about reporting load as `load = 1 / wheel turn rate`. It has the same curve as
`1/x`, which is monotonic and continuous, but we need to determine whether we can give any
actionable meaning to discrete `load` factors.

TODO - Look at how load is calculated on Linux and FreeBSD.

## Actor migration

Actors that may be migrated.
- (Discussed with Jason. Not so sure about this one.) Runnable actors that have just yielded. Upon
  yielding, the executor will inspect `N` other random executors to compare load statistics. Based on
  the comparison, the executor may migrate the actor.  The receiving executor schedules the actor `N` segments forward.
- (We discussed finding an actor somewhere in the wheel that we'd expect to run around the
  same time as the end of the receiving executor's runnable tail).
- Idle actors that have just become runnable. The executor will inspect N other random executors to
  compare load statistics. Based on the comparison, the executor may migrate the actor.

Actors that may not be migrated.
- Posse members, at least not to executors that are part of the cabal.
- Actors marked as pinned.
- Idle actors waiting on mailbox completions. Doing so would cause degenerate load balancing
  overhead long before the idle actors contribute to meaningful load. Additionally, in the case
  where the idle actor is waiting on I/O completion messages, it is optimal to keep such actors
  associated with the executor with which they submitted I/O. It has the upside of reducing
  inter-executor I/O completion messaging. Migrating such actors would guarantee at least one
  follow-up message for the I/O completion.

If idle actors become active, normal load balancing mechanisms kick in to migrate based on current,
non-speculative load statistics.

## I/O cancellation

## Supervisors
### Strategies
#### Graph

DAG of actor dependencies that determines startup order.

#### CPU selection

- Pinned
- NUMA groups
- Big/little CPU

#### Crash recovery
- pattern
   - one
   - upstream
   - downstream
   - all
- times
   - n
   - n in t time window

## Actors

### States
- Receiving - Waiting for messages in the mailbox.
- Runnable - In the `Runnable` queue, waiting to be run by the executor.
- Running - Actively running.
- Halted - Halted and requires cleanup and possibly supervisor actions.

### Time slice procedures

Between each time slice, the executor is responsible for the following.
1. Reap any CQEs from the ioring's completion queue. Determine which actor is associated with the
   CQE. Copy the CQE to the location pointed to by the CQE's `user_data` field. If the actor is...

    a. Not managed by the executor - send a message to the managing executor with a hint that the
       actor should be activated.

    b. `Receiving` - move the actor from the `Receiving` list to the `Runnable` queue.

2. If actor placed any SQEs in the ioring, submit it via `io_uring_submit`.

3. If the actor state is...

    a. `Receiving` - place it at the head of the idle actor list.

    b. `Runnable` - place it at the tail of the active actor queue.

    c. `Halted` - place a message in the actor's supervisor's mailbox and place the actor in the
       halted actor list.

### Supervisors
## Scheduling actors
### Time slicing
### Time interrupt
## I/O
### Submitting
#### Can't submit - queue is full
#### Can't submit - completions must be reaped
### Completing