# Execution stack

Individual Hemlock actor stacks are native, C-like stacks. It is generally easy to represent the
layout of such a stack given an input program.

```hemlock
bar () =
  ...some work...
  Stdout.print "Representing the stack right here is easy!"
  ...some work...

foo () =
  ...some work...
  bar ()
  ...some work...

main argv =
  ...some work...
  foo ()
  ...some work...
```

```
+++++++++++++++++++++++
| `main` stack frame  |  ^
+++++++++++++++++++++++  |
| `foo` stack frame   |  | older stack frames
+++++++++++++++++++++++
| `bar` stack frame   |
+++++++++++++++++++++++  | newer stack frames
| `baz` stack frame   |  |
+++++++++++++++++++++++  v
```

Like C, the bulk of a simplified Hemlock stack frame is made of arguments passed from a caller and
of data. Calling conventions in both languages push the program counter (PC) onto the stack as part
of making another call.

```
.                     .
: ...<previous frame> :
+++++++++++++++++++++++ <|
| Non-register args...| <| S
|---------------------| <| t f
| Non-register data...| <| a r
|---------------------| <| c a
| PC                  | <| k m
+++++++++++++++++++++++ <|   e
: <next frame>...     :
.                     .
```

## Stack frame

Unlike C, Hemlock supports efficient [garbage collection](memory.md#minor-heap) and [work-stealing
algorithms](work_stealing.md#algorithms). Both features must introspect and modify parts of the
stack at runtime. We compile a PC-indexed table of stack frame metadata (PC metadata) into Hemlock
binaries to allow efficient lookup of stack frame layout to enable both of these requirements.

```
        +++++++++++++++++++++++++++++++++++++++++
    |>  | On-stack args...                   0+ |
G       |---------------------------------------|
a c |>  | On-stack data...                   0+ |
r o     |---------------------------------------|
b l     | RC quantum generation          0 or 1 |  <|
a l     |---------------------------------------|     W
g e |>  | RC on-stack args...                0+ |  <| o s
e c     |---------------------------------------|     r t
  t |>  | RC in-register args...             0+ |  <| k e
  i     |---------------------------------------|       a
  o |>  | Caller-saved registers...          0+ |       l
  n     |---------------------------------------|       i
    |>  | Callee-saved register refs...      0+ |       n
        |---------------------------------------|       g
        | PC                                  1 |
        +++++++++++++++++++++++++++++++++++++++++
```

### Quick reference - stack frame sections
#### On-stack args...

- May include references that are updated during garbage collection.
- Otherwise similar to C on-stack args.

#### On-stack data...

- May include references that are updated during garbage collection.
- Otherwise similar to C on-stack data.

#### RC quantum generation

- If present, inticates that a stealable [right continuation (RC)](work_stealing.md#terminology)
  is set up in the stack frame.
- Initially 0 when written to the stack.
- Updated by the runtime at the next time-slice pause to record which quantum generation this RC was
  set up.
- Determining whether the RC can be stolen requires knowing the age (in quantum generations) of the
  RC on the stack.
- If the RC is delegated, this space is used by the delegate to record part or all of the RC result.

#### RC on-stack args...

- May include references that are updated during garbage collection.
- These are copied/moved to the stack before calling the RC.
- If the RC is delegated, this space may be used by the delegate to record part of the RC result.

#### RC in-register args...

- May include references that are updated during garbage collection.
- These are copied/moved into registers before calling the RC.
- If the RC is delegated, this space may be used by the delegate to record part of the RC result.

#### Caller-saved registers...

- May include references that are updated during garbage collection.
- Otherwise similar to C caller-save registers.

#### Callee-saved register refs...

- _Only_ includes references that are updated during garbage collection.
- Refs in callee-saved registers must be spilled onto the stack by the caller before calling into
  another function. This guarantees that the references are accounted for in the caller's stack
  frame metadata and can be updated during garbage collection. The callee will also spill them
  before a garbage collection, but it is infeasible to generate metadata for the callee stack frame
  to allow introspection of spilled callee-saved registers.

#### PC

- Used as primary lookup key during stack frame introspection.
- Usually 1:1 mapping of PC to stable stack frame layout.
- Sometimes can come from multiple stack frame layouts if the PC points to a section in a common
  block of code. See [PC metadata table](pc_metadata_table).

### Runtime stack suspension points

Stack introspection in Hemlock requires metadata about each possible stack frame. At runtime, only
the executor (during time-slice pauses) and the garbage collector (during garbage collection) need
to introspect the stack. It is somewhat straightforward to generate metadata for each fully formed
stack frame that leads into another stack frame (e.g. points where a `call` is made into a deeper
function). However, in a runtime system that arbitrarily interrupts execution, the final frame of
execution may only be partially formed. To generate metadata for such partially formed frames, we
would need to generate a metadata entry for every possible point at which a frame could be
interrupted.

To significantly reduce the amount of metadata needed to facilitate runtime operation, the runtime
only allows stack suspension when the final stack frame is in a well-formed state. For executor time
slice pauses, this may happen when the actor runs out of reductions upon jumping backward (looping)
or returning from a function call. For garbage collection pauses, this may happen whenever the actor
allocates memory.

Note that runtime debuggers necessarily cause arbitrary interruptions of execution, so still require
generation of the full set of possible metadata for introspection. However, omitting debug metadata
results in significantly leaner production binaries.

### PC metadata table

The PC metadata table is used during every actor garbage collection and every executor time slice to
introspect the entire stack. Table lookups are in the critical path of runtime execution so must
be made as fast as possible.

Individual stack frames have the following metadata.

```hemlock
FrameMetadata: {
    type t: t = {
        frame_size: uns

        gc_references_bitmap: Bitmap.t uns # 1 bit per word in frame

        ws_qg_offset: uns
        ws_trampoline_pc: PC.t
      }
  }
```

Hemlock stack frame introspection uses a frame's (primary) PC to look up frame metadata. If the
primary PC of a stack frame refers to a common code block (e.g. a block of code reached with
a `jmp` instruction), then the primary PC is not 1:1 with a unique stack frame layout. In such
cases, a secondary lookup using the (secondary) PC from which the jump was initiated is required.
The secondary PC must be placed in an exact location within the stack frame relative to the primary
PC so that it may be found via secondary lookup. This case is recursive, in that the secondary PC
may also come from a common code block and may require yet another secondary PC lookup. The unique
sequence of primary PC and secondary PCs is sufficient to identify every unique stack frame layout.

```hemlock
PCMetadataTable: {
    type t: t = map PC.t node
      also node: node =
      | Frame_metadata of FrameMetadata.t
      | Secondary_lookup of secondary_lookup
      also secondary_lookup: secondary_loookup = {
        pc_offset: uns
        table: t
      }
  }
```

### Stack traversal

Stack traversal is done in one direction—from newest frame to oldest frame. The frame's PC is
always at the end of the frame. The PC alone is sufficient for determining the size of its
associated stack frame via metadata lookup in the PC metadata table, thus there is no need for
storing a frame pointer on the stack. Once the size of a frame is known, determining the location of
the PC in the next-older frame is a simple calculation.

```hemlock
older_pc_address = current_pc_address + (word_size * current_frame_size)
```
#### C frames

- If a PC falls outside of a Hemlock code segment, we determine that it is part of a C frame.
- Use DWARF info to progressively unroll until we hit a Hemlock PC again.
- Store the combined size of the C frames to amortize introspection of long-lived frames.

#### Hemlock reentry from C

- Use libunwind to get stack info. Store the size of the whole blob in a table keyed by the SP where
  the PC is.
- Hemlock is oblivious to VLAs in C frames because libunwind knows how to introspect their sizes.
- In the reentry Hemlock frame, store a value (initially 0) for the size of the combined C frames
  above. Upon unwinding the stack, if this value is 0, calculate the size of the C frames and then
  store the value here.

### Garbage collection

Stack frames include both (GC-able) references and (non-GC-able) non-references. The garbage
collector must know where references exist within each stack frame so that it may update them during
[minor heap copy-collection](memory.md#minor-heap) and [major heap
compaction](memory.md#major-heap). The PC metadata table includes a bitmap of the entire stack frame
that denotes each address as either a reference (1) or a non-reference (0).

```hemlock
gc_frame address frame_metadata gc_state =
    rec inner address bitmap gc_state =
        match Bitmap.(bitmap = 0) with
          | true -> gc_state
          | false ->
            let n_zeros = Bitmap.ctz bitmap
            let address' = address + (word_size * n_zeros)
            let bitmap' = Bitmap.usr (n_zeros + 1) bitmap
            GarbageCollection.mark address' gc_state
              |> inner address' bitmap'
    inner address frame_metadata.gc_references_bitmap gc_state

rec gc_stack pc_address gc_state =
    match pc_address >= gc_state.stack_address with
      | true -> gc_state
      | false ->
        let frame_metadata = FrameMetadata.lookup pc_address.value
        gc_frame pc_address frame_metadata.gc_references_bitmap gc_state
          |> gc_stack (pc_address + (word_size * frame_metadata.frame_size))
```

### Work stealing

Some stack frames include setup for a single stealable [right
continuation (RC)](work_stealing.md#terminology). If there is a stealable RC in a stack frame, the
presence is marked via a non-zero (quantum generation)[rc_quantum_generation] (QG) offset value in
the stack frame's metadata. An offset value of `0` from the PC address would not make sense, so is
reserved to indicate that no RC can be stolen from the stack frame.

```hemlock
ws_frame pc_address frame_metadata ws_state =
    match frame_metadata.qg_offset = 0 with
      | true -> ws_state # No work stealing opportunities in this frame
      | false ->
        let qg_address = pc_address + (word_size * frame_metadata.qg_offset)
        match qg_address.value = 0 with
          | true -> WorkStealingState.set_qg qg_address ws_state
          | false ->
            match qg_address.value > ws_state.stealable_qg with
              | true -> ws_state # RC hasn't survived long enough on the stack for work stealing
              | false -> WorkStealing.steal pc_address frame_metadata.ws_trampoline_pc ws_state

rec ws_stack pc_address ws_state =
    match pc_address >= ws_state.stack_address with
      | true -> ws_state
      | false ->
        let frame_metadata = FrameMetadata.lookup pc_address.value
        ws_frame pc_address frame_metadata ws_state
          |> ws_stack pc_address + (word_size * frame_metadata.frame_size)
```

#### Trampoline function

If an RC can be stolen from the stack frame, stack frame metadata will include a valid
`ws_trampoline_pc` field. The work stealing trampoline PC points to a generated bespoke trampoline
function for launching the RC in a (delegate)[work_stealing.md#terminology] actor. Given the PC
address of the (victim)[work_stealing.md#terminology] actor's stack frame, the bespoke trampoline
function does the following:

1. Rewrites the victim stack frame's PC to point to a merge function generated specifically for the
   [left continuation (LC)](work_stealing.md#terminology) to merge its result with the
   already-present result of the RC (see final trampoline function step).
2. Loads the victim's `RC in-register args...` section of the stack frame into registers.
3. Copies the victim's `RC on-stack args...` to the delegate's stack and calls the RC.
4. Returns the result of the RC into the victim's stack frame. The entire space in the victim stack
   frame's `Quantum generation`, `RC on-stack-args...`, and `RC in-register args...` sections is now
   available for copying a result of constant size. If the result value is of variable size, is a
   boxed value, or is simply a constant size too large to fit in the available space, the
   value is boxed. If the result would be a reference to a value in the delegate's heap, a copy of
   the result is written to a location with lifetime longer than the delegate's heap (global heap,
   victim's heap/mailbox, etc.); mechanism TBD.
