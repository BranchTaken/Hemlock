# IELR(1) as Implemented by `hocc`

[This is a living version of an originally web-published technical report. [^evans2024]]

The `hocc` parser generator, which is part of the [Hemlock](https://github.com/BranchTaken/Hemlock)
programming language project, implements several LR(1)-family parser generation algorithms, namely
[LALR(1)](https://en.wikipedia.org/wiki/LALR_parser) [^deremer1969], [canonical
LR(1)](https://en.wikipedia.org/wiki/LR_parser) [^knuth1965], PGM(1) [^pager1977][^fpottier], and
IELR(1) [^denny2010]. These algorithms are amply documented and (re-)implemented, with the notable
exception of IELR(1), which is documented only in the original paper and implemented only by the
original authors in [`bison`](https://www.gnu.org/software/bison/). This posed extreme
implementation challenges in the context of `hocc`. The IELR(1) paper is closely tied to the
particulars of the `bison` implementation, and perhaps for that reason the terminology and structure
are closely based on the idiosyncrasies of DeRemer's presentation of LALR(1). This terminology
diverges substantially from that of Pager's presentation of PGM(1), whence `hocc` took original
inspiration. This report recasts the IELR(1) algorithm as distilled during `hocc` implementation,
giving a pragmatic high-level perspective more conducive to straightforward (if less efficient)
implementation than that provided by the original paper.

## Introduction

Knuth [^knuth1965] originated the "**L**eft to right, **R**ightmost recursive" (LR) family of
parser generation algorithms. The theory applies generally to languages recognizable by LR(k), where
k denotes the number of tokens of lookahead. In practical use, k is almost always 1, in part because
additional lookahead complicates implementation, but more importantly because most practical
grammars can be easily rewritten to avoid the need for multi-token lookahead.

In 1965, canonical LR(1) in all its elegance posed serious implementation challenges due to ~10X
state redundancy in the generated state machines. **L**ook**a**head LR(1) (LALR(1)) came along in
1969 as a practical compromise that collapses isocore sets (described later), even if doing so
introduces parser inadequacies relative to the grammar specification. The **P**ractical **G**eneral
**M**ethod (PGM(1)) was presented in its full form in 1977, and it dramatically improves on LALR(1)
by avoiding parser inadequacies, with the important caveat that the algorithm can only provide those
guarantees in the absence of disambiguation via precedence/associativity rules. PGM(1) never saw
wide adoption, perhaps because LALR(1) was already widely implemented; nonetheless PGM(1) is
strictly superior. IELR(1) stemmed from a research need for non-redundant parsers with no
LR(1)-relative inadequacies. Although there are edge cases that can in practice cause redundant
states during parser generation, the parsers are much smaller than their LR(1) counterparts, and
IELR(1) does definitively deliver on inadequacy elimination, thus assuring that LR(1) and IELR(1)
parsers recognize the same grammars.

The remainder of this report overviews the canonical LR(1) parser generation algorithm with a focus
on concepts upon which IELR(1) builds, briefly describes LALR(1), then presents IELR(1) as
implemented by `hocc`. The perspective is primarily LR(1)-relative, which differs substantially from
the LALR(1)-relative exposition of the original IELR(1) paper.

## Canonical LR(1)

### Terminology

The following common example "arithmetic" grammar in `hocc` format suffices for defining various
relevant terms as they are used in the `hocc` source code.

```hocc
hocc
    left mul
    left add < mul
    token STAR "*" prec mul
    token SLASH "/" prec mul
    token PLUS "+" prec add
    token MINUS "-" prec add
    token INT
    token EOI
    nonterm MulOp ::=
      | "*"
      | "/"
    nonterm AddOp ::=
      | "+"
      | "-"
    nonterm Expr ::=
      | Expr MulOp Expr prec mul
      | Expr AddOp Expr prec add
      | INT
    start Answer ::= Expr EOI
```

A grammar comprises production rules, abbreviated as **_prod(s)_**. `Answer ::= Expr EOI` and `Expr
::= INT` are examples of prods. A prod has a left-hand side (LHS), which is always a non-terminal
symbol, abbreviated as **_nonterm_**. `Answer` and `Expr` are examples of nonterms. A prod also has
a right-hand side (RHS), which is a sequence of nonterms and **_tokens_**. `STAR`, its alias `"*"`,
and `EOI` are examples of tokens.

An **_LR(0) item_** is a prod with associated position, where the current parsing position is
indicated by a dot. For example, `Answer ::= · Expr EOI`, `Answer ::= Expr · EOI`, and `Answer ::=
Expr EOI ·` are distinct LR(0) items based on the same prod.

An **_LR(1) item_** is an LR(0) item with an associated **_follow set_**, i.e. a set of tokens which
may immediately follow the prod. For example, `MulOp ::= · "*", {INT}` indicates that a
multiplication operator may be followed only by an integer. For a less obvious example, `Expr ::= ·
INT, {"*", "/", "+", "-", EOI}` indicates that an integer may be followed by a math operator or
end-of-input (`EOI`). Note that the dot position is not particularly relevant to the follow set.

An **_LR(0) item set_**, also known as a **_core_**, is simply a set of LR(0) items. Two cores are
**_isocores_** if they are isomorphic, i.e. they comprise identical LR(0) item sets.

An **_LR(1) item set_**, also known as a **_kernel_**, is simply a set of LR(1) items, also known as
**_kernel items_**. Two kernels are **_isokernels_** if they are isomorphic, i.e. they comprise
identical LR(1) item sets. A kernel can be mapped to a core by extracting the LR(0) items from all
all LR(1) items. It is possible (and common in canonical LR(1) parsers) for non-isokernels to map to
isocores.

Parser generators have long supported grammar disambiguation via precedence and associativity. For
example `mul` has higher precedence than `add`, both of which are left-associative. `hocc` differs
from most parser generators in that precedences comprise an explicit optionally-disjoint directed
acyclic graph, rather than a mostly implicit single linear precedence order. Furthermore, since
`hocc` supports neutral associativity (`%precedence` in
[`bison`](https://www.gnu.org/software/bison/manual/bison.html#Precedence-Decl)), non-associativity
is of limited practical use (`%nonassoc` in [YACC](https://en.wikipedia.org/wiki/Yacc)-family parser
generators). These deviations from the status quo avoid masking grammar flaws and increase
specification precision.

### Work queue processing

The full details of the [canonical LR(1)](https://en.wikipedia.org/wiki/Canonical_LR_parser) parser
generation algorithm are beyond the scope of this report, but it is important to describe 1) the
iterative work queue process by which a grammar specification is converted to a state machine, and
2) how isocores play into the state generation process.

The work queue manages incremental state set creation. Compatible states which are merged can in
turn affect later compatibility test results for IELR(1) (and PGM(1)). No effort is made to puzzle
together isocores via optimal merging order, but since merging order can dramatically impact the
total number of work queue insertions, care is taken to insert at the front versus back of the work
queue in such a way as to process states in an approximately breadth-first order rather than
depth-first.

Once the work queue is seeded with start states, states are consumed from the head of the work queue
and processed until none remain, i.e. a fixpoint has been reached. The work queue contains each
_distinct_ state exactly once, over the entire duration of work queue processing. The end goal is to
traverse the entirety of the resulting state machine once. What makes a state distinct is critical
to understanding the work queue behavior. If a kernel compatibility test determines that two
non-isokernels are compatible, the merged result is distinct from at least one of the input
isokernels, even though the state number stays the same. A state number may ephemerally correspond
to a series of distinct states, and although some of those ephemeral states may never be processed
by the work queue, the last one certainly will be. Given this understanding, the work queue
insertion regimen is straightforward:

- A state that is not the result of a merge is pushed onto the back of the work queue unless already
  present in the work queue.
- A state that is the result of merging two non-isokernels is pushed onto the front of the work
  queue unless already present in the work queue.

### State generation/merging

State generation begins with the grammar's start symbol(s). A pseudo-start symbol and kernel item is
wrapped around each start symbol, always with a kernel of the form `Start' ::= · Start "⊥", {"ε"}`,
e.g. `Answer' ::= · Answer "⊥", {"ε"}` in the example grammar. Multiple computations lead to a fully
defined state, namely closing on the **_added set_**, which is the set of productions reachable from
the kernel without advancing the input, and then computing the **_goto set_** for each symbol to the
right of a dot. These goto sets define kernels of other states, in some cases being merged into
existing compatible states, and in other cases defining distinct states not yet encountered during
state machine construction.

For the example grammar, the pseudo-start symbol results in the following state, which is inserted
into both the work queue and the state set:

```
    State 0
        Kernel
            [Answer' ::= · Answer "⊥", {"ε"}]
        Added
            [Expr ::= · Expr MulOp Expr, {"*", "/", "+", "-", EOI}] prec mul
            [Expr ::= · Expr AddOp Expr, {"*", "/", "+", "-", EOI}] prec add
            [Expr ::= · INT, {"*", "/", "+", "-", EOI}]
            [Answer ::= · Expr EOI, {"⊥"}]
        Actions
            INT : ShiftPrefix 1
        Gotos
            Expr : 2
            Answer : 3
```

Note that every symbol immediately to the right of a dot is represented in the **_actions_** or
**_gotos_**, for tokens and nonterms, respectively. Consider `Expr`, which is to the right of
multiple dots. Each item in the goto set for `Expr` is created by advancing the dot one position
relative to the item in this state. The following kernel results:

```
    [Expr ::= Expr · MulOp Expr, {"*", "/", "+", "-", EOI}] prec mul
    [Expr ::= Expr · AddOp Expr, {"*", "/", "+", "-", EOI}] prec add
    [Answer ::= Expr · EOI, {"⊥"}]
```

A work queue state is processed by 1) popping it from the front of the work queue, and 2) computing
the actions and gotos, such that work queue and state set insertion may result.

Canonical LR(1) isocore merge compatibility is maximally strict, in that kernels must be isokernels
in order to be merged. Merging two isokernels results in an isokernel; thus merging is a no-op in
practice. Such strict compatibility commonly results in nearly identical states which could in
practice be merged without changing the grammar recognized by the state machine. IELR(1) eliminates
(nearly) all such redundancy.

## LALR(1)

The LALR(1) algorithm bears mentioning primarily because it is used in the IELR(1) algorithm as the
foundation on which inadequacy attribution metadata are computed. As described in more detail later,
IELR(1) conceptually patches up all inadequacies of an LALR(1) parser to make it recognize the same
grammar as the corresponding canonical LR(1) parser. That means starting with an LALR(1) parser,
analyzing inadequacies, then generating the IELR(1) parser.

As described earlier, canonical LR(1) isocore compatibility requires that corresponding kernel items
have identical follow sets, i.e. isocores are compatible only if they are isokernels. LALR(1) goes
to the other extreme, and treats all isocores as compatible, regardless of follow sets. Given a
functioning canonical LR(1) parser generator, LALR(1) parser generation is trivial to add; isocore
compatibility tests always return true.

The well-known **_mysterious new conflicts_** caused by LALR(1) state merging are always
**_reduce-reduce conflicts_**, but it is also possible for state merging to create **_mysterious
invasive conflicts_** that are caused by merging **_shift-reduce conflicts_** into states which
would otherwise have performed a reduce action. Furthermore, it is possible to create **_mysterious
mutated conflicts_** by merging multiple reduce-reduce conflicts that have distinct resolutions.

The PGM(1) algorithm suffices to avoid mysterious new conflicts. However, input grammars commonly
rely on precedence/associativity to resolve LR(1) ambiguities. Both LALR(1) and PGM(1) can introduce
invasive/mutated conflicts, i.e. they can generate parsers that behave differently than the resolved
LR(1) parser. Such parsers are **_LR(1)-inadequate_**.

## IELR(1)

The IELR(1) algorithm is substantially more complicated than canonical LR(1) or LALR(1). The IELR(1)
paper [^denny2010] describes six stages as implemented by `bison`, three of which directly
correspond to stages in `hocc`:

1. Generate the LALR(1) state machine, with conflict resolution disabled so that later analysis can
   discover and analyze ambiguities.
2. Trace lanes backward from conflict-containing states and annotate state transitions with conflict
   contribution metadata that enable inadequacy-eliminating isocore compatibility testing.
3. Compute the IELR(1) state machine fixpoint, using metadata from (2) to attach metadata to IELR(1)
   states, use those metadata to inform isocore compatibility, and propagate derivative metadata as
   states are created.

The interaction between metadata pre-computed in (2) and dynamically propagated in (3) is
surprisingly subtle, but implementation requires only a modest amount of code given valid data
abstractions. The remainder of this section describes these two phases in detail.

### Terminology

A characteristic [finite state machine](https://en.wikipedia.org/wiki/Finite-state_machine)
generated by any of the LR(1)-family algorithms is a [pushdown
automaton](https://en.wikipedia.org/wiki/Pushdown_automaton). Such state machines maintain a stack
while traversing a [digraph](https://en.wikipedia.org/wiki/Directed_graph). The `hocc` code uses the
following terminology related to state machine digraphs:

- **_state_**: Vertex, node
- **_transit_**: Arc, directed edge (transition), where there is a shift or goto connecting the
  source to the destination
- **_ipred_**: Immediate predecessor, transit source relative to destination
- **_isucc_**: Immediate successor, transit destination relative to source

Each state that contains conflicting actions for the same input symbol that are not resolved to a
shift action (i.e. the conflict remains unresolved or the resolution is a reduce action) is a
starting point for tracing backward through every **_lane_**, some or all of which may contribute to
conflicts. From the perspective of the conflict state, an individual lane is a linear (i.e.
non-forking but potentially cyclic) predecessor path back to a start state; the lane may extend
forward past the conflict state either via shift or via goto, but these extensions are irrelevant to
the conflict unless they participate in a cycle back to the conflict state. Cycles pose
complications with regard to lane tracing, as do acyclic diamond-pattern fork/join patterns, whether
sequential or nested. Such topologies can induce an infinitude of lanes, which is why analyses based
on lane tracing must be able to reach closure while tracing each lane segment only once.

Lane tracing matters to inadequacy elimination because each shift and reduce contribution
(**_contrib_**) is attributed to a transit &mdash; an **_attrib_** (attribution). Removal of an
inadequacy entails splitting portions of two or more merged lanes such that attribs are fully
partitioned with respect to each conflict-containing state. Each attrib is specific to a conflict
state, symbol, and transit. Thus an attrib comprises a (conflict state, symbol, conflict
manifestation, isucc LR(1) itemset, contrib) tuple.

### Lane tracing

The goal of lane tracing is to annotate transits with conflict attribution metadata, such that
attributions can be propagated forward through all relevant lanes during state machine fixed-point
iteration. These **_annotations_** are **_kernel attribs_** keyed by conflict state kernel items
(one follow set symbol per key).

For example, consider this transcription of Pager's G2 grammar [^pager1977]:

```hocc
hocc
    token At
    token Bt
    token Ct
    token Dt
    token Et
    token Tt
    token Ut
    token EOI

    start Sn ::= Xn EOI

    nonterm Xn ::=
      | At Yn Dt
      | At Zn Ct
      | At Tn
      | Bt Yn Et
      | Bt Zn Dt
      | Bt Tn

    nonterm Yn ::=
      | Tt Wn
      | Ut Xn

    nonterm Zn ::= Tt Ut

    nonterm Tn ::= Ut Xn At

    nonterm Wn ::= Ut Vn

    nonterm Vn ::= epsilon
```

As analyzed by `hocc` in LALR(1)/IELR(1) mode, this results in the following state subgraphs
(inconsequential states omitted for brevity):

```
        LALR(1)                           IELR(1)
          ___                               ___
         |   |                             |   |
    /----| 0 |----\                   /----| 0 |----\
   /     |___|     \                 /     |___|     \
  |                 |               |                 |
 _v_      ___      _v_             _v_      ___      _v_
|   |--->|   |<---|   |           |   |--->|   |<---|   |
| 1 |    | 6 |    | 2 |           | 1 |    | 6 |    | 2 |
|___|<---|___|--->|___|           |___|<---|___|--->|___|
  |                 |               |                 |
  |       ___       |              _v_               _v_
   \     |   |     /              |   |             |   |
    \--->| 5 |<---/               | 5₀|             | 5₁|
         |___|                    |___|             |___|
           |                        |                 |
          _v_                      _v_               _v_
         |   |                    |   |             |   |
         |15 |                    |15₀|             |15₁|
         |___|                    |___|             |___|
```

There are four acyclic lanes in the LALR(1) state graph which can be traced backward from state 15:
0→1→5→15, 0→1→6→2→5→15, 0→2→5→15, and 0→2→6→1→5→15. Additionally, there is an infinitude of cyclic
lanes, e.g. 0→1→{6→1}⁺→5→15 and 0→1→{6→2→6→1}⁺→5→15. This grammar lacks chained and/or nested
fork/join topologies, which would combinatorially induce lanes even in the absence of cycles.

All depicted transits except for 0→1 and 0→2 initially receive annotations, but most of the
annotations are useless because they cannot lead to state splitting, and can therefore optionally be
filtered out prior to state closure.

```
annotations=[
    ({src=1; dst=5}, [
        [Xn ::= At · Yn Dt, {Dt}] = [
            {conflict_state_index=15;
              symbol_index=5 (Dt);
              conflict={Reduce [Zn ::= Tt Ut]; Reduce [Vn ::= epsilon]};
              isucc_lr1itemset=[
                [Yn ::= Tt · Wn, {Dt}]
              ];
              contrib={Reduce [Vn ::= epsilon]}}
          ]
      ])
    ({src=2; dst=5}, [
        [Xn ::= Bt · Zn Dt, {Dt}] = [
            {conflict_state_index=15;
              symbol_index=5 (Dt);
              conflict={Reduce [Zn ::= Tt Ut]; Reduce [Vn ::= epsilon]};
              isucc_lr1itemset=[
                [Zn ::= Tt · Ut, {Dt}]
              ];
              contrib={Reduce [Zn ::= Tt Ut]}}
          ]
      ])
    ({src=5; dst=15}, [
        [Yn ::= Tt · Wn, {Dt}] = [
            {conflict_state_index=15;
              symbol_index=5 (Dt);
              conflict={Reduce [Zn ::= Tt Ut]; Reduce [Vn ::= epsilon]};
              isucc_lr1itemset=[
                [Wn ::= Ut · Vn, {Dt}]
              ];
              contrib={Reduce [Vn ::= epsilon]}}
          ]
        [Zn ::= Tt · Ut, {Dt}] = [
            {conflict_state_index=15;
              symbol_index=5 (Dt);
              conflict={Reduce [Zn ::= Tt Ut]; Reduce [Vn ::= epsilon]};
              isucc_lr1itemset=[
                [Zn ::= Tt Ut ·, {Dt}]
              ];
              contrib={Reduce [Zn ::= Tt Ut]}}
          ]
      ])
  ]
```

The G2 grammar has no shift action involved in the conflict, which makes useless annotation
filtering simpler. Consider state 6 in the set of [all annotations](G2_all_annotations.txt). There
are two annotations on 1→6, and two more on 2→6, but all four attribs contain the same contrib. No
matter how those attribs are partitioned by state splitting, the contrib will be either unchanged or
completely absent, i.e. state splitting cannot remove a conflict for the lanes passing through state
6 by splitting its ipreds.

A conflict involving a shift action is more complicated to handle, because *all* lanes are
implicitly implicated in the shift action. Even if a lane makes no reduce contributions, it is still
implicated in the shift action, so the "completely absent" splitting outcome can potentially
eliminate inadequacies. This situation requires non-local graph analysis because lanes that make no
reduce action contribution can converge anywhere in the transitive predecessor graph.

The useful G2 grammar annotations indicate that states 5 and 15 must be split in order to remove
inadequacies. Following are lightly edited excerpts from [LALR(1)](G2_lalr1.txt) and
[IELR(1)](G2_ielr1.txt) `hocc` reports showing the state splits.

```
             LALR(1)

 ____________State_5_____________
| Kernel                         |
|     [Yn ::= Tt · Wn, {Dt, Et}] |
|     [Zn ::= Tt · Ut, {Ct, Dt}] |
| Added                          |
|     [Wn ::= · Ut Vn, {Dt, Et}] |
| Actions                        |
|     Ut : ShiftPrefix 15        |
| Gotos                          |
|     Wn : 16                    |
|________________________________|
                |
                |
                v
 ____________State_15____________
| Kernel                         |
|     [Zn ::= Tt Ut ·, {Ct, Dt}] |
|     [Wn ::= Ut · Vn, {Dt, Et}] |
| Added                          |
|     [Vn ::= ·, {Dt, Et}]       |
| Actions                        |
|     Ct : Reduce Zn ::= Tt Ut   |
|     Dt : CONFLICT              |
|          Reduce Zn ::= Tt Ut   |
|          Reduce Vn ::= epsilon |
|     Et : Reduce Vn ::= epsilon |
| Gotos                          |
|     Vn : 22                    |
|________________________________|


                                    IELR(1)

 _____________State_5₀_______________     ____________State_5₁_______________
| Kernel                             |   | Kernel                           |
|     [Yn ::= Tt · Wn, {Dt}]         |   |     [Yn ::= Tt · Wn, {Et}]       |
|     [Zn ::= Tt · Ut, {Ct}]         |   |     [Zn ::= Tt · Ut, {Dt}]       |
| Added                              |   | Added                            |
|     [Wn ::= · Ut Vn, {Dt}]         |   |     [Wn ::= · Ut Vn, {Et}]       |
| Actions                            |   | Actions                          |
|     Ut : ShiftPrefix 15₀           |   |     Ut : ShiftPrefix 15₁         |
| Gotos                              |   | Gotos                            |
|     Wn : 16₀                       |   |     Wn : 16₀                     |
| Conflict contributions             |   | Conflict contributions           |
|     [Yn ::= Tt · Wn, {Dt}]         |   |     [Zn ::= Tt · Ut, {Dt}]       |
|         15 : Reduce Vn ::= epsilon |   |         15 : Reduce Zn ::= Tt Ut |
|____________________________________|   |__________________________________|
                 |                                       |
                 |                                       |
                 v                                       v
 ____________State_15₀_______________     ____________State_15₁_____________
| Kernel                             |   | Kernel                           |
|     [Zn ::= Tt Ut ·, {Ct}]         |   |     [Zn ::= Tt Ut ·, {Dt}]       |
|     [Wn ::= Ut · Vn, {Dt}]         |   |     [Wn ::= Ut · Vn, {Et}]       |
| Added                              |   | Added                            |
|     [Vn ::= ·, {Dt}]               |   |     [Vn ::= ·, {Et}]             |
| Actions                            |   | Actions                          |
|     Ct : Reduce Zn ::= Tt Ut       |   |     Dt : Reduce Zn ::= Tt Ut     |
|     Dt : Reduce Vn ::= epsilon     |   |     Et : Reduce Vn ::= epsilon   |
| Gotos                              |   | Gotos                            |
|     Vn : 22₀                       |   |     Vn : 22₀                     |
| Conflict contributions             |   | Conflict contributions           |
|     [Wn ::= Ut · Vn, {Dt}]         |   |     [Zn ::= Tt Ut ·, {Dt}]       |
|         15 : Reduce Vn ::= epsilon |   |         15 : Reduce Zn ::= Tt Ut |
|____________________________________|   |__________________________________|
```

As mentioned earlier, lanes may contain cycles (conceptually an infinite set of lanes with [0..∞]
cycle transits), and lanes may fork/join (combinatorial explosion of lanes), which means that lanes
cannot be iteratively annotated. The tractable approach is to simultaneously trace all lanes passing
through each relevant transit by traversing backward through transits until no new annotation is
added. Each iteration of the annotation closure function traverses a transit from a state to its
ipred, such that data from one or more previously traversed successor transits are propagated
backward through one or more lanes, i.e. per transit lane context (**_lanectx_**) backward
propagation.

Each lanectx comprises a map of zero or more lane **_traces_**, where each map key is a (conflict
state, symbol, conflict manifestation, action) tuple, and map values are in turn M:N maps that
associate transit source/destination LR(1) items. Note that a trace key/value pair may represent
multiple lanes, because multiple conflict state kernel items can induce the same added ε production.
Furthermore, note that a lanectx generates annotations only if it contains traces, and since traces
are transitively based on those of successors, it is possible for tracing to terminate before
reaching a start state, as for the G2 grammar above.

Lane tracing starts at each conflict-containing state, with traces for all reduce actions implicated
in conflicts. At each annotation closure iteration, a lanectx is computed based on a successor
lanectx. The basic idea at each iteration is to move the dot in each traced LR(1) item one position
to the left; in the case where the dot is already at position 0 (i.e. the item is in the added set),
attempt to trace into generating kernel items.

The critical section for IELR(1) lies in repeatedly computing the **_leftmost transitive closure_**
of a state's kernel items given a LHS symbol and a lookahead symbol. This computation recurses
backward through a state's added items to determine which kernel items are implicated in lanes for
the conflict being traced. The naíve approach to this computation suffices, but for complicated
grammars memoization is a superlinear optimization that can improve overall performance by over 10X.

The precise details of trace initialization are intricate enough that the `hocc`
[implementation](https://github.com/BranchTaken/Hemlock/tree/main/bootstrap/bin/hocc/) serves as a
clearer explanation than would further verbiage. That said, it is worth calling out that lane
tracing operates on kernel items rather than kernels as a whole, which can be especially confusing
when traces intertwine as mentioned *vis a vis* ε productions.

### State machine fixpoint computation

Each step of the state machine fixpoint computation for IELR(1) is structurally very similar to the
approach taken for LALR(1), PGM(1), and canonical LR(1). All of the algorithms rely on isocore
compatibility testing and merging, but IELR(1) is more complicated than the other algorithms in two
ways. First, compatibility testing must reference lane tracing metadata that are attached to the
isocores, and second, merging isocores requires merging the attached lane tracing metadata. Thus
IELR(1) does not operate on bare goto sets and states, rather on **_goto nubs_** and **_state
nubs_**, both of which carry kernel attribs.

What metadata actually flow through the state machine during fixpoint computation, and how far do
they propagate? This is perhaps the most confusing part of IELR(1) implementation. Attrib flow
through the state graph is disjoint &mdash; each attrib flows through a single transit, i.e. from an
ipred to a state nub. In other words, the attribs that accumulate in a state nub matter to isocore
compatibility testing, but they do not flow to isuccs. Attribs are introduced afresh for each
transit by initializing each goto nub with the transit's kernel attribs attached. Given a goto nub
that is incompatible with existing state nubs (if any), the goto nub is converted to a state nub.
If, on the other hand, the goto nub is compatible with an existing state nub, the goto nub is merged
into the state nub, including its kernel attribs.

## Remerging

Redundant states can arise during IELR(1) state machine closure because `hocc` makes no effort to
exclude to-be-orphaned states from consideration during isocore compatibility testing. Instead it
implements a limited form of state remerging that iteratively merges states with isomorphic isucc
graphs. Specifically, two isocoric states are remergeable if for all paired out transits one of the
following holds:

- The out transits are identical.
- The out transits are self-cyclic.

Iterative application of state remerging in practice works backward through the state graph, because
remerging isocoric states' successors may enable subsequent remerging.

Although remerging was initially motivated by IELR(1) in `hocc`, it also minorly benefits PGM(1),
and majorly benefits canonical LR(1). Given the same grammar, canonical LR(1) tends to generate
roughly ten times more states than does LALR(1)/PGM(1)/IELR(1). Initial results indicate that
remerging reduces that from a factor of ~10 to a factor of ~4. For example, consider `hocc` results
for the `Gpic` grammar originally analyzed in the IELR(1) paper [^denny2010].

| Algoritm  | # of states | Ratio |
|:----------|------------:|------:|
| LALR(1)   |         423 |  1___ |
| PGM(1)    |         423 |  1___ |
| PGM(1)\*  |         426 |  1.01 |
| IELR(1)   |         428 |  1.01 |
| IELR(1)\* |         437 |  1.03 |
| LR(1)     |        1506 |  3.56 |
| LR(1)\*   |        4834 | 11.43 |

\* &mdash; no remerging

Interestingly, `bison` generates 428 states for `Gpic` even though it lacks a remerging
implementation. `bison` presumably omits remergeable states by generating states in a different
order, but to my knowledge there is no tractable approach which universally eliminates remerging
utility.

## Performance

The `hocc` implementation of IELR(1) is dramatically slower than that of `bison`. The following wall
clock times for the `Gpic` grammar are representative. Both `hocc` and `bison` are configured to
emit no reports nor generated code, and the reported numbers are the best of three runs on an AMD
EPYC 7742 CPU running Ubuntu Linux 24.04, using OCaml 5.2.0 with flambda enabled for `hocc` versus
the vendor-supplied `bison` 3.8.2.

| Algorithm | hocc   | bison   |
|:----------|-------:|--------:|
| LALR(1)   |  0.929 |   0.017 |
| PGM(1)    |  1.487 | &mdash; |
| IELR(1)   | 13.623 |   0.029 |
| LR(1)     | 10.011 |   1.527 |

`bison` is a [C](https://en.wikipedia.org/wiki/C_(programming_language)) application that relies on
flat and linearly allocated mutable global data structures, whereas `hocc` is an
[OCaml](https://ocaml.org/) application that relies on high-level purely functional data structures.
`hocc` uses maps in many places where a custom low-level data structure would perform much better,
albeit at the cost of code clarity and maintainability. More critically, `hocc` is implemented on
top of the `Basis` library, which is an OCaml-native standard library intended to correspond closely
to the standard library designed for the [Hemlock](https://github.com/BranchTaken/Hemlock)
programming language. OCaml unfortunately provides 63-bit integers as its default high-performance
integer type, but Hemlock's design calls for 64-bit integers. As a consequence, `Basis` ubiquitously
uses OCaml's boxed 64-bit integers, which imposes an epic load on OCaml's automatic memory
management subsystem. These `Basis`-related implementation quirks could easily account for ~10X of
the `hocc`-`bison` performance gap, saying nothing of the high-level data structure overhead.
Nonetheless, current performance meets practical requirements for Hemlock bootstrapping, and further
optimization is left as an exercise for the aspirational Hemlock-native `hocc` implementation.

A basic IELR(1) implementation can get away without two of the refinements described in this report,
namely useless annotation filtering and leftmost transitive closure memoization. That said,
anecdotal evidence based on processing the `Lyken` grammar (an abandoned research language) suggests
that these refinements can matter for antagonistic inputs. The `Lyken` grammar was developed using
an [implementation of the PGM(1) algorithm](https://github.com/MagicStack/parsing), and it relied
heavily on per conflict precedence relationships to converge on a specification which ended up with
no LR(1)-relative inadequacies. But the IELR(1) annotations required to determine this are copious,
and the lanes are heavily intertwined. Absent either refinement, IELR(1) processing requires nearly
30 GiB of RAM and approximately 16 hours of wall time. Useless annotation filtering reduces this to
13 GiB and 14 hours. Leftmost transitive closure memoization has no significant impact on memory
usage, and further reduces wall time to approximately 1 hour. Performance impacts for less tortuous
grammars range from neutral to modest speedup, e.g. ~1.25X for `Gpic`.

## Conclusion

This report is intended to help others bypass the morass that IELR(1) implementation turned out to
be in the context of `hocc`. My initial intention regarding IELR(1) was to demonstrate that it has
no practical utility relative to PGM(1), but careful rereading of the IELR(1) paper convinced me
otherwise. Full understanding was elusive, and the `hocc` implementation is in large part a
re-invention given the benefits of an imperfectly understood paper and an existence proof in the
form of `bison`.

Although `hocc` is primarily a Hemlock-targeting parser generator, it is also generally useful for
grammar experimentation/validation due to its clean syntax when omitting embedded reduction code.
More importantly in the context of this report, `hocc` serves as a straightforward reference
implementation of IELR(1), even to implementers who are unfamiliar with OCaml. Choice of data
structures is key to implementation, and OCaml record syntax is self-evident to experienced
programmers, e.g. `Prod.t` as defined in the `prod.mli` interface file:

```ocaml
type t = {
  index: Index.t;
  (** Unique production index. *)

  lhs_index: SymbolIndex.t;
  (** LHS symbol index. *)

  rhs_indexes: SymbolIndex.t array;
  (** RHS symbol indexes in left-to-right order. *)

  prec: Prec.t option;
  (** Precedence, if any. This is denormalized with respect to the hocc specification, such that it
      is [Some p] regardless of whether precedence is specified for just this prod versus all of the
      nonterm (LHS symbol) prods. *)

  stmt: Parse.nonterm_prod option;
  (** Declaration AST. *)

  callback: Callback.t;
  (** Reduction callback code. *)
}
```

Was the effort required to implement IELR(1) worthwhile? For the Hemlock project, almost certainly
not &mdash; myriad false starts imposed an extreme opportunity cost. But IELR(1) is a powerful tool
with practical application, and I hope to see it broadly implemented over the coming years. In the
meanwhile Hemlock's grammar specification development will leverage IELR(1), first as a safety tool
during prototyping, and later to assure that no LR(1)-relative inadequacies survive in the grammar's
stable form even when generated by the LALR(1) algorithm. PGM(1) is capable of this role only if
precedence/associativity are completely avoided, and such an austere grammar development environment
is unacceptable to me. I look forward to routinely pulling IELR(1) out of my toolbox and crafting
grammars with it.

## Citations

[^evans2024]:
    Jason Evans,
    “IELR(1) as Implemented by `hocc`”,
    BranchTaken LLC,
    [https://branchtaken.com/reports/ielr1.html](https://branchtaken.com/reports/ielr1.html),
    July 2024.

[^deremer1969]:
    Frank DeRemer,
    “Practical Translators for LR(k) languages”,
    Ph.D Dissertation,
    Department of Electrical Engineering,
    Massachusetts Institute of Technology, Cambridge, 1969.

[^knuth1965]:
    Donald Knuth,
    “On the Translation of Languages from Left to Right”,
    Information and Control 8(6):607–639, July 1965.

[^pager1977]:
    David Pager,
    “A Practical General Method for Constructing LR(k) Parsers”,
    Acta Informatica 7:249-268, 1977.

[^fpottier]:
    François Pottier and Yann Régis-Gianas,
    “Menhir LR(1) Parser Generator,”
    [http://gallium.inria.fr/~fpottier/menhir/](http://gallium.inria.fr/~fpottier/menhir/)

[^deremer1969]:
    Frank DeRemer,
    “Practical Translators for LR(k) languages”,
    Ph.D Dissertation,
    Department of Electrical Engineering,
    Massachusetts Institute of Technology, Cambridge, 1969.

[^denny2010]:
    Joel E. Denny and Brian A. Malloy,
    “The IELR(1) algorithm for generating minimal LR(1) parser tables for non-LR(1) grammars with
    conflict resolution”,
    Science of Computer Programming, 75(11):943-979, 2010.
