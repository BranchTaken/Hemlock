# hocc

`hocc` is an [LR(1) parser generator](https://en.wikipedia.org/wiki/Canonical_LR_parser). Its name
carries on a long tradition, to wit:

- [`yacc`](https://en.wikipedia.org/wiki/Yacc) stands for "Yet Another Compiler Compiler". Clearly
  the name derives from "yack", as in, "Chuck's dinner didn't sit well and he yacked it."
- `hocc` stands for "Hardly Original Compiler Compiler". The name derives from "hock", as in, "Hank
  hocked a loogie."

Both programs interpret high-level human-written parser descriptions and produce output unfit for
human consumption. However `hocc` has several distinguishing features relative to `yacc`, aside from
interoperating with [Hemlock](https://github.com/BranchTaken/Hemlock) rather than
[C](https://en.wikipedia.org/wiki/The_C_Programming_Language).

- `hocc` generates LR(1) rather than [LALR(1)](https://en.wikipedia.org/wiki/LALR_parser) parsers,
  optionally using a behavior-preserving compaction algorithm [^denny2010] that reduces the state
  machine size relative to the canonical LR(1) algorithm [^knuth1965], as well as unreachable state
  garbage collection and equivalent state remerging.
- `hocc`'s precedence facilities are more precise and easier to use without inadvertently masking
  grammar ambiguities. Whereas `yacc` supports only a single linear precedence ordering, `hocc`
  supports arbitrarily many directed acyclic precedence graphs. Given this more powerful conflict
  resolution mechanism, `hocc` refuses to generate parsers for ambiguous grammars.
- `hocc` supports an automated error recovery algorithm [^diekmann2020] based on minimum-cost repair
  sequences. [XXX Not implemented.]

## Command usage

`hocc <parameters>`

Parameters:

- `-h[elp]`: Print command usage and exit.
- `-v[erbose]`: Print progress information during parser generation.
- `-txt` | `-text`: Write a detailed automaton description in plain text format to
  `<dstdir>/hocc/<module>.txt`.
- `-hmh` | `-hocc`: Write a complete grammar specification in `hocc` format to
  `<dstdir>/hocc/<module>.hmh`, but with all non-terminal types and reduction code omitted.
- `-a[lgorithm] <alg>`: Use the specified `<alg>`orithm for generating an automaton. Defaults to
  `lr1`.
  + `lr1`: Canonical LR(1) automaton [^knuth1965].
  + `ielr1`: Compact LR(1) automaton [^denny2010] that recognizes valid inputs identically to `lr1`
    automatons, even in the presence of precedence-resolved ambiguities.
  + `pgm1`: Compact LR(1) automaton [^pager1977] that recognizes valid inputs identically to `lr1`
    automatons, provided there were no precedence-resolved ambiguities in the grammar specification.
  + `lalr1`: LALR(1) automaton [^deremer1969].
- `-r[esolve] (yes|no)`: Control whether conflict resolution is enabled. Defaults to `yes`.
- `-[re]m[erge] (yes|no)`: Control whether remerging equivalent split states is enabled. Defaults to
  `yes`.
- `-hm` | `-hemlock`: Generate a Hemlock-based parser implementation and write it to
  `<dstdir>/<module>.hm[i]`.
- `-ml` | `-ocaml`: Generate an OCaml-based parser implementation and write it to
  `<dstdir>/<module>.ml[i]`. This is brittle functionality intended only for Hemlock
  bootstrapping.
- `-s[rc] <src>`: Path and module name of input source, where inputs match `<src>.hmh[i]` and
  `<src>` comprises the source directory and module name, `[<srcdir>/]<module>`.
- `-d[stdir] <dstdir>`: Path to directory in which to place generated output, such that output file
  paths match `<dstdir>/[hocc/]<module>.*`. Defaults to `<srcdir>`.

Syntax errors in the input file may prevent file generation. Specification errors do not prevent
report and graph file generation, but all specification errors must be resolved for parser
generation to succeed. Some syntax errors in the embedded Hemlock code may pass through `hocc`
unnoticed.

Example invocations:

- `hocc -hm -src Parser`: Read `Parser.{hmh,hmhi}` and generate `Parser.{hm,hmi}`.
- `hocc -verbose -text -hocc -hemlock -src src/Parser -d obj`: Verbosely read
  `src/Parser.{hmh,hmhi}` and generate `obj/hocc/Parser.{txt,hmh}` and `obj/Parser.{hm,hmi}`.

## Parser specification

The `hocc` specification grammar is layered onto Hemlock's grammar via the addition of several
contextual keywords and one operator:

- Parser: `hocc`
- Symbols:
  + [Tokens](#tokens): `token`
  + [Non-terminals](#non-terminals): `nonterm`, `start`, `::=`
  + [Productions](#productions): `epsilon`
- [Precedence](#precedence): `neutral`, `left`, `right`, `nonassoc`, `prec`

A valid parser specification is encapsulated by a `hocc` statement and describes how to construct a
parse tree of symbols. `token` statements correspond to terminal symbols, i.e. leaf nodes in the
parse tree, whereas non-terminal `start`/`nonterm` statements correspond to internal nodes in the
parse tree. A parse tree always has a non-terminal start symbol at its root. Non-terminals have
associated production patterns that specify how to construct non-terminal nodes during post-order
tree construction. Precedences may be declared via the `neutral`/`left`/`right`/`nonassoc`
statements and symbols may be assigned those precedences for use during conflict resolution via the
`prec` reference clause.

The `hocc` keyword introduces the `hocc` statement, and it cannot be otherwise used outside the
`hocc` statement. There are no other syntactic restrictions of note with regard to the keywords and
operator.

The following subsections document specification semantics. See the `hocc` [grammar](#grammar)
specification for comprehensive syntax details.

### Tokens

Token identifiers match `[_]*[A-Z][A-Za-z0-9_']*` in conformance with Hemlock's capitalized
identifier syntax. By convention the `hocc` documentation restricts token identifiers to
`[A-Z][A-Z0-9_]*` to distinguish tokens from non-terminals, but other conventions can work just as
well.

```hocc
hocc
    token SOME_TOKEN
```

In practice, many token types serve as punctuation and have invariant contents. These token types
can be declared with a string alias, which can then be used in production patterns.

```hocc
hocc
    token LPAREN "("
```

Tokens with variant contents must have an optionally qualified declared data type.

```hocc
hocc
    token T of t
    token U of M.t
    token V of M.N.t
```

Tokens may be assigned [precedence](#precedence) to aid in conflict resolution.

```hocc
hocc
    left p
    token X prec p
```

### Non-terminals

Non-terminal identifiers match `[_]*[A-Z][A-Za-z0-9_']*` in conformance with Hemlock's capitalized
identifier syntax. By convention the `hocc` documentation restricts non-terminal identifiers to
`[A-Z][A-Za-z0-9]*` to distinguish non-terminals from tokens, but other conventions can work just as
well.

```hocc
hocc
    nonterm SomeNonterm ::= # [...]
    start SomeStart ::= # [...]
```

Note that each `start` symbol is augmented with a wrapper symbol that facilitates parser reduction
actions on the corresponding start symbol. The wrapper's name is generated by appending a `'` to the
start symbol's name. For example, `start S ...` implies the `S'` wrapper symbol. As such, `start S
...` and `nonterm S' ...` cannot coexist.

Just as for tokens, non-terminals with variant contents must have a declared data type. A parser
which universally utilizes implicitly typed non-terminals does not construct a parse tree, but it
may still be useful as a recognizer, or as an abstract grammar specification which `hocc` can verify
without generating a parser.

```hocc
hocc
    nonterm SomeNonterm of Node.t ::= # [...]
    start SomeStart of Node.t ::= # [...]
```

Non-terminals may be assigned [precedence](#precedence) to aid in conflict resolution, with the
restriction that non-terminal precedence assignment is mutually exclusive with per production
precedence assignment for the non-terminal's productions.

```hocc
hocc
    neutral p
    nonterm SomeNonterm of Node.t prec p ::= # [...]
    start SomeStart of Node.t prec p ::= # [...]
```

#### Productions

Each non-terminal symbol has one or more associated productions, which denote patterns for combining
symbols to construct a symbol during post-order parse tree construction.

```hocc
hocc
    token SOME_TOKEN
    nonterm SomeNonterm ::= SOME_TOKEN
    start SomeStart ::= SomeNonterm SOME_TOKEN
```

As a special case, the `epsilon` keyword denotes an empty pattern which can be used to construct a
non-terminal without combining any symbols.

```hocc
hocc
    token A
    token B
    nonterm N ::=
      | A
      | epsilon
    start S ::= N B
```

Productions may be assigned [precedence](#precedence) to aid in conflict resolution, with the
restriction that production precedence assignment is mutually exclusive with predecence assignment
for the enclosing non-terminal.

```hocc
    neutral p1
    neutral p2
    token A
    token B
    nonterm N ::=
      | A
      | B prec p1
      | epsilon prec p2
    start S ::= N B prec p1
```

All of the above examples use non-terminals lacking concrete type. Non-terminals in a runnable
parser must have concrete type, `unit` in the simplest case, as well as reduction code associated
with every production.

```hocc
hocc
    token A
    token B
    nonterm N of unit ::=
      | A
      | epsilon
      -> ()
    start S of unit ::= N B -> ()
```

Parsers which construct a parse tree may need to associate production-specific reduction code rather
than sharing the reduction code with all of a non-terminal's productions. As for Hemlock pattern
matching, all productions which share reduction code must specify equivalently typed lexical
bindings.

```hocc
hocc
    token U of Uns.t
    token PLUS "+"
    start S of Uns.t ::=
      | PLUS u1:U u2:U
      | u1:U "+" u2:U
      | u1:U u2:U _:PLUS ->
        u1 + u2
```

The following example uses richer pattern binding syntax to reduce boilerplate. Patterns support
relevant Hemlock pattern syntax, with the notable exception of type constraints, which are out of
place in `hocc` proper because it makes no attempt at typing embedded code.

```hocc
type point: point = {
    x: r64
    y: r64
  }

hocc
    token R64 of r64
    token LPAREN "("
    token RPAREN ")"
    token COMMA ","
    left pAdd
    token PLUS "+" prec pAdd
    token MINUS "-" prec pAdd
    token EOI

    nonterm Point of point ::=
      | "(" x:R64 "," y:R64 ")" -> {x; y}
      | p0:Point "+" p1:Point prec pAdd ->
        let {x=x0; y=y0} = p0
        let {x=x1; y=y1} = p1
        {x=x0+x1; y=y0+y1}
      | {x=x0; y=y0}:Point "-" {x=x1; y=y1}:Point prec pAdd ->
        {x=x0-x1; y=y0-y1}

    start S of point ::=
      | p:Point EOI -> p
```

Ordinarily, the characteristic finite state machine (CFSM) corresponding to an LR(1) grammar delays
each transition until the lookahead symbol becomes available. However this poses a challenge for
start symbols because there is no concrete lookahead symbol past the end of input. The following
invalid grammar would infinitely recurse, and `hocc` reports a conflicting action for the
`PSEUDO_END` (`"⊥"`) symbol.

```hocc
# Invalid (infinite recursion).
hocc
    token U of Uns.t
    start S of Uns.t ::=
      | u:U s:S -> u + s
      | epsilon -> 0
```

A typical solution to this challenge is to require the application to signal end of input to the
CFSM via a dedicated API. However `hocc` uses the same approach as Menhir [^fpottier] and instead
proactively (transitively) reduces when the current symbol unambiguously delimits a valid start
symbol reduction. Some start symbols may trivially meet the requirements for proactive reduction,
e.g. for a grammar which incrementally parses a file comprising newline-separated statements, each
of which is encapsulated by a start symbol. However, many grammars do need to manually incorporate
an explicit end of input token. The above grammar can be repaired as follows.

```hocc
# Valid, though right-associative.
hocc
    token U of Uns.t
    token EOI
    start S of Uns.t ::=
      | u:U s:S -> u + s
      | EOI -> 0
```

Note that the above grammar is right-associative only because the `EOI` repair is simpler to
demonstrate in that context. The following left-associative grammar is preferable in practice
because it incrementally constructs the parse tree rather than delaying all reductions until `EOI`.

```hocc
# Valid and left-associative.
hocc
    token U of Uns.t
    token EOI
    nonterm E of Uns.t ::=
      | e:E u:U -> e + u
      | epsilon -> 0
    start S of Uns.t ::=
      | e:E EOI -> e
```

### Precedence

Precedence identifiers match `[_]*[a-z][A-Za-z0-9_']*` in conformance with Hemlock's uncapitalized
identifier syntax. By convention the `hocc` documentation restricts precedence identifiers to
`p[A-Z][A-Za-z0-9]*`.

```hocc
hocc
    neutral pSomeNeutral
    left pSomeLeft
    right pSomeRight
    nonassoc pSomeNonassoc
```

A parser specification may contain conflicts wherein a parser state encodes multiple valid actions
for one or more inputs. `hocc` refuses to generate parsers which contain unresolved conflicts.
Parser specifications can often be refactored or expanded to eliminate conflicts, but such revisions
may reduce clarity and maintainability. Precedences provide a mechanism for conflict resolution,
i.e. explicit choice of actions. `hocc` attempts to resolve conflicts based on the precedences
assigned to tokens and productions.

Each production can specify its precedence, or if all of a non-terminal's productions are to have
the same precedence, the precedence can be more succinctly specified for the non-terminal as a
whole. It is an error to explicitly specify both a non-terminal's precedence and the precedence of
any of its productions.

Precedences may be defined with any of the following associativities:

- `neutral`: Do not resolve conflicts via associativity. Neutral associativity is useful for
  specifying precedence-based resolutions without inadvertently masking conflicts.
- `left`: Resolve shift/reduce conflicts by reducing. This induces left associativity, e.g.
  `2 + 3 + 4` is parsed as `(2 + 3) + 4`.
- `right`: Resolve shift/reduce conflicts by shifting. This induces right associativity, e.g.
  `2 + 3 + 4` is parsed as `2 + (3 + 4)`. All else being equal, prefer left associativity to
  minimize intermediate parser state.
- `nonassoc`: Resolve shift/reduce conflicts by removing all actions to prevent associative grammar
  constructs. This causes parse errors, e.g. at the second `+` in `2 + 3 + 4`. Avoid using this
  feature in new grammars; it is provided only to ease transcription of legacy grammars.

Sets of equivalent precedences can be defined via the `neutral`, `left`, `right`, and `nonassoc`
statements, and precedence sets may optionally be ordered relative to previously defined precedence
sets via `<` relationships, irrespective of associativity. These precedence relationships are used
to compute the transitive closure of precedence orderings. Precedences with disjoint relationships
are incomparable, i.e. they have no relative ordering. By default, all tokens and productions have a
*lack* of precedence, which is equivalent to each such token/production being assigned a unique
disjoint `neutral` precedence.

Conflicts may occur for any given input symbol between two or more actions, of which at least one is
a reduce action. Such an action set induces shift/reduce and/or reduce/reduce conflicts; by
construction shift/shift conflicts cannot occur. Given conflicting actions A and B, A "dominates" B
if A is preferred over B. For conflict resolution to succeed, one action must dominate all other
conflicting actions. The rules for conflict resolution are as follows. If none of the rules apply,
conflict resolution fails.

- If a subset of actions has higher precedence than all other actions, and the actions in the
  highest-precedence subset have equal associativity, resolve the conflict under any of the
  following conditions:
  + `neutral`: A singleton action subset dominates, i.e. a neutral-associative action only dominates
    actions of lower precedence.
  + `left`: A single reduce action dominates, i.e a single reduce action dominates zero or one shift
    action(s) of the same precedence.
  + `right`: A (single) shift action dominates, i.e. a shift action dominates zero or more reduce
    actions of the same precedence.
  + `nonassoc`: Remove all actions, i.e. cause parse errors for associative grammar constructs.

Associativity suffices for resolving simple shift/reduce conflicts as in e.g. `2 + 3 + 4`, so that
it is deterministically parsed as `(2 + 3) + 4` (`left` as in the following example specification),
`2 + (3 + 4)` (`right`), or a syntax error (`nonassoc`).

```hocc
hocc
    left add
    token PLUS prec add
    token INT of Int.t
    nonterm Expr of Int.t ::=
      | x:INT -> x
      | e0:Expr PLUS e1:Expr prec add -> Int.(e0 + e1)
    token EOI
    start S ::= Expr EOI
```

Alternatively, precedence ordering can resolve shift/reduce conflicts, though associativity is
preferable when applicable.

```hocc
hocc
    neutral add
    neutral plus < add
    token PLUS prec plus
    token INT of Int.t
    nonterm Expr of Int.t ::=
      | x:INT -> x
      | e0:Expr PLUS e1:Expr prec add -> Int.(e0 + e1)
    token EOI
    start S ::= Expr EOI
```

Precedence ordering can also resolve reduce/reduce conflicts between productions, which is beyond
the power of associativity. In the following parser specification, `MUL` has precedence over `PLUS`
due to the precedence relationship `add < mul`, so `2 + 3 * 4` is parsed as `2 + (3 * 4)`.

```hocc
hocc
    left mul
    token MUL prec mul
    left add < mul
    token PLUS prec add
    token INT of Int.t
    nonterm Expr of Int.t ::=
      | e0:Expr MUL e1:Expr prec mul -> Int.(e0 * e1)
      | e0:Expr PLUS e1:Expr prec add -> Int.(e0 + e1)
      | x:INT -> x
    token EOI
    start S ::= Expr EOI
```

Precedence relationships are optional in precedence declarations. Contrived examples follow.

```hocc
hocc
    left a
    left b < a
    left c < a
    left d < b, c # Transitive: a
    right e, f
    neutral g < d, e # Transitive: a, b, c, f
```

Precedences are bound to tokens, non-terminals, and productions using the optional `prec` reference
clause. Omitting the `prec` reference clause is equivalent to referring to a unique disjoint
`neutral` precedence. The following example demonstrates the `prec` reference clause syntax.

```hocc
hocc
    neutral p1
    left p2 < p1

    token FOO prec p1

    nonterm Bar prec p2 ::=
      | FOO
      | epsilon

    start Biz ::=
      | Bar FOO prec p1
```

The `PSEUDO_END` (`"⊥"`) token is implicitly defined with no precedence; any related conflicts must
be resolved by restructuring the grammar.

```hocc
hocc
    token PSEUDO_END "⊥"
```

## Example

The following example implements a simple mathematical expression calculator.

`Example.hmhi`:

```hocc
open import Basis

# Export the parser API so that alternatives to `calculate` can be implemented. `hocc` expands to a
# module signature.
include hocc

calculate: string -> zint
  [@@doc "Calculate the result of a simple arithmetic expression comprising non-negative integers
  and `+`, `-`, `*`, and `/` operators. Tokens must be separated by one or more spaces."]
```

`Example.hmh`:

```hocc
open import Basis

# Specify the parser. `hocc ...` expands to a module implementation, `{ ... }`.
include hocc
    left mul
    token STAR "*" prec mul
    token SLASH "/" prec mul
    nonterm MulOp of Token.t ::=
      | "*" -> STAR
      | "/" -> SLASH

    left add < mul
    token PLUS "+" prec add
    token MINUS "-" prec add
    nonterm AddOp of Token.t ::=
      | "+" -> PLUS
      | "-" -> MINUS

    token INT of Zint.t
    nonterm Expr of Zint.t ::=
      | e0:Expr op:MulOp e1:Expr prec mul ->
        match op with
          | STAR -> Zint.(e0 * e1)
          | SLASH -> Zint.(e0 / e1)
          | _ -> not_reached ()
      | e0:Expr op:AddOp e1:Expr prec add ->
        match op with
          | PLUS -> Zint.(e0 + e1)
          | MINUS -> Zint.(e0 - e1)
          | _ -> not_reached ()
      | x:INT -> x

    token EOI
    start Answer of Zint.t ::=
      | e:Expr EOI -> e

# Tokenize `s`, e.g. "2 + 3 * 4", and append an `EOI` token.
tokenize s =
    s |> String.split_rev ~f:(fn cp -> Codepoint.(cp = ' '))
      |> List.rev_filter ~f:(fn s -> not (String.is_empty s))
      |> List.rev_map ~f:fn s ->
        let open Token
        match s with
          | "*" -> STAR
          | "/" -> SLASH
          | "+" -> PLUS
          | "-" -> MINUS
          | _ -> INT (Zint.of_string s)
      |> List.push Token.EOI
      |> List.rev

# Calculate the result of the arithmetic expression expressed in `s`, e.g. "2 + 3 * 4".
calculate s =
    let {status; _} = List.fold_until (tokenize s) ~init:Start.Answer.boi ~f:fn parser tok ->
        let {status; _} as parser' = Start.Answer.next tok parser
        let done = match status with
          | Prefix -> false
          | Accept _
          | Error _ -> true
          | _ -> not_reached ()
        parser', done
    match status with
      | Accept (Answer answer) -> answer
      | Prefix _ -> halt "Partial input"
      | Error _ -> halt "Parse error"
      | _ -> not_reached ()
```

To generate Hemlock code from the above inputs, run `hocc -hm -s Example`.

## Generated API

The generated parser is encapsulated in a module with an interface similar to the following. The
interface is mainly relevant to application code which utilizes the generated parser rather than the
specification itself, with the exception that non-terminals may need to refer to the `Token.t` type.
Note that the `EPSILON` token identifier, alias `"ε"`, is reserved as the token associated with the
start state at the base of the parser stack; it remains on the stack until parsing accepts and is
therefore visible to introspection at any intermediate parse state. The `PSEUDO_END` token
identifier, alias `"⊥"`, is reserved as a terminator pseudo-token that follows start symbols;
although `"⊥"` is never constructed, it can appear in follow sets and is therefore exposed for
parser introspection purposes.

The generated parser intentionally omits support for effects in reduction code, so that intermediate
parser states can be used as persistent reusable snapshots.

```hemlock
{
    Spec = {
        Algorithm = {
            type t: t =
              | Lr1 [@doc "LR(1) algorithm."]
              | Ielr1 [@doc "IELR(1) algorithm."]
              | Pgm1 [@doc "PGM(1) algorithm."]
              | Lalr1 [@doc "LALR(1) algorithm."]

            include IdentifiableIntf.S with type t := t
          }

        algorithm: Algorithm.t
          [@@doc "Algorithm used to generate parser."]

        Assoc = {
            type t: t =
              | Left
              | Right
              | Nonassoc

            include IdentifiableIntf.S with type t := t
          }

        PrecSet = {
            type t: t = {
                index: uns # Index in `prec_sets` array.
                names: array string
                assoc: option Assoc.t
                doms: Ordset.t uns Uns.cmper_witness (* Indices in `prec_sets` array of dominator
                                                      * precedence sets. *)
              }

            include IdentifiableIntf.S with type t := t
          }

        prec_sets: array PrecSet.t
          [@@doc "Array of precedence sets, where each element's `index` field corresponds to the
          element's array index."]

        Prec = {
            type t: t = {
                name_index: uns # Index of precedence name in precedence set.
                prec_set_index: uns # Index of precedence set in `prec_sets`.
              }

            include IdentifiableIntf.S with type t := t
          }

        Prod = {
            type t: t = {
                index: uns # Index in `prods` array.
                lhs_index: uns
                rhs_indexes: array uns
                prec: option Prec.t
                callback: uns # Index of reduction callback in `Stack.Reduction.callbacks`.
              }

            include IdentifiableIntf.S with type t := t
          }

        prods: array Prod.t
          [@@doc "Array of productions, where each element's `index` field corresponds to the
          element's array index."]

        Symbol = {
            type t: t = {
                index: uns # Index in `symbols` array.
                name: string
                prec: option Prec.t
                alias: option string
                start: bool
                prods: Ordset.t Prod.t Prod.cmper_witness # empty ≡ token
                first: Ordset.t uns Uns.cmper_witness
                follow: Ordset.t uns Uns.cmper_witness
              }

            include IdentifiableIntf.S with type t := t
          }

        symbols: array Symbol.t
          [@@doc "Array of symbols, where each element's `index` field corresponds to the element's
          array index."]

        Lr0Item = {
            type t: t = {
                prod: Prod.t
                dot: uns
              }

            include IdentifiableIntf.S with type t := t
          }

        Lr1Item = {
            type t: t = {
                lr0item: Lr0Item.t
                follow: Ordset.t uns Uns.cmper_witness
              }

            include IdentifiableIntf.S with type t := t
          }

        Lr1Itemset = {
            type t: t = Ordmap.t Lr0Item.t Lr1Item.t Lr0Item.cmper_witness

            include IdentifiableIntf.S with type t := t
          }

        Lr1ItemsetClosure = {
            type t: t = {
                index: uns # Index of corresponding `State.t` in `states` array.
                kernel: Lr1Itemset.t
                added: Lr1Itemset.t
              }

            include IdentifiableIntf.S with type t := t
          }

        Action = {
            type t: t =
              | ShiftPrefix of uns # `states` index.
              | ShiftAccept of uns # `states` index.
              | Reduce of uns # `prods` index.

            include IdentifiableIntf.S with type t := t
          }

        State = {
            type t: t = {
                lr1ItemsetClosure: Lr1ItemsetClosure.t
                actions: Map.t uns Action.t Uns.cmper_witness
                gotos: Map.t uns uns Uns.cmper_witness
              }

            include IdentifiableIntf.S with type t := t
          }

        states: array State.t
          [@@doc "Array of CFSM states, where each element's `lr1ItemsetClosure.index` field
          corresponds to the element's array index."]
      }

    Token = {
        type t: t =
          # Built-in tokens with reserved names.
          | EPSILON # ε
          | PSEUDO_END # ⊥
          # One variant per `token` statement, e.g. `A` and `B`.
          | A of TypeA.t
          | B of TypeB.t

        include IdentifiableIntf.S with type t := t

        spec: t -> Spec.Symbol.t
      }

    Nonterm = {
        type t: t =
          # One variant per `nonterm`/`start` statement, e.g. `S` and `N`.
          | N of TypeN.t
          | S of TypeS.t
          # One variant per start symbol wrapper.
          | S' of TypeS.t

        include IdentifiableIntf.S with type t := t

        spec: t -> Spec.Symbol.t
      }

    Symbol = {
        type t: t =
          | Token of Token.t
          | Nonterm of Nonterm.t

        include IdentifiableIntf.S with type t := t

        spec: t -> Spec.Symbol.t
      }

    State = {
        type t: t = uns

        include IdentifiableIntf.S with type t := t

        spec: t -> Spec.State.t
      }

    Stack = {
        module Elm : sig
            type t: t = {
                symbol: Symbol.t;
                state: State.t;
              }

            include IdentifiableIntf.S with type t := t
          end

        type t: t = Elm.t list

        pp >e: t -> Fmt.Formatter e >e-> Fmt.Formatter e
        fmt >e: ?alt:bool -> ?width:uns -> t -> Fmt.Formatter e >e-> Fmt.Formatter e

        Reduction = {
            type stack: stack = t
            type t: t
            type callback: callback = stack -> Symbol.t * stack

            include IdentifiableIntf.S with type t := t

            callbacks: array callback
              [@@doc "Array of reduction callback functions containing embedded parser code."]

            callback: t -> callback
          }

        shift: symbol:Symbol.t -> state:State.t -> t -> t
          [@@doc "Perform a shift."]

        reduce: reduction:Reduction.t -> t -> t
          [@@doc "Perform a reduction."]
      }

    Status = {
        type t: t =
          # `feed`/`step` may produce these variants; `next` fast-forwards over them.
          | ShiftPrefix of Token.t * State.t
          | ShiftAccept of Token.t * State.t
          | Reduce of Token.t * Stack.Reduction.t
          # Common variants.
          | Prefix # Valid parse prefix; more input needed.
          | Accept of Nonterm.t # Successful parse result.
          | Reject of Token.t # Syntax error due to unexpected token.

        include IdentifiableIntf.S with type t := t
      }

    type t: t = {
        stack: Stack.t
        status: Status.t
      }

    Start = {
        # One submodule per `start` symbol, e.g. `S`.
        S = {
            boi: t
          }
      }

    feed: Token.t -> t -> t
      [@@doc "`feed token t` returns a result with status in {`ShiftPrefix`, `ShiftAccept`,
      `Reduce`, `Reject`}. `t.status` must be `Prefix`."]

    step: t -> t
      [@@doc "`step t` returns the result of applying one state transition to `t`. `t.status` must
      be in {`ShiftPrefix`, `ShiftAccept`, `Reduce`}."]

    next: -> Token.t -> t -> t
      [@@doc "`next token t` calls `feed token t` and fast-forwards via `step` calls to return a
      result with status in {`Prefix`, `Accept`, `Reject`}. `t.status` must be `Prefix`."]
  }
```

## Automaton description format

Per the [Command usage](#command-usage) documentation, `hocc` can emit a detailed automaton
description. Following is a brief explanation of the automaton description format, using abridged
output generated by `hocc -txt -src Example`.

```
Example grammar

Precedences
    left mul
    left add < mul
Tokens
    token EPSILON "ε"
        First: {"ε"}
        Follow: {}
[...]
    token EOI
        First: {EOI}
        Follow: {"⊥"}
Non-terminals
    nonterm MulOp of Token.t
        First: {"*", "/"}
        Follow: {INT}
        Productions
            MulOp ::= "*"
            MulOp ::= "/"
[...]
    start Answer'
        First: {INT}
        Follow: {"ε"}
        Productions
            Answer' ::= Answer "⊥"
IELR(1) States
    State 0 [0.0]
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
[...]
    State 13 [13.0]
        Kernel
            [Expr ::= Expr · MulOp Expr, {"*", "/", "+", "-", EOI}] prec mul
            [Expr ::= Expr · AddOp Expr, {"*", "/", "+", "-", EOI}] prec add
            [Expr ::= Expr AddOp Expr ·, {"*", "/", "+", "-", EOI}] prec add
        Added
            [MulOp ::= · "*", {INT}]
            [MulOp ::= · "/", {INT}]
            [AddOp ::= · "+", {INT}]
            [AddOp ::= · "-", {INT}]
        Actions
            "*" : ShiftPrefix 4 prec mul
            "/" : ShiftPrefix 5 prec mul
            "+" : Reduce Expr ::= Expr AddOp Expr prec add
            "-" : Reduce Expr ::= Expr AddOp Expr prec add
            EOI : Reduce Expr ::= Expr AddOp Expr prec add
        Gotos
            MulOp : 9
            AddOp : 10
        Conflict contributions
            [Expr ::= Expr · MulOp Expr, {"*"}]
                12 : Reduce Expr ::= Expr MulOp Expr
            [Expr ::= Expr · MulOp Expr, {"/"}]
                12 : Reduce Expr ::= Expr MulOp Expr
            [Expr ::= Expr · MulOp Expr, {"+"}]
                12 : Reduce Expr ::= Expr MulOp Expr
                13 : Reduce Expr ::= Expr AddOp Expr
            [Expr ::= Expr · MulOp Expr, {"-"}]
                12 : Reduce Expr ::= Expr MulOp Expr
                13 : Reduce Expr ::= Expr AddOp Expr
            [Expr ::= Expr · AddOp Expr, {"*"}]
                12 : Reduce Expr ::= Expr MulOp Expr
            [Expr ::= Expr · AddOp Expr, {"/"}]
                12 : Reduce Expr ::= Expr MulOp Expr
            [Expr ::= Expr · AddOp Expr, {"+"}]
                12 : Reduce Expr ::= Expr MulOp Expr
                13 : Reduce Expr ::= Expr AddOp Expr
            [Expr ::= Expr · AddOp Expr, {"-"}]
                12 : Reduce Expr ::= Expr MulOp Expr
                13 : Reduce Expr ::= Expr AddOp Expr
            [Expr ::= Expr AddOp Expr ·, {"+"}]
                13 : Reduce Expr ::= Expr AddOp Expr
            [Expr ::= Expr AddOp Expr ·, {"-"}]
                13 : Reduce Expr ::= Expr AddOp Expr
```

Of note:

- The first line specifies the grammar name &mdash; `Example`.
- Precedences and their relationships are enumerated in the `Precedences` section.
- Tokens and their first/follow sets are enumerated in the `Tokens` section
- Non-terminals, their first/follow sets, and their productions are enumerated in the
  `Non-terminals` section.
- The algorithm used to generate the state machine is specified in the `States` section header,
  `IELR(1)` in this case.
- The n states are indexed [0..n-1], [0..14-1] in this case.
- Isocore indexing is also reported for all algorithms that can generate non-singleton isocore
  sets, i.e. all algorithms besides LALR(1). For a more complex grammar, the IELR(1) algorithm might
  generate indexes like the following:
    ```
    State 235 [234.0]
    State 297 [234.1]
    ```
  These states are isocoric because they both have isocore index 234. Furthermore, IELR(1) generates
  LALR(1) states as preliminary metadata, and `hocc` assures that IELR(1) isocore indexes match
  LALR(1) state indexes.
- Each state comprises the following subsections (empty subsections omitted in output):
  + `Kernel`: Kernel LR(1) items
  + `Added`: Added LR(1) items
  + `Actions`: Map of per token actions, with conflicts prefixed by `CONFLICT`
  + `Gotos`: Map of per non-terminal gotos
  + `Conflict contributions`: Per {kernel item, follow symbol, conflict state} IELR(1) conflict
    contributions that inform isocore (in)compatibility (NB: conflict state is an LALR(1) state
    index)
- Conflict contributions are best interpreted in combination with a corresponding LALR(1) automaton
  report generated with conflict resolution disabled (e.g. `hocc -txt -algorithm lalr1 -resolve no
  -src Example`). This enables inspection of the conflicts which compel IELR(1) state splitting.

## Grammar

The `hocc` specification language grammar is equivalent to the following specification.

```hocc
hocc
    right pCIDENT
    left pDOT
    left pCOMMA < pCIDENT
    right pSEMI
    neutral pAS < pCOMMA
    token HOCC "hocc"
    token NONTERM "nonterm"
    token EPSILON_ "epsilon"
    token START "start"
    token TOKEN "token"
    token NEUTRAL "neutral"
    token LEFT "left"
    token RIGHT "right"
    token NONASSOC "nonassoc"
    token PREC "prec"
    token UIDENT
    token CIDENT prec pCIDENT
    token USCORE "_"
    token ISTRING
    token COLON_COLON_EQ "::="
    token OF "of"
    token COLON ":"
    token DOT "." prec pDOT
    token ARROW "->"
    token BAR "|"
    token LT "<"
    token EQ "="
    token COMMA "," prec pCOMMA
    token SEMI ";" prec pSEMI
    token AS "as" prec pAS
    token LINE_DELIM
    token INDENT
    token DEDENT
    token LPAREN "("
    token RPAREN ")"
    token LCAPTURE "(|"
    token RCAPTURE "|)"
    token LBRACK "["
    token RBRACK "]"
    token LARRAY "[|"
    token RARRAY "|]"
    token LCURLY "{"
    token RCURLY "}"
    token OTHER_TOKEN
    token EOI
    nonterm Uident ::=
      | "hocc"
      | "nonterm"
      | "epsilon"
      | "start"
      | "token"
      | "neutral"
      | "left"
      | "right"
      | "nonassoc"
      | "prec"
      | UIDENT
    nonterm PrecsTl ::=
      | "," Uident PrecsTl
      | epsilon
    nonterm Precs ::= Uident PrecsTl
    nonterm PrecRels ::=
      | "<" Precs
      | epsilon
    nonterm PrecType ::=
      | "neutral"
      | "left"
      | "right"
      | "nonassoc"
    nonterm PrecSet ::= PrecType Precs PrecRels
    nonterm SymbolTypeQualifier ::=
      | CIDENT "." SymbolTypeQualifier
      | epsilon
    nonterm SymbolType ::= "of" SymbolTypeQualifier Uident
    nonterm SymbolType0 ::=
      | SymbolType
      | epsilon
    nonterm PrecRef ::=
      | "prec" Uident
      | epsilon
    nonterm TokenAlias ::=
      | ISTRING
      | epsilon
    nonterm Token ::= "token" CIDENT TokenAlias SymbolType0 PrecRef
    nonterm Sep ::=
      | LINE_DELIM
      | ";"
      | "|"
    nonterm CodesTl ::=
      | Sep Code CodesTl
      | epsilon
    nonterm Codes ::= Code CodesTl
    nonterm Codes0 ::=
      | Codes
      | epsilon
    nonterm Delimited ::=
      | INDENT Codes DEDENT
      | "(" Codes0 ")"
      | "(|" Codes0 "|)"
      | "[" Codes0 "]"
      | "[|" Codes0 "|]"
      | "{" Codes0 "}"
    nonterm CodeToken ::=
      | OTHER_TOKEN
      | Uident
      | CIDENT
      | "_"
      | ISTRING
      | "::="
      | "as"
      | "of"
      | ":"
      | "."
      | "->"
      | "<"
      | "="
      | ","
    nonterm CodeTl ::=
      | Delimited CodeTl
      | CodeToken CodeTl
      | epsilon
    nonterm Code ::=
      | Delimited CodeTl
      | CodeToken CodeTl
    nonterm PatternField ::=
      | Uident
      | Uident "=" Pattern
    nonterm PatternFields prec pSEMI ::=
      | PatternField
      | PatternField ";" "_"
      | PatternField ";" PatternFields
    nonterm SemiSuffix ::=
      | ";"
      | epsilon
    nonterm ModulePath ::=
      | CIDENT
      | ModulePath "." ModulePath prec pDOT
    nonterm Pattern ::=
      | "_"
      | Uident
      | Pattern "as" Uident prec pAS
      | "(" Pattern ")"
      | CIDENT Pattern prec pCIDENT
      | ModulePath "." "(" Pattern ")"
      | Pattern "," Pattern prec pCOMMA
      | "{" PatternFields SemiSuffix "}"
      | ModulePath "." "{" PatternFields SemiSuffix "}"
    nonterm ProdParamSymbol ::=
      | CIDENT
      | ISTRING
    nonterm ProdParam ::=
      | Uident ":" ProdParamSymbol
      | "(" Pattern ")" ":" ProdParamSymbol
      | ModulePath "." "(" Pattern ")" ":" ProdParamSymbol
      | "{" PatternFields SemiSuffix "}" ":" ProdParamSymbol
      | ModulePath "." "{" PatternFields SemiSuffix "}" ":" ProdParamSymbol
      | "_" ":" ProdParamSymbol
      | ProdParamSymbol
    nonterm ProdParamsTl ::=
      | ProdParam ProdParamsTl
      | PrecRef
    nonterm ProdParams ::= ProdParam ProdParamsTl
    nonterm ProdPattern ::=
      | ProdParams
      | "epsilon" PrecRef
    nonterm Prod ::= ProdPattern
    nonterm ProdsTl ::=
      | "|" Prod ProdsTl
      | epsilon
    nonterm Prods ::=
      | "|" Prod ProdsTl
      | Prod ProdsTl
    nonterm Reduction ::= Prods "->" Code
    nonterm ReductionsTl ::=
      | "|" Reduction ReductionsTl
      | epsilon
    nonterm Reductions ::= Reduction ReductionsTl
    nonterm NontermType ::=
      | "nonterm"
      | "start"
    nonterm Nonterm ::=
      | NontermType CIDENT PrecRef "::=" Prods
      | NontermType CIDENT SymbolType PrecRef "::=" Reductions
    nonterm Stmt ::=
      | PrecSet
      | Token
      | Nonterm
    nonterm StmtsTl ::=
      | LINE_DELIM Stmt StmtsTl
      | epsilon
    nonterm Stmts ::= Stmt StmtsTl
    nonterm Hocc ::= "hocc" INDENT Stmts DEDENT
    nonterm MatterToken ::=
      | Sep
      | "nonterm"
      | "epsilon"
      | "start"
      | "token"
      | "neutral"
      | "left"
      | "right"
      | "nonassoc"
      | "prec"
      | OTHER_TOKEN
      | UIDENT
      | CIDENT
      | "_"
      | ISTRING
      | "::="
      | "as"
      | "of"
      | ":"
      | "."
      | "->"
      | "<"
      | "="
      | ","
      | INDENT
      | DEDENT
      | "("
      | ")"
      | "(|"
      | "|)"
      | "["
      | "]"
      | "[|"
      | "|]"
      | "{"
      | "}"
    nonterm Matter ::=
      | MatterToken Matter
      | epsilon
    start Hmh ::= Matter Hocc Matter EOI
    start Hmhi ::= Matter "hocc" Matter EOI
```

## Citations

[^knuth1965]:
    Donald Knuth,
    “On the Translation of Languages from Left to Right”,
    Information and Control 8(6):607–639, July 1965.

[^pager1977]:
    David Pager,
    “A Practical General Method for Constructing LR(k) Parsers”,
    Acta Informatica 7:249-268, 1977.

[^diekmann2020]:
    Lukas Diekmann and Laurence Tratt,
    “Don't Panic! Better, Fewer, Syntax Errors for LR Parsers,”
    34th European Conference on Object-Oriented Programming (ECOOP 2020), Article No. 6, pages 6:1–6:32.

[^fpottier]:
    François Pottier and Yann Régis-Gianas,
    “Menhir LR(1) Parser Generator,”
    http://gallium.inria.fr/~fpottier/menhir/

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
