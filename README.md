# SawML

SawML (pronounced as "sawmill") is a programming language in the [ML
family](https://en.wikipedia.org/wiki/ML_(programming_language)) that emphasizes
reliable high performance parallel computation.  SawML synthesizes the
expressive type and module systems of [OCaml](http://ocaml.org/), the precise
mutability control of [Skip](http://skiplang.com/), and the shared-nothing
message-passing parallelism of [Erlang](https://erlang.org/).  SawML's features
combine to enhance human reasoning in ways that also enable reliable high
performance parallel computation via both static compiler analysis and dynamic
profile-driven just-in-time (JIT) optimization.

## Getting Started

SawML is a work in progress, and is not yet self hosting (i.e. it is written in
OCaml rather than SawML).  As such, SawML may interest researchers and
programming language developers, but it is not yet suited for any sort of
production use.

### Prerequisites

The bootstrap compiler depends on [OCaml](http://ocaml.org/) and the
[Dune](https://dune.build/) build system.  [opam](https://opam.ocaml.org/) is
the recommended mechanism for installing and maintaining an OCaml development
environment.

### Building

To build the bootstrap `sawml` and invoke the test suite, run

```sh
dune runtests
```

See the [Dune documentation](https://dune.readthedocs.io/en/latest/) for more
details on building and interacting with `sawml` via the build system.

### License

This project is licensed under the MIT license; see the
[LICENSE.md](LICENSE.md) file for details.
