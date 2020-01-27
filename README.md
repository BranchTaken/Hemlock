# Hemlock

Hemlock is a programming language in the [ML
family](https://en.wikipedia.org/wiki/ML_(programming_language)) that
emphasizes reliable high performance parallel computation.  Hemlock synthesizes
the expressive type and module systems of [OCaml](http://ocaml.org/), the
precise mutability control of [Skip](http://skiplang.com/), and the
shared-nothing message-passing parallelism of [Erlang](https://erlang.org/).
Hemlock's features combine to enhance human reasoning in ways that also enable
reliable high performance parallel computation via both static compiler
analysis and dynamic profile-driven just-in-time (JIT) optimization.

## Getting Started

Hemlock is a work in progress, and is not yet self hosting (i.e. it is written
in OCaml rather than Hemlock).  As such, Hemlock may interest researchers and
programming language developers, but it is not yet suited for any sort of
production use.

### Prerequisites

The bootstrap compiler depends on [OCaml](http://ocaml.org/) and the
[Dune](https://dune.build/) build system, as well as several ppx rewriters.
[opam](https://opam.ocaml.org/) is the recommended mechanism for installing and
maintaining an OCaml development environment.  To install the necessary
dependencies into an existing opam repository, run

```sh
cd bootstrap
opam install --deps-only .
```

### Building

To build the bootstrap `hlc` Hemlock compiler and invoke the test suite, run

```sh
cd bootstrap
dune build
dune runtest
```

See the [Dune documentation](https://dune.readthedocs.io/en/latest/) for more
details on building and interacting with `hlc` via the build system.

To build the bootstrap standard library documentation, run

```sh
dune build @doc-private
```

Use a web browser to open the documentation in a location that looks similar to
`_build/default/_doc/_html/Hemlock@<hex>/Hemlock/index.html`, where `<hex>` is
a generated hex string.

### License

This project is licensed under the MIT license; see the
[LICENSE.md](LICENSE.md) file for details.
