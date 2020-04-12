# Hemlock

Hemlock is a programming language in the [ML
family](https://en.wikipedia.org/wiki/ML_(programming_language)) that
emphasizes reliable high performance parallel computation.  Hemlock synthesizes
the expressive type and module systems of [OCaml](http://ocaml.org/), a
unified parametric effects/mutability/type system, and the shared-nothing
message-passing parallelism of [Erlang](https://erlang.org/),

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
dune build @doc
```

Use a web browser to open the documentation at
`_build/default/_doc/_html/index.html`.

### License

This project is licensed under the MIT license; see the
[LICENSE.md](LICENSE.md) file for details.
