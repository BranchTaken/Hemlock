# Hemlock

Hemlock is a systems programming language that emphasizes reliable high performance parallel
computation. Hemlock synthesizes the expressive type and module systems of [ML-family
languages](https://en.wikipedia.org/wiki/ML_(programming_language)), a unified parametric
effects/mutability/type system, and the shared-nothing message-passing parallelism of
[Erlang](https://erlang.org/).

## Getting Started

Hemlock is a work in progress, and is not yet self hosting (i.e. it is written in OCaml rather than
Hemlock). As such, Hemlock may interest researchers and programming language developers, but it is
not yet suited for any sort of production use.

### Prerequisites

There are two separate methods for building the bootstrap compiler (Docker and Native). Follow
prerequisites for your preferred method.

#### Docker Prerequisites

Install [docker](https://docs.docker.com/engine/install/).

Install [docker compose](https://docs.docker.com/compose/cli-command/).

Build the Hemlock `prod` docker image from the root of the Hemlock repo checkout.

```sh
docker compose build prod
```

Run a Hemlock `prod` docker container.

```sh
docker compose run prod
```

(Optional) Install
[binfmt_misc](https://docs.docker.com/buildx/working-with-buildx/#build-multi-platform-images). This
enables building and running cross-platform images.
```sh
docker run --privileged --rm tonistiigi/binfmt --install all
HEMLOCK_PLATFORM=linux/arm64 docker compose build prod
HEMLOCK_PLATFORM=linux/arm64 docker compose run prod
```

#### Native Prerequisites

The bootstrap compiler depends on [OCaml](http://ocaml.org/) and the [Dune](https://dune.build/)
build system, as well as several ppx rewriters.  [opam](https://opam.ocaml.org/) is the recommended
mechanism for installing and maintaining an OCaml development environment. To install the necessary
dependencies into an existing opam repository, run

```sh
cd bootstrap
opam install --deps-only .
```

### Building

To build the bootstrap `hmc` Hemlock compiler and invoke the test suite, run

```sh
cd bootstrap
dune build
dune runtest
```

See the [Dune documentation](https://dune.readthedocs.io/en/latest/) for more details on building
and interacting with `hmc` via the build system.

To build the bootstrap standard library documentation, run

```sh
dune build @doc
```

Use a web browser to open the documentation at `_build/default/_doc/_html/index.html`.

### Pull Requests

Pull requests are required to pass tests before being merged to `main`. Pull request CI checks that
tests passed as part of Hemlock's custom `gh push` process. Use our custom
[gh](https://github.com/cli/cli) CLI [extension](https://github.com/BranchTaken/gh-push) to push,
test, and record the test result status of branches.

### License

This project is licensed under the MIT license; see the [LICENSE.md](LICENSE.md) file for details.
