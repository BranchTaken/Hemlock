ARG HEMLOCK_PLATFORM=$BUILDPLATFORM
ARG HEMLOCK_UBUNTU_TAG
FROM --platform=${HEMLOCK_PLATFORM} ubuntu:${HEMLOCK_UBUNTU_TAG} AS base
RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -y \
        ca-certificates \
        git \
        liburing-dev \
        m4 \
        opam \
        python3 \
        sudo \
    && rm -rf /var/lib/apt/lists/* \
    && useradd -l -m -U -G sudo -s /bin/bash hemlock \
    && echo "hemlock ALL=(ALL:ALL) NOPASSWD: ALL" >> /etc/sudoers
ARG HEMLOCK_BOOTSTRAP_OCAML_VERSION
USER hemlock
WORKDIR /home/hemlock
COPY --chown=hemlock:hemlock bootstrap/Hemlock.opam .
RUN opam init \
        --bare \
        --disable-sandboxing \
        --dot-profile /home/hemlock/.bashrc \
        --reinit \
        --shell-setup \
        --yes \
    && opam switch create ${HEMLOCK_BOOTSTRAP_OCAML_VERSION} \
    && opam install -y ocp-indent \
    && opam install -y --deps-only . \
    && rm Hemlock.opam

FROM --platform=${HEMLOCK_PLATFORM} base AS latest
USER hemlock
WORKDIR /home/hemlock/Hemlock
COPY --chown=hemlock:hemlock /.git/ .git
RUN git reset --hard

FROM --platform=${HEMLOCK_PLATFORM} latest AS tested
USER hemlock
WORKDIR /home/hemlock/Hemlock/bootstrap
RUN find . -type f -regex '.*\mli?' | xargs -- opam exec -- ocp-indent -i \
    && git diff --exit-code \
    && opam exec -- dune build \
    && opam exec -- dune runtest \
    && rm -rf _build

FROM --platform=${HEMLOCK_PLATFORM} base AS pre-push
ARG HEMLOCK_PRE_PUSH_CLONE_PATH
ARG HEMLOCK_CHECK_OCP_INDENT_BASE_COMMIT
USER hemlock
WORKDIR /home/hemlock/origin
COPY --chown=hemlock:hemlock ${HEMLOCK_PRE_PUSH_CLONE_PATH:?arg-is-required} .
WORKDIR /home/hemlock/Hemlock
RUN git clone ~/origin . \
    && (cd bootstrap; opam exec -- dune build) \
    && (cd bootstrap; opam exec -- dune runtest) \
    && HEMLOCK_CHECK_OCP_INDENT_BASE_COMMIT=${HEMLOCK_CHECK_OCP_INDENT_BASE_COMMIT:?arg-is-required} \
        python3 .github/scripts/check_ocp_indent.py