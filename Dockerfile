ARG HEMLOCK_PLATFORM=$BUILDPLATFORM
ARG HEMLOCK_UBUNTU_TAG=rolling
FROM --platform=${HEMLOCK_PLATFORM} ubuntu:${HEMLOCK_UBUNTU_TAG} AS base
RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -y \
        ca-certificates \
        git \
        liburing-dev \
        m4 \
        opam \
        sudo \
    && rm -rf /var/lib/apt/lists/* \
    && mv /home/ubuntu /home/hemlock \
    && groupmod -n hemlock ubuntu \
    && usermod -d /home/hemlock -c Hemlock -l hemlock ubuntu \
    && echo "hemlock ALL=(ALL:ALL) NOPASSWD: ALL" >> /etc/sudoers
ARG HEMLOCK_BOOTSTRAP_OCAML_VERSION=4.14.0
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

FROM --platform=${HEMLOCK_PLATFORM} latest AS test
USER hemlock
WORKDIR /home/hemlock/Hemlock/bootstrap
CMD find . -type f -regex '.*\mli?' | xargs -- opam exec -- ocp-indent -i \
    && git diff --exit-code \
    && opam exec -- dune build src \
    && opam exec -- dune runtest \
    && rm -rf _build

