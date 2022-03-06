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
CMD [ "/bin/bash" ]

FROM --platform=${HEMLOCK_PLATFORM} base AS prod
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
    && opam install -y \
        ocaml-lsp-server \
        ocp-indent \
        utop \
    && opam install -y --deps-only . \
    && rm Hemlock.opam

FROM --platform=${HEMLOCK_PLATFORM} prod AS pre-push
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

FROM --platform=${HEMLOCK_PLATFORM} base AS dev
USER root
RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -y \
        meson \
        openssh-client \
    && rm -rf /var/lib/apt/lists/*
ARG DOTFILES_URL
ARG DOTFILES_HASH
USER hemlock
WORKDIR /home/hemlock
RUN --mount=type=ssh,uid=1000,gid=1000 \
    --mount=type=cache,target=/var/cache/apt \
    --mount=type=cache,target=/var/lib/apt \
    ([ -z ${DOTFILES_URL} ] || \
        mkdir -p -m 0700 ~/.ssh \
        && ssh-keyscan -H github.com \
            >> ~/.ssh/known_hosts \
        && git clone -v ${DOTFILES_URL} .dotfiles) \
    && ([ ! -f .dotfiles/install.sh ] || .dotfiles/install.sh)
COPY --chown=hemlock:hemlock --from=prod /home/hemlock/.opam /home/hemlock/.opam
