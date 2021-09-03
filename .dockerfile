FROM ubuntu AS base
RUN --mount=type=cache,target=/var/cache/apt \
    --mount=type=cache,target=/var/lib/apt \
    rm /etc/apt/apt.conf.d/docker-clean \
    && echo 'Binary::apt::APT::Keep-Downloaded-Packages "true";' \
        > /etc/apt/apt.conf.d/keep-cache \
    && apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -y \
        ca-certificates \
        m4 \
        opam \
        sudo \
    && rm -rf /var/lib/apt/lists/* \
    && useradd -l -m -U -G sudo -s /bin/bash hemlock \
    && echo "hemlock ALL=(ALL:ALL) NOPASSWD: ALL" >> /etc/sudoers
CMD [ "/bin/bash" ]

FROM base AS prod
ARG HEMLOCK_BOOTSTRAP_OCAML_VERSION
USER hemlock
WORKDIR /home/hemlock
COPY --chown=hemlock:hemlock bootstrap/Hemlock.opam .
RUN --mount=type=cache,target=/home/hemlock/.opam/download-cache,uid=1000,gid=1000 \
    --mount=type=cache,target=/home/hemlock/.opam/repo,uid=1000,gid=1000 \
    sudo chown hemlock:hemlock .opam \
    && opam init \
        --bare \
        --disable-sandboxing \
        --dot-profile /home/hemlock/.bashrc \
        --reinit \
        --shell-setup \
        --yes \
    && opam switch create ${HEMLOCK_BOOTSTRAP_OCAML_VERSION} \
    && opam install -y \
        ocaml-lsp-server \
        utop \
    && opam install -y --deps-only . \
    && rm Hemlock.opam

FROM base AS dev
USER root
RUN --mount=type=cache,target=/var/cache/apt \
    --mount=type=cache,target=/var/lib/apt \
    apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -y \
        git \
        meson \
        python3 \
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
