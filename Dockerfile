FROM ubuntu AS bootstrap_base
ARG HEMLOCK_BOOTSTRAP_GID=1000
ARG HEMLOCK_BOOTSTRAP_OPAMSWITCH=4.14.0
ARG HEMLOCK_BOOTSTRAP_SHELL=/bin/bash
ARG HEMLOCK_BOOTSTRAP_UID=1000
RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y \
        ca-certificates \
        git \
        liburing-dev \
        m4 \
        opam \
        sudo \
    && apt-get clean \
    && userdel -r ubuntu \
    && groupadd -r -o -g ${HEMLOCK_BOOTSTRAP_GID} hemlock \
    && useradd -l -m -r -d /home/hemlock -G sudo \
        -g ${HEMLOCK_BOOTSTRAP_GID} \
        -s ${HEMLOCK_BOOTSTRAP_SHELL} \
        -u ${HEMLOCK_BOOTSTRAP_UID} \
        hemlock \
    && echo "hemlock ALL=(ALL:ALL) NOPASSWD: ALL" >> /etc/sudoers \
    && chown hemlock:hemlock /home/hemlock \
    && sudo -u hemlock mkdir -p /home/hemlock/Hemlock/bootstrap \
    && sudo -u hemlock opam init -a -y --disable-sandboxing --switch=${HEMLOCK_BOOTSTRAP_OPAMSWITCH}

FROM bootstrap_base AS bootstrap_src
USER hemlock
WORKDIR /home/hemlock/Hemlock
COPY --chown=hemlock:hemlock .git .git
RUN git reset --hard

FROM bootstrap_base AS bootstrap_lint
USER hemlock
RUN opam install -y ocp-indent
WORKDIR /home/hemlock/Hemlock
COPY --from=bootstrap_src /home/hemlock/Hemlock .
WORKDIR /home/hemlock/Hemlock/bootstrap
CMD find . -type f -regex '.*\mli?' \
        | grep -v -e '^\./test/hocc/' -e '^\./bin/hocc/Parse\.ml$' \
        | xargs -- opam exec -- ocp-indent -i \
    && git diff --exit-code

FROM bootstrap_base AS bootstrap_deps
USER hemlock
WORKDIR /home/hemlock/Hemlock/bootstrap
COPY --from=bootstrap_src /home/hemlock/Hemlock/bootstrap/Hemlock.opam .
RUN opam install -y --deps-only .

FROM bootstrap_base AS bootstrap_build
USER hemlock
WORKDIR /home/hemlock/Hemlock/bootstrap
COPY --from=bootstrap_deps /home/hemlock/.opam /home/hemlock/.opam
COPY --from=bootstrap_src /home/hemlock/Hemlock/bootstrap .
CMD opam exec -- dune build src --verbose

FROM bootstrap_base AS bootstrap_test
USER hemlock
WORKDIR /home/hemlock/Hemlock/bootstrap
COPY --from=bootstrap_deps /home/hemlock/.opam /home/hemlock/.opam
COPY --from=bootstrap_src /home/hemlock/Hemlock/bootstrap .
CMD  opam exec -- dune runtest test --verbose

FROM bootstrap_base AS bootstrap
USER hemlock
WORKDIR /home/hemlock/Hemlock/bootstrap
COPY --from=bootstrap_deps /home/hemlock/.opam /home/hemlock/.opam

