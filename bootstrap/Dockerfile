FROM ubuntu AS latest
ARG HEMLOCK_BOOTSTRAP_OPAMSWITCH=4.14.2
ENV OPAMROOTISOK=1
RUN --mount=type=bind,source=/Hemlock.opam,target=/Hemlock.opam apt-get update \
    && apt-get install -y liburing-dev opam \
    && apt-get clean \
    && opam init -a -y --disable-sandboxing --switch="${HEMLOCK_BOOTSTRAP_OPAMSWITCH}" \
    && opam install -y ocp-indent \
    && opam install -y --deps-only .

