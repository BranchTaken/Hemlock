FROM ubuntu AS dev
ARG HEMLOCK_EXTRA_APT_PACKAGES
ARG HEMLOCK_EXTRA_OPAM_PACKAGES
ARG HEMLOCK_GID
ARG HEMLOCK_UID
ARG HEMLOCK_OPAMSWITCH=4.14.2
RUN --mount=type=bind,source=/bootstrap/Hemlock.opam,target=/Hemlock.opam \
    userdel -r ubuntu \
    && groupadd -g "${HEMLOCK_GID}" hemlock \
    && useradd -d /home/hemlock -g "${HEMLOCK_GID}" -G sudo -m -u "${HEMLOCK_UID}" hemlock \
    && apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y liburing-dev opam sudo ${HEMLOCK_EXTRA_APT_PACKAGES} \
    && apt-get clean \
    && echo "hemlock ALL=(ALL:ALL) NOPASSWD: ALL" >> /etc/sudoers \
    && sudo -u hemlock opam init -a -y --disable-sandboxing --switch="${HEMLOCK_OPAMSWITCH}" \
    && sudo -u hemlock opam install -y ocp-indent ${HEMLOCK_EXTRA_OPAM_PACKAGES} \
    && sudo -u hemlock opam install -y --deps-only . \
    && sudo -u hemlock mkdir -p /home/hemlock/Hemlock
USER hemlock
WORKDIR /home/hemlock/Hemlock

