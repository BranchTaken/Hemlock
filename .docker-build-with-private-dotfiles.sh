#!/bin/bash

set -e

BD=$(dirname $(realpath $0))

if [ -z $DOTFILES ] ; then
    DOTFILES=$HOME/.dotfiles
fi

if [ -d $DOTFILES ] ; then
    pushd $DOTFILES > /dev/null
    if [ -z $DOTFILES_URL ]; then
        DOTFILES_URL=$(git remote get-url origin)
    fi
    if [ -z $DOTFILES_HASH ] ; then
        DOTFILES_HASH=$(get rev-parse origin/master)
    fi
    popd > /dev/null
fi

. $BD/.env

docker buildx build \
    --build-arg DOTFILES_HASH=$DOTFILES_HASH \
    --build-arg DOTFILES_URL=$DOTFILES_URL \
    --build-arg HEMLOCK_BOOTSTRAP_OCAML_VERSION=$HEMLOCK_BOOTSTRAP_OCAML_VERSION \
    --file $BD/.dockerfile \
    --ssh default \
    --tag branchtaken__hemlock__dev \
    --target dev \
    $BD
