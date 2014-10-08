#!/bin/sh
set -xe

rust_version=$(head -n 1 RELEASES.txt | cut -f 2 -d " ")

rev=`git rev-parse --short @`
branch=$(git symbolic-ref HEAD | sed -e 's|refs/heads/||')

if [ "$branch" != "master" ]; then
    prefix="~/.rusts/$rust_version-$branch"
else
    prefix="~/.rusts/$rust_version-$rev"
fi

./configure --prefix=$prefix
make -j4
make install

set +x
echo "You may want to update your dotfiles to use '$(basename ${prefix})'"
