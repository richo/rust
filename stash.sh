#!/bin/sh
set -xe

rust_version=$(head -n 1 RELEASES.txt | cut -f 2 -d " ")

rev=`git rev-parse --short @`
prefix="~/.rusts/$rust_version-$rev"

./configure --prefix=$prefix
make -j4
make install

set +x
echo "You may want to update your dotfiles to use '$rust_version-$rev'"
