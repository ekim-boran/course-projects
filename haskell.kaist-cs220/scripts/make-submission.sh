#!/usr/bin/env bash

set -e

BASEDIR=$(dirname "$0")/..
cd $BASEDIR

mkdir -p target

rm -rf target/a?*.zip

zip target/a03.zip src/A02.hs src/A03.hs
zip target/a04.zip src/A02.hs src/A03.hs src/A04.hs
zip target/a06.zip src/A02.hs src/A03.hs src/A05.hs src/A06.hs
