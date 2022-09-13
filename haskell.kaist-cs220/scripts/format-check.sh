#!/usr/bin/env bash

set -e

# format
brittany --write-mode=inplace src/*.hs app/*.hs test/*.hs -c

# lint
hlint .
