#!/usr/bin/env bash

set -e

# format
find src/ app/ test/ -name '*.hs' ! -wholename 'test/*_Random_*.hs' -type f -print0 \
  | xargs -0 -I {} -n 1 brittany --write-mode=inplace

# lint
find src/ app/ test/ -name '*.hs' ! -wholename 'test/*_Random_*.hs' -type f -print0 \
  | xargs -0 -I {} -n 1 hlint --refactor --refactor-options="-i" {}
