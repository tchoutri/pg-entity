#!/usr/bin/env bash

set -eao pipefail

source ./environment.sh
[ -f ./environment.local.sh ] && source ./environment.local.sh

make db-drop
make db-setup

if [ -z "$1" ] ;
then
  cabal test
else
  ghcid --command='cabal v2-repl entity-test' --test 'Main.main'
fi

