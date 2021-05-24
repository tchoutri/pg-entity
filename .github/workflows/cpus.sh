#!/bin/sh
# Originally from:
# https://github.com/blankpage/e5UNIXBuilder/blob/master/build-akili.sh

# Linux and similar...
CPUS=$(getconf _NPROCESSORS_ONLN 2>/dev/null)
# FreeBSD, macOS and similar...
[ -z "$CPUS" ] && CPUS=$(getconf NPROCESSORS_ONLN)
# Give up...
[ -z "$CPUS" ] && CPUS=1

export CPUS
