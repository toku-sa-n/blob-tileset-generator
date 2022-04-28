#!/bin/bash

set -e

usage () {
    cat << EOS
Usage: ./formatter.sh [-c]

Format the all haskell sources in this repository.

You need to install these commands:
- sponge
- hindent
- stylish-haskell


Options:
    -c --check  Do not format the source code, and instead check if the source files are formatted. This script will exit with non-zero exit code if one of the source files are not formatted correctly.
    -h --help   Show this help
EOS
}

readonly ARGV=("$@")

case "${ARGV[0]}" in
    "" )
        find src tests app -name '*.hs'|xargs -I {} sh -c "cat {}|hindent|stylish-haskell|sponge {}"
        ;;
    "-c"|"--check" )
        find src tests app -name '*.hs'|xargs -I {} sh -c "cat {}|hindent|stylish-haskell|diff --color -u {} -"
        ;;
    "-h"|"--help" )
        usage
        ;;
    * )
        usage >&2
        exit 1
        ;;
esac
