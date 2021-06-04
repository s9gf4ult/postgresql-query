#!/bin/bash
set -e

# stack --stack-yaml ghc-8.4.4.yaml test --fast --ghc-options='-ferror-spans -instances -j +RTS -A128m -n2m -qb0 -RTS'
stack --stack-yaml ghc-8.6.5.yaml test --no-nix-pure --fast --ghc-options='-ferror-spans -instances -j +RTS -A128m -n2m -qb0 -RTS'
stack --stack-yaml ghc-8.8.3.yaml test --no-nix-pure --fast --ghc-options='-ferror-spans -instances -j +RTS -A128m -n2m -qb0 -RTS'
