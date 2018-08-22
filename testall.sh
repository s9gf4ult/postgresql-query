#!/bin/bash
set -e

cp ghc-7.10.3.yaml stack.yaml &&
stack clean && stack test &&

cp ghc-8.4.3.yaml stack.yaml &&
stack clean && stack test
