#!/bin/bash

cp lts-2.9.yaml stack.yaml &&
stack clean && stack test &&
cp lts-6.7.yaml stack.yaml &&
stack clean && stack test &&
cp ghc-8.0.1.yaml stack.yaml &&
stack clean && stack test
