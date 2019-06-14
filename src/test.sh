#!/usr/bin/env bash
stack install --resolver=lts-8.12 clash-ghc-0.7.1 HTF
stack exec --resolver=lts-8.12 -- clash --make test/Main -i. && test/Main

