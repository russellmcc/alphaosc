#!/usr/bin/env bash
stack install --resolver=lts-8.12 clash-ghc-0.7.1
stack exec --resolver=lts-8.12 -- clash MainR2R.hs --verilog
