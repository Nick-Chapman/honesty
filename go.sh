#!/usr/bin/env bash
set -euo pipefail

stack run -- --dis > dis.log
cmp dis.log{.expected,}
#diff dis.log{.expected,} | colordiff

#(stack run | tee em.log) || diff -c <(cat data/nestest.log-cut | sed 's/ CYC.*//' | sed 's/ = ../     /') em.log | colordiff | head


(stack run | tee em.log | tail) || diff -c <(cat data/nestest.log-cut | sed 's/ = ../     /') em.log | colordiff | head
