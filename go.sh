#!/usr/bin/env bash
set -uo pipefail

clear

stack run -- --dis > dis.log
cmp dis.log{.expected,}
#diff dis.log{.expected,} | colordiff

stack run > em.log

status=$?

rm diff.log
diff -u <(cat data/nestest.log-cut | sed 's/ = ../     /') em.log > diff.log

if [ ! -f diff.log ]; then echo '**no diffs**'; exit $status; fi

cat diff.log | sed '1,/^-/!d'
cat diff.log | grep -v ^+++ | grep ^[+] | head -1

exit 1
