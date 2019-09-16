#!/usr/bin/env bash
set -euo pipefail

clear

RTS='+RTS -t -RTS'

echo tests started

stack run -- $RTS --dis > dis.log
cmp dis.log{.expected,}
#diff dis.log{.expected,} | colordiff
echo - dis-nestest passed

stack run -- $RTS --nestest > em.log
cmp em.log{.expected,}
#diff em.log{.expected,} | colordiff
echo - emu-nestest passed

echo tests done

exit

status=$?

rm diff.log
diff -u <(cat data/nestest.log-cut |
              sed 's/@ .. = .... = ../                /' |
              sed 's/@ .... = ../           /' |
              sed 's/= .... @ .... = ../                  /' |
              sed 's/= .... /       /' |
              sed 's/@ .. /     /' |
              sed 's/= .. /     /'
         ) em.log > diff.log

if [ ! -f diff.log ]; then echo '**no diffs**'; exit $status; fi

cat diff.log | sed '1,/^-/!d'
cat diff.log | grep -v ^+++ | grep ^[+] | head -1

exit 1
