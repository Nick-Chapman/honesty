#!/usr/bin/env bash
set -euo pipefail

clear

#RTS='+RTS -t -RTS'
RTS=

echo tests started

stack run -- $RTS --dis data/nestest.nes > data/dis.log
diff data/dis.log{.expected,} | head
echo - dis-nestest passed

stack run -- $RTS --emu data/nestest.nes > data/em.log
cmp data/em.log{.expected,}
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
