#!/usr/bin/env bash
set -euo pipefail

clear

echo testing DK...

echo - disassemble
rm -f dk.dis
stack run -- --dis data/dk.nes > dk.dis
wc -l dk.dis

echo - emulate
rm -f dk.log
#(stack run -- --emu data/dk.nes > dk.log) || true
(stack run -- data/dk.nes > dk.log) || true
wc -l dk.log
