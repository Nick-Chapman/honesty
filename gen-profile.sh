#!/usr/bin/env bash
pro-stack build
pro-stack exec -- nes --speed --max-frames 20 +RTS -p -RTS
