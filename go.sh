#!/usr/bin/env bash
set -euo pipefail

stack run > dis.log
cmp dis.log{.expected,}
#diff dis.log{.expected,} | colordiff
