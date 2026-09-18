#!/bin/sh
# Guards the client/server split.
#
# The protocol half of gameroy-server must compile, and link, without the
# emulator. That is what lets a front-end talk to a server it does not host --
# in another process, or a web worker. If this fails, the boundary has rotted.
#
# Note the *embedded* front-end does link gameroy-core, and should: it hosts the
# server in-process. The invariant is about the featureless build, not the client.
set -eu

cd "$(dirname "$0")/../.."

echo "building gameroy-server --no-default-features ..."
cargo build -p gameroy-server --no-default-features

echo "checking its dependency tree ..."
tree=$(cargo tree -p gameroy-server --no-default-features -e normal)
if echo "$tree" | grep -qE 'gameroy-core|gameroy-jit|flume'; then
    echo "FAIL: the protocol-only build pulls in the emulator or a transport:"
    echo "$tree" | grep -E 'gameroy-core|gameroy-jit|flume'
    exit 1
fi

echo "OK: protocol builds free of the emulator."
