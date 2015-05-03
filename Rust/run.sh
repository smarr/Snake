#!/bin/sh
cargo build || exit 1

stty -echo -echok -icanon min 1 time 0
cargo run
stty echo echok icanon
