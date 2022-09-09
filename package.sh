#!/bin/bash

# a script for packaging the app for linux release.

cargo about generate -m native\Cargo.toml -c license\about.toml license\about.hbs > license\license.html
cargo build --profile minsize --features=static,rfd,audio-engine

mkdir package
cp -f target/minsize/native package/gameroy
cp -f gameroy.toml package/
cp -f license/license.html package/

cd package
echo '' > gameroy.log
zip gameroy-x86_64-unknown-linux-gnu.zip gameroy gameroy.log gameroy.toml license.html

cd ..
