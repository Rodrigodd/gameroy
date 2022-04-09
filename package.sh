#!/bin/bash

# a script for packaging the app for linux release.

cargo about generate -c license/about.toml license/about.hbs > license/license.html
cargo build --profile minsize --features=static

mkdir package
cp -f target/minsize/gameroy package/
cp -f gameroy.toml package/
cp -f license/license.html package/

cd package
echo '' > gameroy.log
zip gameroy-x86_64-unknown-linux-gnu.zip gameroy gameroy.log gameroy.toml license.html
