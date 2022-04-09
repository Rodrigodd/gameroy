@echo off

::a script for packaging the app for windows release.

cargo about generate -c license\about.toml license\about.hbs > license\license.html
cargo build --profile minsize --no-default-features --features=static

mkdir package
copy /Y target\minsize\gameroy.exe package\
copy /Y gameroy.toml package\
copy /Y license\license.html package\

cd package
echo '' > gameroy.log
zip gameroy-x86_64-pc-windows-msvc.zip gameroy.exe gameroy.log gameroy.toml license.html

cd ..
