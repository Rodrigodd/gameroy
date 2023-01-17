@echo off

::a script for packaging the app for windows release.

cargo about generate -m native\Cargo.toml -c license\about-windows.toml license\about.hbs > license\license.html
cargo build -p native --profile minsize --no-default-features --features=static,rfd,audio-engine,threads

mkdir package
copy /Y target\minsize\native.exe package\gameroy.exe
copy /Y gameroy.toml package\
copy /Y license\license.html package\

cd package
echo '' > gameroy.log
zip gameroy-x86_64-pc-windows-msvc.zip gameroy.exe gameroy.log gameroy.toml license.html

cd ..
