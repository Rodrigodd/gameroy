## Test

Most test depends on [c-sp/gameboy-test-roms v.51](https://github.com/c-sp/gameboy-test-roms/releases/tag/v5.1).

To run all tests, download and extract the suite to `gameroy/core/tests/gameboy-test-roms`,
then go to the project root and run the command:

```shell
curl -o core/tests/gameboy-test-roms.zip -L https://github.com/c-sp/gameboy-test-roms/releases/download/v5.1/game-boy-test-roms-v5.1.zip
unzip core/tests/gameboy-test-roms.zip -d core/tests/gameboy-test-roms
cargo test -p gameroy-core
```
