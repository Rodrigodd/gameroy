rustc reference.rs --emit=asm --crate-type=lib --edition=2021 \
    -C opt-level=3 -C "llvm-args=-x86-asm-syntax=intel" \
    -C force-frame-pointers=yes \
    && rustfilt -i reference.s \
    | rg "^\s+\." -v \
    | rg 'qword ptr' -r QWORD --passthru \
    | rg 'dword ptr' -r DWORD --passthru \
    | rg 'word ptr' -r WORD --passthru \
    | rg 'byte ptr' -r BYTE --passthru \
    > reference.s
