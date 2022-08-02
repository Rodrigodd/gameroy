#![cfg_attr(
    all(target_os = "windows", not(feature = "console")),
    windows_subsystem = "windows"
)]
#![cfg_attr(
    all(target_os = "windows", feature = "console"),
    windows_subsystem = "console"
)]

fn main() {
    gameroy_lib::main(None, None)
}
