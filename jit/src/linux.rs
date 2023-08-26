use std::{fs::OpenOptions, io::Write};

/// Emmit symbol information to `/tmp/perf-$PID.map`, enabling `perf`'s JIT support.
///
/// See: https://lkml.org/lkml/2009/6/8/499
///      https://github.com/jatovm/jato/blob/bb1c7d4fd987e016b2e0379182c4bfbb8c1c1a78/jit/perf-map.c#L32
pub fn write_to_perf_map(symbol: &str, addr: usize, size: usize) -> std::io::Result<()> {
    let pid = std::process::id();
    let path = format!("/tmp/perf-{}.map", pid);
    let mut file = OpenOptions::new().create(true).append(true).open(path)?;
    writeln!(file, "{:x} {} {}", addr, size, symbol)?;
    Ok(())
}
