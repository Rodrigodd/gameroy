use instant::Instant;
use std::{path::PathBuf, time::Duration};

use gameroy::interpreter::Interpreter;

// Return the mean and standart error of the samples
fn mean(samples: &[Duration]) -> (Duration, Duration) {
    let sum: Duration = samples.iter().sum();
    let mean = sum / samples.len() as u32;

    let sample_varience_sq: f64 = samples
        .iter()
        .copied()
        .map(|x| (x.as_secs_f64() - mean.as_secs_f64()).powi(2))
        .sum::<f64>()
        / (samples.len() - 1) as f64;

    let error = (sample_varience_sq / samples.len() as f64).sqrt();

    (mean, Duration::from_secs_f64(error))
}

fn print_val(val: f64, err: f64) -> String {
    let p = (-err.log10()).ceil() as usize + 1;
    format!("{:.p$} +/- {:.p$}", val, err, p = p)
}

pub fn benchmark(path: &str, timeout: u64, len: usize) {
    let rom_path = PathBuf::from(path);
    let gb = crate::ui::RomEntry::from_path(rom_path).and_then(|x| x.load_gameboy());
    let mut game_boy = match gb {
        Ok(x) => x,
        Err(e) => return eprintln!("{}", e),
    };

    // remove serial transfer console output
    game_boy.serial_transfer_callback = None;

    let mut times = Vec::with_capacity(len);
    for _ in 0..len {
        game_boy.reset();
        let mut inter = Interpreter(&mut game_boy);
        let start = Instant::now();
        while inter.0.clock_count < timeout {
            inter.interpret_op();
        }
        times.push(start.elapsed());
    }

    let (mean_time, mean_error) = mean(&times);
    println!("mean time: {:?} +/- {:?}", mean_time, mean_error);

    let emulated_time = game_boy.clock_count as f64 / gameroy::consts::CLOCK_SPEED as f64;
    let times = emulated_time / mean_time.as_secs_f64();
    let times_err = times * mean_error.as_secs_f64() / mean_time.as_secs_f64();
    println!(
        "            {} times faster than real time.",
        print_val(times, times_err),
    );
}
