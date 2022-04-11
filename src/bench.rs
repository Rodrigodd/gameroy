use std::{
    path::PathBuf,
    time::{Duration, Instant},
};

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
    let (_, mut game_boy) = match super::load_gameboy(&rom_path, None) {
        Ok(x) => x,
        Err(e) => return eprintln!("{}", e),
    };
    let mut inter = Interpreter(&mut game_boy);

    let step = timeout / len as u64;
    let mut timeout = 0;

    let mut times = Vec::with_capacity(len);
    times.push((Instant::now(), inter.0.clock_count));
    for _ in 0..len {
        timeout += step;
        while inter.0.clock_count < timeout {
            inter.interpret_op();
        }
        times.push((Instant::now(), inter.0.clock_count));
    }

    let samples = times
        .windows(2)
        .map(|x| [x[0].0, x[1].0])
        // the first sample is strange, don't know why
        .skip(1)
        .map(|x| x[1] - x[0])
        .collect::<Vec<_>>();

    let (mean, error) = mean(&samples);
    let total = mean * samples.len() as u32;
    let total_err = error * samples.len() as u32;
    println!("total time: {:?} +/- {:?}", total, total_err);

    let total_cycles = times[len - 1].1 - times[1].1;
    let real_time = total_cycles as f64 / gameroy::consts::CLOCK_SPEED as f64;
    let times = real_time / total.as_secs_f64();
    let times_err = times * total_err.as_secs_f64() / total.as_secs_f64();
    println!(
        "            {} times faster than real time.",
        print_val(times, times_err),
    );
}
