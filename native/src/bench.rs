use std::{
    path::PathBuf,
    time::{Duration, Instant},
};

use gameroy_lib::gameroy::{
    consts::CLOCK_SPEED,
    gameboy::{cartridge::Cartridge, GameBoy},
    interpreter::Interpreter,
};

use gameroy_jit::JitCompiler;

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

    // HACKY: handle this better
    if error.is_nan() {
        return (mean, Duration::ZERO);
    }
    (mean, Duration::from_secs_f64(error))
}

fn print_val(val: f64, err: f64) -> String {
    let p = if err.is_nan() || err == 0.0 {
        (-val.log10()).ceil() as usize + 1
    } else {
        (-err.log10()).ceil() as usize + 1
    };
    format!("{:.p$} +/- {:.p$}", val, err, p = p)
}

pub fn benchmark(
    path: &str,
    timeout: u64,
    number_of_times: usize,
    predict_interrupt: bool,
    jit: bool,
) {
    let len = number_of_times + 1;

    let rom_path = PathBuf::from(path);
    let rom = std::fs::read(rom_path);

    let rom = match rom {
        Ok(x) => x,
        Err(e) => return eprintln!("failed to load '{}': {}", path, e),
    };

    let cartridge = Cartridge::new(rom).unwrap();
    let mut game_boy = GameBoy::new(None, cartridge);
    game_boy.predict_interrupt = predict_interrupt;

    // remove serial transfer console output
    game_boy.serial.get_mut().serial_transfer_callback = None;

    let mut times = if jit {
        run_jitted(len, &mut game_boy, timeout)
    } else {
        run_interpreted(len, &mut game_boy, timeout)
    };

    // Remove first run, because in that one the code is compiled and traced.
    times.remove(0);

    let (mean_time, mean_error) = mean(&times);
    println!("mean time: {:?} +/- {:?}", mean_time, mean_error);

    let emulated_time = game_boy.clock_count as f64 / CLOCK_SPEED as f64;
    let times = emulated_time / mean_time.as_secs_f64();
    let times_err = times * mean_error.as_secs_f64() / mean_time.as_secs_f64();
    println!(
        "            {} times faster than real time.",
        print_val(times, times_err),
    );
    log::info!("faster than real time: {} +/- {}", times, times_err);
}

fn run_interpreted(len: usize, game_boy: &mut GameBoy, timeout: u64) -> Vec<Duration> {
    let mut times = Vec::with_capacity(len);
    for _ in 0..len {
        game_boy.reset();
        let start = Instant::now();
        while game_boy.clock_count < timeout {
            Interpreter(game_boy).interpret_op();
        }
        times.push(start.elapsed());
    }
    times
}

fn run_jitted(len: usize, game_boy: &mut GameBoy, timeout: u64) -> Vec<Duration> {
    let mut jit_compiler = JitCompiler::new();

    let mut times = Vec::with_capacity(len);
    for _ in 0..len {
        game_boy.reset();
        let start = Instant::now();
        while game_boy.clock_count < timeout {
            jit_compiler.interpret_block(game_boy);
        }
        times.push(start.elapsed());
    }
    times
}
