use criterion::{criterion_group, criterion_main, Criterion};
use gameroy::consts::CLOCK_SPEED;
use gameroy::gameboy::cartridge::Cartridge;
use gameroy::gameboy::GameBoy;
use gameroy::interpreter::Interpreter;

fn criterion_benchmark(c: &mut Criterion) {
    let rom = std::fs::read("../roms/Kirby's Dream Land (USA, Europe).gb").unwrap();
    let cartridge = Cartridge::new(rom).unwrap();
    let mut gb = GameBoy::new(None, cartridge);

    let save_state = {
        let mut save_state = Vec::new();

        let target_clock = gb.clock_count + CLOCK_SPEED * 10;

        while gb.clock_count < target_clock {
            Interpreter(&mut gb).interpret_op();
        }
        gb.save_state(None, &mut save_state).unwrap();

        save_state
    };

    gb.predict_interrupt = true;
    c.bench_function("kirby 2s", |b| {
        let gb = &mut gb;
        gb.load_state(&mut save_state.as_slice()).unwrap();

        // will run some seconds of emulated time.
        let target_clock = gb.clock_count + CLOCK_SPEED * 2;

        b.iter(move || {
            while gb.clock_count < target_clock {
                Interpreter(gb).interpret_op()
            }
        })
    });

    gb.predict_interrupt = false;
    c.bench_function("kirby 2s - no interrupt prediction", |b| {
        let gb = &mut gb;
        gb.load_state(&mut save_state.as_slice()).unwrap();

        // will run some seconds of emulated time.
        let target_clock = gb.clock_count + CLOCK_SPEED * 2;

        b.iter(move || {
            while gb.clock_count < target_clock {
                Interpreter(gb).interpret_op()
            }
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
