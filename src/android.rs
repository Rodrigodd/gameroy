#[ndk_glue::main(
    backtrace = "on",
    ndk_glue = "ndk_glue",
    logger(
        level = "trace",
        tag = "gameroy",
        filter = "gameroy,raw_gl_context::android,giui=debug,winit=debug,audio_engine=trace"
        // filter = "debug"
    )
)]
fn main() {
    log::info!("Starting android gameroy!");
    super::main()
}
