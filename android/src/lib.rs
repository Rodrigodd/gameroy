#[cfg_attr(target_os = "android", ndk_glue::main(
    backtrace = "on",
    ndk_glue = "ndk_glue",
    logger(
        level = "trace",
        tag = "gameroy",
        filter = "gameroy,raw_gl_context::android,giui=debug,winit=debug,audio_engine=trace"
        // filter = "debug"
    )
))]
pub fn main() {
    log::info!("Starting android gameroy!");
    gameroy_lib::main()
}

#[allow(non_snake_case)]
#[no_mangle]
pub extern "C" fn Java_io_github_rodrigodd_gameroy_MainActivity_helloWorld(
    _env: jni::JNIEnv,
    _class: jni::objects::JObject,
    _activity: jni::objects::JObject,
) {
    log::info!("Hello Android!!!");
}
