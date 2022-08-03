#[cfg_attr(target_os = "android", ndk_glue::main(
    backtrace = "on",
    ndk_glue = "ndk_glue",
    logger(
        level = "trace",
        tag = "gameroy",
        filter = "gameroy,raw_gl_context::android,giui=debug,winit=debug,audio_engine=debug,rfd=debug"
        // filter = "debug"
    )
))]
pub fn main() {
    gameroy_lib::log_panic();
    log::info!("Starting android gameroy!");
    gameroy_lib::main(None, None)
}

/// This function receives the file_picker_result from Java, and repass it to rfd
#[cfg(target_os = "android")]
#[allow(non_snake_case)]
#[no_mangle]
pub extern "C" fn Java_io_github_rodrigodd_gameroy_MainActivity_filePickerResult(
    env: jni::JNIEnv,
    class: jni::objects::JObject,
    callback_ptr: jni::sys::jlong,
    uri: jni::objects::JString,
) {
    gameroy_lib::rfd::file_picker_result(env, class, callback_ptr, uri)
}
