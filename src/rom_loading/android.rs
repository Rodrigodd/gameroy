use std::borrow::Cow;

use jni::objects::JString;

pub fn load_roms(roms_path: &str) -> Result<Vec<RomFile>, std::io::Error> {
    log::trace!("loading rom list in android from uri '{}'", roms_path);
    let android_context = ndk_context::android_context();
    let vm =
        std::sync::Arc::new(unsafe { jni::JavaVM::from_raw(android_context.vm().cast()).unwrap() });
    let childs = jni::Executor::new(vm)
        .with_attached(|env| {
            let roms_path = env.new_string(roms_path)?;
            let string_array = env.call_method(
                android_context.context() as jni::sys::jobject,
                "listChild",
                "(Ljava/lang/String;)[Ljava/lang/String;",
                &[roms_path.into()],
            )?;

            let string_array = match string_array {
                jni::objects::JValue::Object(x) => {
                    if x.is_null() {
                        return Ok(None);
                    }
                    x
                }
                _ => return Err(jni::errors::Error::WrongJValueType("a", "b")),
            };

            let size = env.get_array_length(*string_array)?;
            let mut childs = Vec::with_capacity(size as usize);
            for i in 0..size {
                let obj = env.get_object_array_element(*string_array, i)?;
                let string = env
                    .get_string(JString::from(obj))?
                    .to_str()
                    // TODO: I don't know if this is true.
                    .expect("android.net.Uri.toString should give a valid UTF-8, rigth?")
                    .to_string();
                childs.push(string);
            }

            Ok(Some(childs))
        })
        .unwrap();

    let childs = match childs {
        Some(x) => x,
        None => return Err(panic!("failed to load children of uri: {}", roms_path)),
    };

    Ok(childs
        .into_iter()
        .map(|x| rfd::FileHandle::wrap(x).into())
        .collect::<Vec<RomFile>>())
}

pub fn load_boot_rom() -> Option<[u8; 256]> {
    None
}

pub fn load_file(file_name: &str) -> Option<Vec<u8>> {
    let android_context = ndk_context::android_context();
    let vm =
        std::sync::Arc::new(unsafe { jni::JavaVM::from_raw(android_context.vm().cast()).unwrap() });
    jni::Executor::new(vm)
        .with_attached(|env| {
            let filename = env.new_string(file_name)?;
            let buffer = env.call_method(
                android_context.context() as jni::sys::jobject,
                "loadRam",
                "(Ljava/lang/String;)Ljava/nio/ByteBuffer;",
                &[filename.into()],
            )?;

            let buffer = match buffer {
                jni::objects::JValue::Object(x) => {
                    if x.is_null() {
                        return Ok(None);
                    }
                    jni::objects::JByteBuffer::from(x)
                }
                _ => return Err(jni::errors::Error::WrongJValueType("a", "b")),
            };

            let data = env.get_direct_buffer_address(buffer)?.to_vec();

            Ok(Some(data))
        })
        .unwrap()
}

pub fn save_file(file_name: &str, data: &[u8]) {
    let android_context = ndk_context::android_context();
    let vm =
        std::sync::Arc::new(unsafe { jni::JavaVM::from_raw(android_context.vm().cast()).unwrap() });

    jni::Executor::new(vm)
        .with_attached(|env| {
            let filename = env.new_string(file_name)?;

            let mut data = data.to_vec();
            let buffer = env.new_direct_byte_buffer(&mut data)?;

            env.call_method(
                android_context.context() as jni::sys::jobject,
                "saveRam",
                "(Ljava/lang/String;Ljava/nio/ByteBuffer;)V",
                &[filename.into(), buffer.into()],
            )?;

            Ok(())
        })
        .unwrap();
}

#[derive(Clone, Debug)]
pub struct RomFile {
    uri: String,
}
impl RomFile {
    pub fn file_name(&self) -> Cow<str> {
        self.uri
            .rsplit_once("/")
            .map_or(self.uri.as_str(), |x| x.1)
            .into()
    }

    pub async fn read(&self) -> Result<Vec<u8>, String> {
        Ok(rfd::FileHandle::wrap(self.uri.clone()).read().await)
    }

    pub fn save_ram_data(&self, data: &[u8]) -> Result<(), String> {
        let file_name = self.file_name().to_owned() + ".sav";

        save_file(&file_name, data);
        Ok(())
    }

    pub async fn load_ram_data(&self) -> Result<Vec<u8>, String> {
        let file_name = self.file_name().to_owned() + ".sav";

        load_file(&file_name).ok_or_else(|| "load save failed".to_string())
    }

    pub fn save_state(&self, state: &[u8]) -> Result<(), String> {
        let file_name = self.file_name().to_owned() + ".save_state";

        save_file(&file_name, state);
        Ok(())
    }

    pub fn load_state(&self) -> Result<Vec<u8>, String> {
        let file_name = self.file_name().to_owned() + ".save_state";

        load_file(&file_name).ok_or_else(|| "load save state failed".to_string())
    }
}
#[cfg(feature = "rfd")]
impl From<rfd::FileHandle> for RomFile {
    fn from(handle: rfd::FileHandle) -> Self {
        Self {
            uri: handle.inner().to_string(),
        }
    }
}
