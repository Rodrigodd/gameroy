use std::borrow::Cow;

use gameroy::gameboy::cartridge::CartridgeHeader;
use wasm_bindgen::{closure::Closure, JsCast, JsValue};

pub fn load_roms(_roms_path: &str) -> Result<Vec<RomFile>, String> {
    Ok(Vec::new())
}

pub fn load_boot_rom() -> Option<[u8; 256]> {
    None
}
pub fn load_file(file_name: &str) -> Result<Vec<u8>, String> {
    let window = web_sys::window().ok_or_else(|| "window object is null".to_string())?;
    let local_storage = window
        .local_storage()
        .map_err(|_| "error getting local storage".to_string())?
        .ok_or_else(|| "local storage is null".to_string())?;

    let save = local_storage
        .get_item(file_name)
        .map_err(|_| "error getting item from local storage".to_string())?
        .ok_or_else(|| "save not found".to_string())?;

    base64::decode(save).map_err(|err| format!("failed decoding save: {}", err.to_string()))
}

pub fn save_file(file_name: &str, data: &[u8]) -> Result<(), String> {
    let window = web_sys::window().ok_or_else(|| "window object is null".to_string())?;
    let local_storage = window
        .local_storage()
        .map_err(|_| "error getting local storage".to_string())?
        .ok_or_else(|| "local storage is null".to_string())?;

    let save = base64::encode(data);

    local_storage
        .set_item(file_name, &save)
        .map_err(|_| "error setting item in local storage".to_string())?;

    Ok(())
}

#[derive(Clone, Debug)]
pub struct RomFile {
    web_file: web_sys::File,
}
impl RomFile {
    pub async fn get_header(&self) -> Result<CartridgeHeader, String> {
        let mut data = self.read().await?;
        let header = match CartridgeHeader::from_bytes(&mut data) {
            Ok(x) | Err((Some(x), _)) => x,
            Err((_, e)) => return Err(e),
        };
        Ok(header)
    }

    pub fn file_name(&self) -> Cow<str> {
        self.web_file.name().into()
    }

    pub async fn read(&self) -> Result<Vec<u8>, String> {
        let promise = js_sys::Promise::new(&mut move |res, _rej| {
            let file_reader = web_sys::FileReader::new().unwrap();

            let fr = file_reader.clone();
            let closure = Closure::wrap(Box::new(move || {
                res.call1(&JsValue::undefined(), &fr.result().unwrap())
                    .unwrap();
            }) as Box<dyn FnMut()>);

            file_reader.set_onload(Some(closure.as_ref().unchecked_ref()));

            closure.forget();

            file_reader.read_as_array_buffer(&self.web_file).unwrap();
        });

        let future = wasm_bindgen_futures::JsFuture::from(promise);

        let res = future.await.unwrap();

        let buffer: js_sys::Uint8Array = js_sys::Uint8Array::new(&res);
        let mut vec = vec![0; buffer.length() as usize];
        buffer.copy_to(&mut vec[..]);

        Ok(vec)
    }

    pub fn save_ram_data(&self, data: &[u8]) -> Result<(), String> {
        let file_name = self.file_name().to_string() + ".sav";

        save_file(&file_name, data)
    }

    pub async fn load_ram_data(&self) -> Result<Vec<u8>, String> {
        let file_name = self.file_name().to_string() + ".sav";

        load_file(&file_name)
    }

    pub fn save_state(&self, state: &[u8]) -> Result<(), String> {
        let file_name = self.file_name().to_string() + ".save_state";

        save_file(&file_name, state)
    }

    pub fn load_state(&self) -> Result<Vec<u8>, String> {
        let file_name = self.file_name().to_string() + ".save_state";

        load_file(&file_name)
    }
}
#[cfg(feature = "rfd")]
impl From<rfd::FileHandle> for RomFile {
    fn from(handle: rfd::FileHandle) -> Self {
        Self {
            web_file: handle.inner().clone(),
        }
    }
}
