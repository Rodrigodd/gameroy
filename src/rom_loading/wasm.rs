use std::borrow::Cow;

use gameroy::gameboy::cartridge::CartridgeHeader;

use wasm_bindgen::{closure::Closure, JsCast, JsValue};

pub fn load_boot_rom() -> Option<[u8; 256]> {
    None
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

    pub async fn load_ram_data(&self) -> Result<Vec<u8>, String> {
        Err("unimplemented".to_string())
    }

    pub fn save_ram_data(&self, data: &[u8]) -> Result<(), String> {
        Err("unimplemented".to_string())
    }

    pub fn save_state(&self, state: &[u8]) -> Result<(), String> {
        Err("unimplemented".to_string())
    }

    pub fn load_state(&self) -> Result<Vec<u8>, String> {
        Err("unimplemented".to_string())
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
