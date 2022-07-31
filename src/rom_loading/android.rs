use std::borrow::Cow;

pub fn load_boot_rom() -> Option<[u8; 256]> {
    None
}

#[derive(Clone, Debug)]
pub struct RomFile {
    uri: String,
}
impl RomFile {
    pub fn file_name(&self) -> Cow<str> {
        self.uri.clone().into()
    }

    pub async fn read(&self) -> Result<Vec<u8>, String> {
        Ok(rfd::FileHandle::wrap(self.uri.clone()).read().await)
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
            uri: handle.inner().to_string(),
        }
    }
}
