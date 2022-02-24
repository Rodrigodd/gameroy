use std::io::{Read, Write};

#[derive(Debug)]
pub enum LoadStateError {
    InvalidImeState(u8),
    InvalidState(u8),
    InvalidPpuMode(u8),
    InvalidBool(u8),
    InvalidBoolBitArray(u8, u8),
    SoundControllerDesync(u64, u64),
    IoError(std::io::Error),
}
impl From<std::io::Error> for LoadStateError {
    fn from(error: std::io::Error) -> Self {
        Self::IoError(error)
    }
}

pub trait SaveState {
    fn save_state(&self, data: &mut impl Write) -> Result<(), std::io::Error>;
    fn load_state(&mut self, data: &mut impl Read) -> Result<(), LoadStateError>;
}

impl SaveState for u8 {
    fn save_state(&self, data: &mut impl Write) -> Result<(), std::io::Error> {
        data.write(&[*self])?;
        Ok(())
    }

    fn load_state(&mut self, data: &mut impl Read) -> Result<(), LoadStateError> {
        data.read_exact(std::slice::from_mut(self))?;
        Ok(())
    }
}

impl SaveState for u16 {
    fn save_state(&self, data: &mut impl Write) -> Result<(), std::io::Error> {
        data.write_all(&self.to_be_bytes())?;
        Ok(())
    }

    fn load_state(&mut self, data: &mut impl Read) -> Result<(), LoadStateError> {
        let mut bytes = [0; 2];
        data.read_exact(&mut bytes)?;
        *self = u16::from_be_bytes(bytes);
        Ok(())
    }
}

impl SaveState for u32 {
    fn save_state(&self, data: &mut impl Write) -> Result<(), std::io::Error> {
        data.write_all(&self.to_be_bytes())?;
        Ok(())
    }

    fn load_state(&mut self, data: &mut impl Read) -> Result<(), LoadStateError> {
        let mut bytes = [0; 4];
        data.read_exact(&mut bytes)?;
        *self = u32::from_be_bytes(bytes);
        Ok(())
    }
}

impl SaveState for u64 {
    fn save_state(&self, data: &mut impl Write) -> Result<(), std::io::Error> {
        data.write_all(&self.to_be_bytes())?;
        Ok(())
    }

    fn load_state(&mut self, data: &mut impl Read) -> Result<(), LoadStateError> {
        let mut bytes = [0; 8];
        data.read_exact(&mut bytes)?;
        *self = u64::from_be_bytes(bytes);
        Ok(())
    }
}

impl<T: SaveState, const N: usize> SaveState for [T; N] {
    fn save_state(&self, data: &mut impl Write) -> Result<(), std::io::Error> {
        for x in self {
            x.save_state(data)?;
        }
        Ok(())
    }

    fn load_state(&mut self, data: &mut impl Read) -> Result<(), LoadStateError> {
        for x in self {
            x.load_state(data)?;
        }
        Ok(())
    }
}

impl SaveState for Vec<u8> {
    fn save_state(&self, data: &mut impl Write) -> Result<(), std::io::Error> {
        data.write(&(self.len() as u32).to_be_bytes())?;
        data.write_all(self)?;
        Ok(())
    }

    fn load_state(&mut self, data: &mut impl Read) -> Result<(), LoadStateError> {
        let mut bytes = [0; 4];
        data.read_exact(&mut bytes)?;
        let len = u32::from_be_bytes(bytes);
        self.resize(len as usize, 0);
        data.read_exact(self)?;
        Ok(())
    }
}

impl<const N: usize> SaveState for [&bool; N] {
    fn save_state(&self, data: &mut impl Write) -> Result<(), std::io::Error> {
        if N <= 8 {
            let mut flags = 0;
            for &&b in self {
                flags = (flags << 1) | b as u8;
            }
            data.write(&[flags])?;
            Ok(())
        } else {
            unimplemented!()
        }
    }

    fn load_state(&mut self, _data: &mut impl Read) -> Result<(), LoadStateError> {
        unimplemented!()
    }
}

impl<const N: usize> SaveState for [&mut bool; N] {
    fn save_state(&self, _data: &mut impl Write) -> Result<(), std::io::Error> {
        unimplemented!()
    }

    fn load_state(&mut self, data: &mut impl Read) -> Result<(), LoadStateError> {
        if N <= 8 {
            let mut flags = 0;
            data.read_exact(std::slice::from_mut(&mut flags))?;
            for b in self.iter_mut().rev() {
                **b = flags & 0x1 != 0;
                flags = flags >> 1;
            }
            if flags != 0 {
                return Err(LoadStateError::InvalidBoolBitArray(flags, N as u8));
            }
            Ok(())
        } else {
            unimplemented!()
        }
    }
}
