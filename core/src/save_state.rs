use std::cell::Cell;
use std::io::{Read, Write};

#[derive(Debug)]
pub enum LoadStateError {
    InvalidImeState(u8),
    InvalidState(u8),
    InvalidPpuMode(u8),
    InvalidBool(u8),
    InvalidBoolBitArray(u8, u8),
    SoundControllerDesync(u64, u64),
    ConstMismatch(String, String),
    IoError(std::io::Error),
    InvalidMagicConst([u8; 4]),
    UnknownVersion(u32),
}
impl From<std::io::Error> for LoadStateError {
    fn from(error: std::io::Error) -> Self {
        Self::IoError(error)
    }
}

pub trait BoolExt {
    fn get(&self) -> bool;
    fn get_mut(&mut self) -> &mut bool;
}

impl BoolExt for bool {
    fn get(&self) -> bool {
        *self
    }

    fn get_mut(&mut self) -> &mut bool {
        self
    }
}

/// The Header of a save state. Contains some metadata like version and time of save.
#[derive(Debug)]
pub struct SaveStateHeader {
    /// "GameRoy Save State" magic contant.
    ///
    /// Should have value b"GRST".
    pub magic: [u8; 4],
    /// The version of this save
    pub version: u32,
    /// The instant that this file was saved, in number of milliseconds since the UNIX_EPOCH
    pub time: Option<u64>,
}
impl SaveStateHeader {
    /// The current version of the save state format
    const SAVE_STATE_VERSION: u32 = 2;
    const MAGIC_CONST: [u8; 4] = *b"GRST";

    /// Create a new SaveStateHeader with default values and current SystemTime.
    #[cfg(not(target_arch = "wasm32"))]
    pub fn new() -> Self {
        use std::time::{Duration, SystemTime};

        use std::convert::TryInto;

        // TODO: this does not work for negative values. Maybe need to use a time crate? In this
        // case the time would need to be provide by the calee (to avoid dependencies).
        let since_epoch = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH);
        let time: Result<u64, _> = since_epoch.unwrap_or(Duration::ZERO).as_millis().try_into();
        // Also not handle times after 584 millions years after epoch.
        let time = time.unwrap_or(u64::max_value());

        Self {
            magic: Self::MAGIC_CONST,
            version: Self::SAVE_STATE_VERSION,
            time: Some(time),
        }
    }

    #[cfg(target_arch = "wasm32")]
    pub fn new() -> Self {
        Self::default()
    }
}
impl Default for SaveStateHeader {
    fn default() -> Self {
        Self {
            magic: Self::MAGIC_CONST,
            version: Self::SAVE_STATE_VERSION,
            time: None,
        }
    }
}
impl SaveState for SaveStateHeader {
    fn save_state(&self, data: &mut impl Write) -> Result<(), std::io::Error> {
        self.magic.save_state(data)?;
        self.version.save_state(data)?;
        if let Some(time) = self.time {
            time.save_state(data)?;
        } else {
            u64::max_value().save_state(data)?;
        }
        Ok(())
    }

    fn load_state(&mut self, data: &mut impl Read) -> Result<(), LoadStateError> {
        self.magic.load_state(data)?;
        self.version.load_state(data)?;

        if self.version > 1 {
            let mut time = 0;
            time.load_state(data)?;
            self.time = (time != u64::max_value()).then_some(time);
        } else {
            self.time = None;
        }

        if self.magic != Self::MAGIC_CONST {
            return Err(LoadStateError::InvalidMagicConst(self.magic));
        }

        if self.version > Self::SAVE_STATE_VERSION {
            return Err(LoadStateError::UnknownVersion(self.version));
        }

        Ok(())
    }
}

pub trait SaveState {
    fn save_state(&self, data: &mut impl Write) -> Result<(), std::io::Error>;
    fn load_state(&mut self, data: &mut impl Read) -> Result<(), LoadStateError>;
}

impl SaveState for u8 {
    fn save_state(&self, data: &mut impl Write) -> Result<(), std::io::Error> {
        data.write_all(&[*self])?;
        Ok(())
    }

    fn load_state(&mut self, data: &mut impl Read) -> Result<(), LoadStateError> {
        data.read_exact(std::slice::from_mut(self))?;
        Ok(())
    }
}

#[macro_export]
macro_rules! save_state {
    // end
    (@accum ($n:ident, $s:ident, $d:ident,) -> ($($save:tt)*) -> ($($load:tt)*)) => {
        impl SaveState for $n {
            fn save_state(&$s, $d: &mut impl std::io::Write) -> Result<(), std::io::Error> {
                $($save)*
                let _ = $d;
                Ok(())
            }

            fn load_state(&mut $s, $d: &mut impl std::io::Read) -> Result<(), LoadStateError> {
                $($load)*
                let _ = $d;
                Ok(())
            }
        }
    };
    // const <expr>
    (@accum ($n:ident, $s:ident, $d:ident, const $e:expr; $($f:tt)* ) -> ($($save:tt)*) -> ($($load:tt)*)) => {
        $crate::save_state!(
            @accum ($n, $s, $d, $($f)* )
            -> ($($save)* ($e).save_state($d)?; )
            -> ($($load)* {
                let expected = $e;
                let mut loaded = expected;
                loaded.load_state($d)?;
                if loaded != expected {
                    LoadStateError::ConstMismatch(format!("{:?}", loaded), format!("{:?}", expected));
                }
            })
        );
    };
    // bitset [<expr>*]
    (@accum ($n:ident, $s:ident, $d:ident, bitset [ $($e:expr),* ]; $($f:tt)* ) -> ($($save:tt)*) -> ($($load:tt)*)) => {
        $crate::save_state!(
            @accum ($n, $s, $d, $($f)* )
            -> ($($save)* { use $crate::save_state::BoolExt; [ $( &    $e.get()),* ].save_state($d)?; } )
            -> ($($load)* { use $crate::save_state::BoolExt; [ $( $e.get_mut()),* ].load_state($d)?; } )
        );
    };
    // <expr>
    (@accum ($n:ident, $s:ident, $d:ident, $e:expr; $($f:tt)* ) -> ($($save:tt)*) -> ($($load:tt)*)) => {
        $crate::save_state!(
            @accum ($n, $s, $d, $($f)* )
            -> ($($save)* ($e).save_state($d)?; )
            -> ($($load)* ($e).load_state($d)?; )
        );
    };
    // entry
    ($n:ident, $s:ident, $d:ident { $($f:tt)* }) => {
        $crate::save_state!(
            @accum ($n, $s, $d, $($f)* )
            -> ()
            -> ()
        );
    };
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
        data.write_all(&(self.len() as u32).to_be_bytes())?;
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
            data.write_all(&[flags])?;
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
                flags >>= 1;
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

impl<T: SaveState + Default + Copy> SaveState for Cell<T> {
    fn save_state(&self, data: &mut impl Write) -> Result<(), std::io::Error> {
        self.get().save_state(data)?;
        Ok(())
    }

    fn load_state(&mut self, data: &mut impl Read) -> Result<(), LoadStateError> {
        let mut x = T::default();
        x.load_state(data)?;
        self.set(x);
        Ok(())
    }
}
