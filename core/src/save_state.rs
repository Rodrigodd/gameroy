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

/// Context used throughout the serialization process.
#[derive(Clone)]
pub struct SaveStateContext {
    /// The save state format version.
    pub version: u32,
    /// The instant that this file was saved, in number of milliseconds since the UNIX_EPOCH
    pub time: Option<u64>,
    /// The clock_count of the GameBoy.
    pub clock_count: Option<u64>,
}

impl SaveStateContext {
    pub fn new(time: u64, clock_count: u64) -> Self {
        Self {
            version: SaveStateHeader::SAVE_STATE_VERSION,
            time: Some(time),
            clock_count: Some(clock_count),
        }
    }
}

impl Default for SaveStateContext {
    fn default() -> Self {
        Self {
            version: SaveStateHeader::SAVE_STATE_VERSION,
            time: None,
            clock_count: None,
        }
    }
}

/// The Header of a save state. Contains some metadata like version and time of save.
#[derive(Debug)]
pub struct SaveStateHeader;
impl SaveStateHeader {
    /// The current version of the save state format
    const SAVE_STATE_VERSION: u32 = 2;

    /// "GameRoy Save State" magic contant.
    const MAGIC_CONST: [u8; 4] = *b"GRST";
}
impl SaveState for SaveStateHeader {
    fn save_state(
        &self,
        ctx: &mut SaveStateContext,
        data: &mut impl Write,
    ) -> Result<(), std::io::Error> {
        Self::MAGIC_CONST.save_state(ctx, data)?;
        Self::SAVE_STATE_VERSION.save_state(ctx, data)?;
        if let Some(time) = ctx.time {
            time.save_state(ctx, data)?;
        } else {
            u64::max_value().save_state(ctx, data)?;
        }
        Ok(())
    }

    fn load_state(
        &mut self,
        ctx: &mut SaveStateContext,
        data: &mut impl Read,
    ) -> Result<(), LoadStateError> {
        let mut magic = [0u8; 4];
        magic.load_state(ctx, data)?;

        let mut version = 0u32;
        version.load_state(ctx, data)?;

        ctx.version = version;

        if ctx.version > 1 {
            let mut time = 0;
            time.load_state(ctx, data)?;
            ctx.time = (time != u64::max_value()).then_some(time);
        } else {
            ctx.time = None;
        }

        if magic != Self::MAGIC_CONST {
            return Err(LoadStateError::InvalidMagicConst(magic));
        }

        if ctx.version > Self::SAVE_STATE_VERSION {
            return Err(LoadStateError::UnknownVersion(ctx.version));
        }

        Ok(())
    }
}

pub trait SaveState {
    fn save_state(
        &self,
        _: &mut SaveStateContext,
        data: &mut impl Write,
    ) -> Result<(), std::io::Error>;
    fn load_state(
        &mut self,
        _: &mut SaveStateContext,
        data: &mut impl Read,
    ) -> Result<(), LoadStateError>;
}

impl SaveState for u8 {
    fn save_state(
        &self,
        _: &mut SaveStateContext,
        data: &mut impl Write,
    ) -> Result<(), std::io::Error> {
        data.write_all(&[*self])?;
        Ok(())
    }

    fn load_state(
        &mut self,
        _: &mut SaveStateContext,
        data: &mut impl Read,
    ) -> Result<(), LoadStateError> {
        data.read_exact(std::slice::from_mut(self))?;
        Ok(())
    }
}

#[macro_export]
macro_rules! save_state {
    // end
    (@accum ($n:ident, $s:ident, $ctx:ident, $d:ident,) -> ($($save:tt)*) -> ($($load:tt)*)) => {
        impl SaveState for $n {
            fn save_state(&$s, $ctx: &mut $crate::save_state::SaveStateContext, $d: &mut impl std::io::Write) -> Result<(), std::io::Error> {
                $($save)*
                let _ = $d;
                Ok(())
            }

            fn load_state(&mut $s, $ctx: &mut $crate::save_state::SaveStateContext, $d: &mut impl std::io::Read) -> Result<(), LoadStateError> {
                $($load)*
                let _ = $d;
                Ok(())
            }
        }
    };
    // const <expr>
    (@accum ($n:ident, $s:ident, $ctx:ident, $d:ident, const $e:expr; $($f:tt)* ) -> ($($save:tt)*) -> ($($load:tt)*)) => {
        $crate::save_state!(
            @accum ($n, $s, $ctx, $d, $($f)* )
            -> ($($save)* ($e).save_state($ctx, $d)?; )
            -> ($($load)* {
                let expected = $e;
                let mut loaded = expected;
                loaded.load_state($ctx, $d)?;
                if loaded != expected {
                    LoadStateError::ConstMismatch(format!("{:?}", loaded), format!("{:?}", expected));
                }
            })
        );
    };
    // bitset [<expr>*]
    (@accum ($n:ident, $s:ident, $ctx:ident, $d:ident, bitset [ $($e:expr),* ]; $($f:tt)* ) -> ($($save:tt)*) -> ($($load:tt)*)) => {
        $crate::save_state!(
            @accum ($n, $s, $ctx, $d, $($f)* )
            -> ($($save)* { use $crate::save_state::BoolExt; [ $( &   $e.get()),* ].save_state($ctx, $d)?; } )
            -> ($($load)* { use $crate::save_state::BoolExt; [ $( $e.get_mut()),* ].load_state($ctx, $d)?; } )
        );
    };
    // on_load <expr>
    (@accum ($n:ident, $s:ident, $ctx:ident, $d:ident, on_load $e:expr; $($f:tt)* ) -> ($($save:tt)*) -> ($($load:tt)*)) => {
        $crate::save_state!(
            @accum ($n, $s, $ctx, $d, $($f)* )
            -> ($($save)* )
            -> ($($load)* ($e); )
        );
    };
    // <expr>
    (@accum ($n:ident, $s:ident, $ctx:ident, $d:ident, $e:expr; $($f:tt)* ) -> ($($save:tt)*) -> ($($load:tt)*)) => {
        $crate::save_state!(
            @accum ($n, $s, $ctx, $d, $($f)* )
            -> ($($save)* ($e).save_state($ctx, $d)?; )
            -> ($($load)* ($e).load_state($ctx, $d)?; )
        );
    };
    // entry
    ($n:ident, $s:ident, $ctx:ident, $d:ident { $($f:tt)* }) => {
        $crate::save_state!(
            @accum ($n, $s, $ctx, $d, $($f)* )
            -> ()
            -> ()
        );
    };
    // entry
    ($n:ident, $s:ident, $d:ident { $($f:tt)* }) => {
        $crate::save_state!(
            @accum ($n, $s, _ctx, $d, $($f)* )
            -> ()
            -> ()
        );
    };
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

impl SaveState for u16 {
    fn save_state(
        &self,
        _: &mut SaveStateContext,
        data: &mut impl Write,
    ) -> Result<(), std::io::Error> {
        data.write_all(&self.to_be_bytes())?;
        Ok(())
    }

    fn load_state(
        &mut self,
        _: &mut SaveStateContext,
        data: &mut impl Read,
    ) -> Result<(), LoadStateError> {
        let mut bytes = [0; 2];
        data.read_exact(&mut bytes)?;
        *self = u16::from_be_bytes(bytes);
        Ok(())
    }
}

impl SaveState for u32 {
    fn save_state(
        &self,
        _: &mut SaveStateContext,
        data: &mut impl Write,
    ) -> Result<(), std::io::Error> {
        data.write_all(&self.to_be_bytes())?;
        Ok(())
    }

    fn load_state(
        &mut self,
        _: &mut SaveStateContext,
        data: &mut impl Read,
    ) -> Result<(), LoadStateError> {
        let mut bytes = [0; 4];
        data.read_exact(&mut bytes)?;
        *self = u32::from_be_bytes(bytes);
        Ok(())
    }
}

impl SaveState for u64 {
    fn save_state(
        &self,
        _: &mut SaveStateContext,
        data: &mut impl Write,
    ) -> Result<(), std::io::Error> {
        data.write_all(&self.to_be_bytes())?;
        Ok(())
    }

    fn load_state(
        &mut self,
        _: &mut SaveStateContext,
        data: &mut impl Read,
    ) -> Result<(), LoadStateError> {
        let mut bytes = [0; 8];
        data.read_exact(&mut bytes)?;
        *self = u64::from_be_bytes(bytes);
        Ok(())
    }
}

impl<T: SaveState, const N: usize> SaveState for [T; N] {
    fn save_state(
        &self,
        ctx: &mut SaveStateContext,
        data: &mut impl Write,
    ) -> Result<(), std::io::Error> {
        for x in self {
            x.save_state(ctx, data)?;
        }
        Ok(())
    }

    fn load_state(
        &mut self,
        ctx: &mut SaveStateContext,
        data: &mut impl Read,
    ) -> Result<(), LoadStateError> {
        for x in self {
            x.load_state(ctx, data)?;
        }
        Ok(())
    }
}

impl SaveState for Vec<u8> {
    fn save_state(
        &self,
        _: &mut SaveStateContext,
        data: &mut impl Write,
    ) -> Result<(), std::io::Error> {
        data.write_all(&(self.len() as u32).to_be_bytes())?;
        data.write_all(self)?;
        Ok(())
    }

    fn load_state(
        &mut self,
        _: &mut SaveStateContext,
        data: &mut impl Read,
    ) -> Result<(), LoadStateError> {
        let mut bytes = [0; 4];
        data.read_exact(&mut bytes)?;
        let len = u32::from_be_bytes(bytes);
        self.resize(len as usize, 0);
        data.read_exact(self)?;
        Ok(())
    }
}

impl<const N: usize> SaveState for [&bool; N] {
    fn save_state(
        &self,
        _: &mut SaveStateContext,
        data: &mut impl Write,
    ) -> Result<(), std::io::Error> {
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

    fn load_state(
        &mut self,
        _: &mut SaveStateContext,
        _data: &mut impl Read,
    ) -> Result<(), LoadStateError> {
        unimplemented!()
    }
}

impl<const N: usize> SaveState for [&mut bool; N] {
    fn save_state(
        &self,
        _: &mut SaveStateContext,
        _data: &mut impl Write,
    ) -> Result<(), std::io::Error> {
        unimplemented!()
    }

    fn load_state(
        &mut self,
        _: &mut SaveStateContext,
        data: &mut impl Read,
    ) -> Result<(), LoadStateError> {
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
    fn save_state(
        &self,
        ctx: &mut SaveStateContext,
        data: &mut impl Write,
    ) -> Result<(), std::io::Error> {
        self.get().save_state(ctx, data)?;
        Ok(())
    }

    fn load_state(
        &mut self,
        ctx: &mut SaveStateContext,
        data: &mut impl Read,
    ) -> Result<(), LoadStateError> {
        let mut x = T::default();
        x.load_state(ctx, data)?;
        self.set(x);
        Ok(())
    }
}
