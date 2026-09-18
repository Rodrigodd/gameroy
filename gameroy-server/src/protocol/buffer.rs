use serde::{Deserialize, Serialize};

/// One frame's pixels: `SCREEN_PIXELS` bytes, one per pixel, each a 2-bit Game Boy
/// shade index (0-3) — *not* RGB. Applying a palette is a front-end decision, which
/// is also why this is a quarter the size of an RGBA buffer on the wire.
///
/// A `Vec` rather than a boxed array because the allocation is recyclable: the
/// server keeps a small pool and the client hands buffers back with
/// [`super::Command::ReturnFrame`]. Over a serialized transport the client simply
/// never returns one and the pool allocates, which is correct and needs no
/// special-casing in the server.
#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FrameBuffer(#[serde(with = "serde_bytes")] pub Vec<u8>);

impl std::fmt::Debug for FrameBuffer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // 23040 bytes of pixels is never what someone wants to read in a log.
        write!(f, "FrameBuffer({} bytes)", self.0.len())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FramePacket {
    /// Strictly monotonic message counter. Use this for drop detection, *not*
    /// `frame`: while rewinding, `frame` and `clock_count` both run backwards.
    pub seq: u64,
    /// The emulated frame number. Decreases while rewinding.
    pub frame: u32,
    pub clock_count: u64,
    pub width: u16,
    pub height: u16,
    pub pixels: FrameBuffer,
}

/// A run of synthesized audio.
///
/// Produced on the server, because the samples come straight out of the emulated
/// sound controller as the clock advances; played on the client, which owns the
/// audio device. That split is what keeps `cpal` out of the server.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AudioChunk {
    /// Interleaved stereo at [`AudioChunk::sample_rate`].
    pub samples: Vec<i16>,
    /// Clock of the first sample, so a client can detect a gap.
    pub clock_count: u64,
    pub sample_rate: u32,
}
