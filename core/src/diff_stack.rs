use std::io::Write;

/// A Stack where in each push, the previous top element is delta-compressed against the new
/// element. Is used the build a save state stack.
///
/// Each element in the buffer is a list of bytes, followed by a 32-bit length.
pub struct DiffStack {
    buffer: Box<[u8]>,
    top: usize,
    count: usize,
}

impl DiffStack {
    pub fn new(capacity: usize) -> Self {
        Self {
            buffer: vec![0; capacity].into_boxed_slice(),
            top: 0,
            count: 0,
        }
    }

    pub fn capacity(&self) -> usize {
        self.buffer.len()
    }

    /// The number of elements stored.
    pub fn count(&self) -> usize {
        self.count
    }

    pub fn clear(&mut self) {
        self.top = 0;
        self.count = 0;
    }

    pub fn is_empty(&self) -> bool {
        self.top == 0
    }

    pub fn top(&self) -> Option<&[u8]> {
        if self.is_empty() {
            return None;
        }
        let top_len = {
            let len = &self.buffer[self.top - 4..self.top];
            u32::from_le_bytes(len.try_into().unwrap()) as usize
        };

        let top_elem_pos = self.top - 4 - top_len;
        let top = &self.buffer[top_elem_pos..self.top - 4];
        Some(top)
    }

    pub fn push(&mut self, new_elem: &[u8]) -> bool {
        if !self.is_empty() {
            let free_pos = self.top;
            let mut top = self.top;
            let (buffer, free) = self.buffer.split_at_mut(free_pos);

            let top_len = {
                let len = &buffer[self.top - 4..self.top];
                u32::from_le_bytes(len.try_into().unwrap()) as usize
            };

            top -= 4;

            let top_elem_pos = top - top_len;
            let top_elem = &buffer[top_elem_pos..top];

            top -= top_len;

            let diff_len = match delta_compress(new_elem, top_elem, &mut &mut free[..]) {
                Ok(len) => len,
                Err(_) => return false,
            };

            // Check if there is run for storing the new_elem, and a extra space for decompressing
            // the previous element on pop.
            if top + diff_len + 4 + new_elem.len() + 4 + top_len > self.buffer.len() {
                return false;
            }

            self.top = top;

            self.buffer
                .copy_within(free_pos..free_pos + diff_len, self.top);

            self.top += diff_len;
            self.buffer[self.top..][..4].copy_from_slice(&(diff_len as u32).to_le_bytes());
            self.top += 4;
        } else {
            // Check if there is run for storing the new_elem.
            if self.top + new_elem.len() + 4 > self.buffer.len() {
                return false;
            }
        }

        self.buffer[self.top..][..new_elem.len()].copy_from_slice(new_elem);

        self.top += new_elem.len();
        self.buffer[self.top..][..4].copy_from_slice(&(new_elem.len() as u32).to_le_bytes());
        self.top += 4;

        self.count += 1;
        true
    }

    pub fn pop(&mut self) -> bool {
        let free_pos = self.top;
        let (buffer, free) = self.buffer.split_at_mut(free_pos);

        let top_len = {
            let len = &buffer[self.top - 4..self.top];
            u32::from_le_bytes(len.try_into().unwrap()) as usize
        };

        self.top -= 4;

        let top_elem_pos = self.top - top_len;
        let top_elem = &buffer[top_elem_pos..self.top];

        self.top -= top_len;

        if self.top > 0 {
            let prev_len = {
                let len = &buffer[self.top - 4..self.top];
                u32::from_le_bytes(len.try_into().unwrap()) as usize
            };

            self.top -= 4;

            let prev_elem_pos = self.top - prev_len;
            let prev_elem = &buffer[prev_elem_pos..self.top];

            self.top -= prev_len;

            // Can unwrap here, because the compressed data should be valid, and there should be
            // enough free space for the decompression. Both are garanted on push.
            let undiff_len = delta_decompress(top_elem, prev_elem, &mut &mut free[..]).unwrap();

            buffer[self.top..][..undiff_len].copy_from_slice(&free[..undiff_len]);
            self.top += undiff_len;
            self.buffer[self.top..][..4].copy_from_slice(&(undiff_len as u32).to_le_bytes());
            self.top += 4;
        }

        self.count -= 1;
        true
    }
}

// The compression works by writing the out the sequence:
//
// (equal_bytes_count:u16 | different_bytes_count:u16 | different_bytes:u8*)*
pub fn delta_compress(a: &[u8], b: &[u8], out: &mut impl Write) -> std::io::Result<usize> {
    let mut i = 0;
    let mut out_len = 0;

    while i < b.len() {
        let mut eq_count: u16 = 0;
        while i < b.len() && eq_count < 0xFFFF && i < a.len() && a[i] == b[i] {
            eq_count += 1;
            i += 1;
        }

        let mut diff_count: u16 = 0;
        if eq_count < 0xFFFF {
            while i < b.len() && diff_count < 0xFFFF && (i >= a.len() || a[i] != b[i]) {
                diff_count += 1;
                i += 1;
            }
        }

        out.write_all(&eq_count.to_le_bytes())?;
        out.write_all(&diff_count.to_le_bytes())?;
        out.write_all(&b[i - diff_count as usize..i])?;

        out_len += 2 + 2 + diff_count as usize;
    }

    Ok(out_len)
}

/// Decompress data compressed by [`delta_compress`].
pub fn delta_decompress(
    a: &[u8],
    diff: &[u8],
    out: &mut impl Write,
) -> Result<usize, DecompressError> {
    let mut i = 0;
    let mut j = 0;
    let mut out_len = 0;

    while j < diff.len() {
        if diff[j..].len() < 4 {
            return Err(DecompressError::MalformedDiff);
        }

        let eq_count = u16::from_le_bytes(diff[j..][..2].try_into().unwrap());
        j += 2;
        let diff_count = u16::from_le_bytes(diff[j..][..2].try_into().unwrap());
        j += 2;

        if eq_count > 0 {
            if i + eq_count as usize > a.len() {
                return Err(DecompressError::InputTooSmall);
            }
            out.write_all(&a[i..][..eq_count as usize])?;
            i += eq_count as usize;
        }

        if diff[j..].len() < diff_count as usize {
            return Err(DecompressError::MalformedDiff);
        }

        out.write_all(&diff[j..][..diff_count as usize])?;

        i += diff_count as usize;
        j += diff_count as usize;

        out_len += eq_count as usize + diff_count as usize;
    }
    Ok(out_len)
}

#[derive(Debug)]
pub enum DecompressError {
    /// While reading the `equal_bytes` from the input data, the input ended early.
    InputTooSmall,
    /// While reading the next `equal_bytes_count`/`different_bytes_count` pair, or the
    /// `different_bytes` from the diff, the diff ended early.
    MalformedDiff,
    Io(std::io::Error),
}

impl From<std::io::Error> for DecompressError {
    fn from(v: std::io::Error) -> Self {
        Self::Io(v)
    }
}

#[cfg(test)]
mod test {
    use rand::prelude::*;

    use crate::diff_stack::{DecompressError, DiffStack};

    /// Compress and decompress random data, and check if it is the same.
    #[test]
    fn fuzz_reverse() {
        let mut b_ = Vec::new();
        let mut diff = Vec::new();
        let mut thread_rng = rand::thread_rng();

        let start = std::time::Instant::now();
        while start.elapsed().as_secs() < 20 {
            let seed: u64 = thread_rng.gen();
            println!("seed 0x{:016x}", seed);
            let mut rng = rand::rngs::StdRng::seed_from_u64(seed);
            let a = {
                let len = rng.gen_range(0.0f64..20.0).exp2() as usize;
                let mut buf = vec![0; len];
                rng.fill_bytes(&mut buf);
                buf
            };

            let b = {
                let len = rng.gen_range(0.0f64..20.0).exp2() as usize;
                let mut buf = vec![0; len];
                rng.fill_bytes(&mut buf);
                buf
            };

            diff.clear();

            let Ok(len_diff) = super::delta_compress(&a, &b, &mut diff) else {
                panic!("could not write to a Vec!?");
            };

            assert_eq!(diff.len(), len_diff);

            // print_diff(&diff);

            b_.clear();

            let len_out = match super::delta_decompress(&a, &diff, &mut b_) {
                Ok(len_out) => len_out,
                Err(DecompressError::InputTooSmall) => panic!("input too small!?"),
                Err(DecompressError::MalformedDiff) => panic!("malformed diff!?"),
                Err(DecompressError::Io(_)) => panic!("could not write to a Vec!?"),
            };

            assert_eq!(len_out, b_.len());

            assert_eq!(b, b_);
        }
    }

    /// Decompress random data with random diff, and check if it doesn't panic.
    #[test]
    fn fuzz_random_diff() {
        let mut b_ = Vec::new();
        let mut thread_rng = rand::thread_rng();

        let start = std::time::Instant::now();
        while start.elapsed().as_secs() < 20 {
            let seed: u64 = thread_rng.gen();
            println!("seed 0x{:016x}", seed);
            let mut rng = rand::rngs::StdRng::seed_from_u64(seed);
            let a = {
                let len = rng.gen_range(0.0f64..20.0).exp2() as usize;
                let mut buf = vec![0; len];
                rng.fill_bytes(&mut buf);
                buf
            };

            let diff = {
                let len = rng.gen_range(0.0f64..16.0).exp2() as usize;
                let mut buf = vec![0; len];
                rng.fill_bytes(&mut buf);
                buf
            };

            b_.clear();

            match super::delta_decompress(&a, &diff, &mut b_) {
                Ok(len_out) => assert_eq!(len_out, b_.len()),
                Err(super::DecompressError::Io(_)) => panic!("could not write to a Vec!?"),
                _ => {}
            }
        }
    }

    #[test]
    fn fuzz_push_pop() {
        let mut buf = vec![0; 0x40_0000];
        let mut d = Vec::new();
        let mut pos = 0;
        let mut thread_rng = rand::thread_rng();

        let mut diff_stack = DiffStack::new(0x1_0000);

        let start = std::time::Instant::now();
        let mut tries = 0;
        let mut overflow = 0;
        while start.elapsed().as_secs() < 20 {
            let seed: u64 = thread_rng.gen();
            let mut rng = rand::rngs::StdRng::seed_from_u64(seed);

            let mut count = rng.gen_range(0.0f64..5.0).exp2() as usize;

            println!("seed 0x{:016x}", seed);

            diff_stack.clear();

            tries += 1;

            for i in 0..count {
                let a = {
                    let len = rng.gen_range(0.0f64..16.1).exp2() as usize;
                    if pos + len > buf.len() {
                        count = i;
                        break;
                    }
                    rng.fill_bytes(&mut buf[pos..pos + len]);
                    let a = &buf[pos..pos + len];
                    d.push(pos);
                    pos += len;
                    a
                };

                if !diff_stack.push(a) {
                    pos = d.pop().unwrap();
                    overflow += 1;
                    count = i;
                    break;
                }
            }

            for _ in (0..count).rev() {
                let a = {
                    let e = pos;
                    let s = d.pop().unwrap();
                    pos = s;
                    &buf[s..e]
                };

                assert_eq!(a, diff_stack.top().unwrap());
                assert!(diff_stack.pop());
            }
        }

        println!("overflows: {}%", overflow as f64 / tries as f64 * 100.0);
    }
}
