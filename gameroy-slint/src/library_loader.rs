//! Background loading for the ROM list.
//!
//! Listing a folder is cheap, but every row then wants a cartridge header off
//! disk and a thumbnail off the network. Doing that inline is what made the old
//! list stall; doing it here keeps the table on screen while it fills in.
//!
//! The wake-up handshake is deliberately the same as [`crate::server_handle`]'s:
//! push into the queue, and only post to the event loop on the 0 -> 1 edge of a
//! flag the drain clears first. That matters more here than there -- the old
//! front-end rebuilt the whole table once per entry, which for a large folder is
//! hundreds of full refreshes.

use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::time::SystemTime;

use crate::library::{self, Header};
use crate::thumbs;

/// Workers reading headers and fetching thumbnails.
///
/// Eight, not the hundred the old front-end span: a header read is 336 bytes, and
/// only the downloads benefit from width. A hundred simultaneous connections to
/// someone else's thumbnail server is also not neighbourly.
const WORKERS: usize = 8;

/// Something a worker found out about one entry.
pub enum LibraryMsg {
    Header {
        index: usize,
        header: Option<Header>,
        last_played: Option<SystemTime>,
    },
    /// Raw pixels rather than a `slint::Image`, which is not `Send` and can only
    /// be built on the UI thread.
    Thumb {
        index: usize,
        width: u32,
        height: u32,
        rgba: Vec<u8>,
    },
}

/// A scan in progress.
///
/// Dropping it tells the workers to stop. They are detached rather than joined:
/// a thread blocked on a slow download would otherwise stall the folder change
/// that made it irrelevant.
pub struct LibraryLoader {
    queue: Arc<Mutex<Vec<LibraryMsg>>>,
    pending: Arc<AtomicBool>,
    cancel: Arc<AtomicBool>,
}

impl LibraryLoader {
    pub fn spawn(paths: Vec<PathBuf>, wake: impl Fn() + Send + Sync + 'static) -> Self {
        let queue = Arc::new(Mutex::new(Vec::new()));
        let pending = Arc::new(AtomicBool::new(false));
        let cancel = Arc::new(AtomicBool::new(false));

        let next = Arc::new(AtomicUsize::new(0));
        let paths = Arc::new(paths);
        let wake = Arc::new(wake);

        for _ in 0..WORKERS.min(paths.len()) {
            let (queue, pending, cancel) = (queue.clone(), pending.clone(), cancel.clone());
            let (next, paths, wake) = (next.clone(), paths.clone(), wake.clone());
            std::thread::Builder::new()
                .name("gameroy-library".into())
                .spawn(move || {
                    loop {
                        if cancel.load(Ordering::Relaxed) {
                            return;
                        }
                        let index = next.fetch_add(1, Ordering::Relaxed);
                        let Some(path) = paths.get(index) else { return };

                        let post = |msg| {
                            queue.lock().unwrap().push(msg);
                            // Only the thread that flips the flag posts, so a
                            // burst costs one closure rather than one each.
                            if !pending.swap(true, Ordering::AcqRel) {
                                wake();
                            }
                        };

                        post(LibraryMsg::Header {
                            index,
                            header: library::read_header(path),
                            last_played: library::last_played_of(path),
                        });

                        // The header is what the table needs to be usable, so it
                        // goes out before the slower thumbnail is attempted.
                        if cancel.load(Ordering::Relaxed) {
                            return;
                        }
                        let Some(stem) = thumbs::stem_of(path) else {
                            continue;
                        };
                        match thumbs::fetch(&stem).and_then(|png| thumbs::decode_rgba(&png)) {
                            Ok((width, height, rgba)) => post(LibraryMsg::Thumb {
                                index,
                                width,
                                height,
                                rgba,
                            }),
                            // A missing thumbnail is the normal case for a ROM
                            // libretro has never heard of, not a failure.
                            Err(e) => log::debug!("no thumbnail for {stem:?}: {e}"),
                        }
                    }
                })
                .expect("spawning a library worker");
        }

        Self {
            queue,
            pending,
            cancel,
        }
    }

    /// Clears the wake-up flag before the queue is read.
    ///
    /// This order is the whole trick: a message pushed between the drain and the
    /// clear would otherwise see the flag still set, skip the wake, and sit in
    /// the queue until something else happened to post.
    pub fn begin_drain(&self) {
        self.pending.store(false, Ordering::Release);
    }

    pub fn drain(&self) -> Vec<LibraryMsg> {
        std::mem::take(&mut *self.queue.lock().unwrap())
    }
}

impl Drop for LibraryLoader {
    fn drop(&mut self) {
        self.cancel.store(true, Ordering::Relaxed);
    }
}
