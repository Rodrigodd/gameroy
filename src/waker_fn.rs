// Copied from https://github.com/smol-rs/waker-fn
//
// Permission is hereby granted, free of charge, to any
// person obtaining a copy of this software and associated
// documentation files (the "Software"), to deal in the
// Software without restriction, including without
// limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of
// the Software, and to permit persons to whom the Software
// is furnished to do so, subject to the following
// conditions:
//
// The above copyright notice and this permission notice
// shall be included in all copies or substantial portions
// of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
// ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
// TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
// SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

//! Convert closures into wakers.
//!
//! A [`Waker`] is just a fancy callback. This crate converts regular closures into wakers.

use std::{
    mem::{self, ManuallyDrop},
    sync::Arc,
    task::{RawWaker, RawWakerVTable, Waker},
};

#[cfg(not(target_arch = "wasm32"))]
pub fn waker_fn<F>(f: F) -> Waker
where
    F: Fn() + Sync + Send + 'static,
{
    // SAFETY: F is Sync and Send, so the waker can be called any time
    unsafe { waker(f) }
}

#[cfg(target_arch = "wasm32")]
pub fn waker_fn<F>(f: F) -> Waker
where
    F: Fn() + 'static,
{
    // SAFETY: Don't need Send nor Sync because wasm32 is singlethreaded (this may become false
    // when WebWorkers are introduced).
    unsafe { waker(f) }
}

unsafe fn waker<F>(f: F) -> Waker
where
    F: Fn() + 'static,
{
    let raw = Arc::into_raw(Arc::new(f)) as *const ();
    let vtable = &Helper::<F>::VTABLE;
    Waker::from_raw(RawWaker::new(raw, vtable))
}

struct Helper<F>(F);

impl<F: Fn() + 'static> Helper<F> {
    const VTABLE: RawWakerVTable = RawWakerVTable::new(
        Self::clone_waker,
        Self::wake,
        Self::wake_by_ref,
        Self::drop_waker,
    );

    unsafe fn clone_waker(ptr: *const ()) -> RawWaker {
        let arc = ManuallyDrop::new(Arc::from_raw(ptr as *const F));
        let _arc_clone: mem::ManuallyDrop<_> = arc.clone();
        RawWaker::new(ptr, &Self::VTABLE)
    }

    unsafe fn wake(ptr: *const ()) {
        let arc = Arc::from_raw(ptr as *const F);
        (arc)();
    }

    unsafe fn wake_by_ref(ptr: *const ()) {
        let arc = ManuallyDrop::new(Arc::from_raw(ptr as *const F));
        (arc)();
    }

    unsafe fn drop_waker(ptr: *const ()) {
        drop(Arc::from_raw(ptr as *const F));
    }
}
