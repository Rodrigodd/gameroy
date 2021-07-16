use std::collections::HashMap;
use std::num::NonZeroUsize;
use std::sync::{Arc, Mutex, Weak};

use crui::Context;

#[derive(PartialEq, Eq, Hash)]
pub enum Event {
    Debug,
    FrameUpdated,
}

/// A handle to a registered event callback. When this is dropped, the callback is unregistered.
pub struct Handle {
    /// The value of the pointer that the callback have.
    event_id: NonZeroUsize,
    event_table: Weak<Mutex<Vec<Box<dyn FnMut(&mut Context)>>>>,
}
impl Drop for Handle {
    fn drop(&mut self) {
        let event_table = match self.event_table.upgrade() {
            Some(x) => x,
            None => return,
        };
        let mut event_table = event_table.lock().unwrap();
        if let Some(i) = event_table.iter().position(|x| {
            (&**x as *const dyn FnMut(&mut Context) as *const () as usize) == self.event_id.get()
        }) {
            drop(event_table.remove(i));
        }
    }
}
/// Dummy implementation, only for allowing a control hold up this handle, unregistering the
/// callback when the control is dropped.
impl crui::Behaviour for Handle {}

/// A implementation of the observer pattern.
#[derive(Default)]
pub struct EventTable {
    listeners: HashMap<Event, Arc<Mutex<Vec<Box<dyn FnMut(&mut Context)>>>>>,
}
impl EventTable {
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a callback to a event. Returns a Handle that, when dropped, will unregister this
    /// callback.
    pub fn register<F: Fn(&mut Context) + 'static>(&mut self, event: Event, callback: F) -> Handle {
        let callback: Box<dyn FnMut(&mut Context)> = Box::new(callback);
        let event_id: *const dyn FnMut(&mut Context) = &*callback;
        let event_id = NonZeroUsize::new(event_id as *const () as usize).unwrap();
        let listeners = self.listeners.entry(event).or_default();

        listeners.lock().unwrap().push(callback);

        Handle {
            event_id,
            event_table: Arc::downgrade(listeners),
        }
    }

    pub fn notify(&mut self, event: Event, ctx: &mut Context) {
        if let Some(listeners) = self.listeners.get_mut(&event) {
            for listener in &mut *listeners.lock().unwrap() {
                (listener)(ctx);
            }
        }
    }
}
