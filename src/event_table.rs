use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::num::NonZeroUsize;
use std::sync::{Arc, Mutex, Weak};

use crui::Context;

pub trait Event: 'static {
    type Payload: Clone;
}

pub struct Debug;
impl Event for Debug {
    type Payload = bool;
}

pub struct FrameUpdated;
impl Event for FrameUpdated {
    type Payload = ();
}

type CallbackDyn<E> = dyn FnMut(<E as Event>::Payload, &mut Context);

/// A handle to a registered event callback. When this is dropped, the callback is unregistered.
pub struct Handle<E: Event> {
    /// The value of the pointer that the callback have.
    event_id: NonZeroUsize,
    event_table: Weak<Mutex<Vec<Box<CallbackDyn<E>>>>>,
}
impl<E: Event> Drop for Handle<E> {
    fn drop(&mut self) {
        let event_table = match self.event_table.upgrade() {
            Some(x) => x,
            None => return,
        };
        let mut event_table = event_table.lock().unwrap();
        if let Some(i) = event_table.iter().position(|x| {
            (&**x as &CallbackDyn<E> as *const _ as *const () as usize) == self.event_id.get()
        }) {
            drop(event_table.remove(i));
        }
    }
}
/// Dummy implementation, only for allowing a control hold up this handle, unregistering the
/// callback when the control is dropped.
impl<E: Event> crui::Behaviour for Handle<E> {}

#[derive(Default)]
struct List<E: Event> {
    listeners: Arc<Mutex<Vec<Box<CallbackDyn<E>>>>>,
}
impl<E: Event> List<E> {
    fn register(&mut self, callback: impl FnMut(E::Payload, &mut Context) + 'static) -> Handle<E> {
        let callback: Box<CallbackDyn<E>> = Box::new(callback);
        let event_id: *const CallbackDyn<E> = &*callback;
        let event_id = NonZeroUsize::new(event_id as *const () as usize).unwrap();

        self.listeners.lock().unwrap().push(callback);

        Handle {
            event_id,
            event_table: Arc::downgrade(&self.listeners),
        }
    }

    fn notify(&mut self, payload: E::Payload, ctx: &mut Context) {
        for listener in &mut *self.listeners.lock().unwrap() {
            (listener)(payload.clone(), ctx);
        }
    }
}

/// A implementation of the observer pattern.
#[derive(Default)]
pub struct EventTable {
    listeners: HashMap<TypeId, Box<dyn Any>>,
}
impl EventTable {
    pub fn new() -> Self {
        Self::default()
    }

    fn get_list<E: Event>(&mut self) -> &mut List<E> {
        let list = self.listeners.entry(TypeId::of::<E>()).or_insert_with(|| {
            Box::new(List::<E> {
                listeners: Default::default(),
            })
        });
        list.downcast_mut::<List<E>>().unwrap()
    }

    /// Register a callback to a event. Returns a Handle that, when dropped, will unregister this
    /// callback.
    pub fn register<E: Event, F: FnMut(E::Payload, &mut Context) + 'static>(
        &mut self,
        callback: F,
    ) -> Handle<E> {
        let list = self.get_list::<E>();
        list.register(callback)
    }

    pub fn notify<E: Event>(&mut self, payload: E::Payload, ctx: &mut Context) {
        let list = self.get_list::<E>();
        list.notify(payload, ctx);
    }
}
