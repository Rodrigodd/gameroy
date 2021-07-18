use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::num::NonZeroUsize;
use std::cell::RefCell;
use std::rc::{Rc, Weak};

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

pub struct EmulatorUpdated;
impl Event for EmulatorUpdated {
    type Payload = ();
}

// type dyn Callback<E> = dyn FnMut(<E as Event>::Payload, &mut Context) + 'static;
pub trait Callback<E: Event>: FnMut(<E as Event>::Payload, &mut Context) + 'static {}
impl<E:Event, F: FnMut(<E as Event>::Payload, &mut Context) + 'static> Callback<E> for F {}

/// A handle to a registered event callback. When this is dropped, the callback is unregistered.
pub struct Handle<E: Event> {
    /// The value of the pointer that the callback have.
    event_id: NonZeroUsize,
    event_table: Weak<List<E>>,
}
impl<E: Event> Drop for Handle<E> {
    fn drop(&mut self) {
        let event_table = match self.event_table.upgrade() {
            Some(x) => x,
            None => return,
        };
        println!(">> unregister!!");
        event_table.unregister(self.event_id.get());
    }

}
/// Dummy implementation, only for allowing a control hold up this handle, unregistering the
/// callback when the control is dropped.
impl<E: Event> crui::Behaviour for Handle<E> {}

#[derive(Default)]
struct List<E: Event> {
    listeners: RefCell<Vec<Box<dyn Callback<E>>>>,
}
impl<E: Event> List<E> {
    fn register(&self, callback: impl Callback<E>) -> NonZeroUsize {
        let callback: Box<dyn Callback<E>> = Box::new(callback);
        let event_id: *const dyn Callback<E> = &*callback;
        let event_id = NonZeroUsize::new(event_id as *const () as usize).unwrap();

        self.listeners.borrow_mut().push(callback);

        event_id
    }

    fn unregister(&self, event_id: usize) {
        let mut event_table = self.listeners.borrow_mut();
        if let Some(i) = event_table.iter().position(|x| {
            (&**x as &dyn Callback<E> as *const _ as *const () as usize) == event_id
        }) {
            drop(event_table.remove(i));
        }
    }

    fn notify(&self, payload: E::Payload, ctx: &mut Context) {
        for listener in &mut *self.listeners.borrow_mut() {
            (listener)(payload.clone(), ctx);
        }
    }
}

/// A implementation of the observer pattern.
#[derive(Default)]
pub struct EventTable {
    listeners: HashMap<TypeId, Rc<dyn Any>>,
}
impl EventTable {
    pub fn new() -> Self {
        Self::default()
    }

    fn get_list<E: Event>(&mut self) -> Rc<List<E>> {
        let list = self.listeners.entry(TypeId::of::<E>()).or_insert_with(|| {
            Rc::new(List::<E> {
                listeners: Default::default(),
            })
        });
        list.clone().downcast::<List<E>>().unwrap()
    }

    /// Register a callback to a event. Returns a Handle that, when dropped, will unregister this
    /// callback.
    pub fn register<E: Event, F: Callback<E>>(
        &mut self,
        callback: F,
    ) -> Handle<E> {
        let list = self.get_list::<E>();
        let event_id = list.register(callback);


        Handle {
            event_id,
            event_table: Rc::downgrade(&list),
        }
    }

    pub fn notify<E: Event>(&mut self, payload: E::Payload, ctx: &mut Context) {
        let list = self.get_list::<E>();
        list.notify(payload, ctx);
    }
}
