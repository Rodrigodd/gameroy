use std::{
    any::{Any, TypeId},
    cell::RefCell,
    collections::HashMap,
    marker::PhantomData,
    rc::{Rc, Weak},
};

use giui::{Context, Id};

pub trait Event: Clone + 'static {}

#[derive(Clone)]
pub struct Debug(pub bool);
impl Event for Debug {}

#[derive(Clone, Copy)]
pub struct FrameUpdated;
impl Event for FrameUpdated {}

#[derive(Clone, Copy)]
pub struct EmulatorUpdated;
impl Event for EmulatorUpdated {}

#[derive(Clone, Copy)]
pub struct BreakpointsUpdated;
impl Event for BreakpointsUpdated {}

#[derive(Clone, Copy)]
pub struct WatchsUpdated;
impl Event for WatchsUpdated {}

pub struct UpdatedRomList;

/// A handle to a registered event callback. When this is dropped, the callback is unregistered.
pub struct Handle<E: Event> {
    /// The value of the pointer that the callback have.
    id: Id,
    event_table: Weak<List<E>>,
}
impl<E: Event> Drop for Handle<E> {
    fn drop(&mut self) {
        let event_table = match self.event_table.upgrade() {
            Some(x) => x,
            None => return,
        };
        log::warn!(">> unregister!!");
        event_table.unregister(self.id);
    }
}
/// Dummy implementation, only for allowing a control hold up this handle, unregistering the
/// callback when the control is dropped.
impl<E: Event> giui::Behaviour for Handle<E> {}

#[derive(Default)]
struct List<E: Event> {
    listeners: RefCell<Vec<Id>>,
    _event: PhantomData<*const E>,
}
impl<E: Event> List<E> {
    fn register(&self, id: Id) {
        self.listeners.borrow_mut().push(id);
    }

    fn unregister(&self, id: Id) {
        let mut event_table = self.listeners.borrow_mut();
        if let Some(i) = event_table.iter().position(|x| *x == id) {
            drop(event_table.remove(i));
        }
    }

    fn notify(&self, event: E, ctx: &mut Context) {
        for listener in &mut *self.listeners.borrow_mut() {
            ctx.send_event_to(*listener, event.clone())
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
                _event: PhantomData::default(),
            })
        });
        list.clone().downcast::<List<E>>().unwrap()
    }

    /// Register a control to a event. Returns a Handle that, when dropped, will unregister this
    /// callback.
    pub fn register<E: Event>(&mut self, id: Id) -> Handle<E> {
        let list = self.get_list::<E>();
        list.register(id);

        Handle {
            id,
            event_table: Rc::downgrade(&list),
        }
    }

    pub fn notify<E: Event>(&mut self, event: E, ctx: &mut Context) {
        let list = self.get_list::<E>();
        list.notify(event, ctx);
    }
}
