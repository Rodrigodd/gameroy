use std::{collections::HashMap, future::Future, pin::Pin, sync::atomic::AtomicU32};

use giui::Context;
use winit::event_loop::EventLoopProxy;

use crate::UserEvent;

fn next_id() -> u32 {
    static COUNTER: AtomicU32 = AtomicU32::new(0);
    COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

pub struct Executor {
    pub map: HashMap<u32, Pin<Box<dyn Future<Output = ()>>>>,
    proxy: EventLoopProxy<UserEvent>,
}
impl Executor {
    pub fn new(proxy: EventLoopProxy<UserEvent>) -> Self {
        Self {
            map: HashMap::default(),
            proxy,
        }
    }
    pub fn spawn_task(task: impl Future<Output = ()> + 'static, ctx: &mut Context) {
        let this = ctx.get_mut::<Executor>();
        let task_id = next_id();
        this.map.insert(task_id, Box::pin(task));
        this.proxy
            .send_event(UserEvent::SpawnTask(task_id))
            .unwrap();
    }
}

/// This works reasonably as long the given Future do not do any Async operation. Not very
/// useful.
pub fn block_on<T>(mut task: Pin<&mut impl Future<Output = T>>) -> T {
    let waker = crate::waker_fn::waker_fn(move || ());
    let mut cx = std::task::Context::from_waker(&waker);

    loop {
        match Future::poll(task.as_mut(), &mut cx) {
            std::task::Poll::Ready(x) => {
                return x;
            }
            std::task::Poll::Pending => {}
        }
    }
}
