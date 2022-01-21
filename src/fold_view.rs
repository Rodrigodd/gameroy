use crui::Behaviour;

pub struct FoldView {
    pub fold: bool,
}
impl FoldView {
    fn active_children(&mut self, ctx: &mut crui::Context, this: crui::Id) {
        for c in ctx
            .get_all_children(this)
            .iter()
            .skip(1)
            .cloned()
            .collect::<Vec<_>>()
        {
            if self.fold {
                ctx.deactive(c);
            } else {
                ctx.active(c);
            }
        }
    }
}
impl Behaviour for FoldView {
    fn on_active(&mut self, this: crui::Id, ctx: &mut crui::Context) {
        self.active_children(ctx, this);
    }

    fn input_flags(&self) -> crui::InputFlags {
        crui::InputFlags::MOUSE
    }

    fn on_mouse_event(&mut self, mouse: crui::MouseInfo, this: crui::Id, ctx: &mut crui::Context) {
        match mouse.event {
            crui::MouseEvent::Enter => (),
            crui::MouseEvent::Exit => (),
            crui::MouseEvent::Down(_) => (),
            crui::MouseEvent::Up(_) => {
                self.fold = !self.fold;
                self.active_children(ctx, this);
            }
            crui::MouseEvent::Moved => (),
            crui::MouseEvent::None => (),
        }
    }
}
