//! The disassembly list's model.
//!
//! The trace runs to tens of thousands of instructions, so the client holds
//! windows of rendered rows and tells Slint the list is as long as the whole
//! trace. `ListView` only asks for the rows it is about to draw, so the ones
//! outside a window are never requested unless the user scrolls to them -- at
//! which point [`window_needed`] says to go and fetch them.
//!
//! There are two windows, because there are two things asking for rows. The
//! server pushes a PC-centred window on every pause, and the user scrolling
//! pulls chunks from wherever they are looking. Holding only one meant a step
//! threw away whatever had been scrolled to.

use std::cell::RefCell;
use std::ops::Range;

use gameroy_server::protocol::DisassemblyWindow;
use slint::{Model, ModelNotify, ModelTracker};

use crate::DisasmRow;

/// Rows per request. Four to ten screens' worth, so scrolling costs roughly one
/// request per several screens rather than one per row.
pub const CHUNK: u16 = 256;

/// Refetch once the view comes this close to an edge of the cache, rather than
/// waiting until it falls off and shows placeholders.
const MARGIN: u32 = 32;

/// Chunk requests are aligned to this, so scrolling back and forth over a
/// boundary asks for the same window twice instead of two overlapping ones.
const ALIGN: u32 = 128;

/// The range the cache holds once the server has answered a request for `start`.
///
/// The server keeps the window `CHUNK` long by pulling its start back when the
/// request runs off the end of the trace, so what comes back is not always what
/// was asked for.
fn served(start: u32, total: u32) -> Range<u32> {
    let start = start.min(total.saturating_sub(CHUNK as u32));
    start..(start + CHUNK as u32).min(total)
}

/// Whether `cached` holds a view of `count` rows at `first` with room to spare.
fn covers(first: u32, count: u32, cached: &Range<u32>, total: u32) -> bool {
    let last = (first + count).min(total);
    let inside = !cached.is_empty() && first >= cached.start && last <= cached.end;
    // A margin is only worth having when there is somewhere for it to be: at
    // either end of the trace, being flush against the edge is as good as it gets.
    let room_above = cached.start == 0 || first >= cached.start.saturating_add(MARGIN);
    let room_below = cached.end >= total || last.saturating_add(MARGIN) <= cached.end;
    inside && room_above && room_below
}

/// Where to put a `CHUNK`-long window so a view of `count` rows at `first` sits
/// inside it with room to spare.
///
/// The answer has to satisfy [`covers`], or the very next scroll report asks for
/// the same chunk all over again -- once per mouse-move frame, for ever.
fn chunk_start(first: u32, count: u32, total: u32) -> u32 {
    // The furthest a chunk can start and still leave the view clear of its top.
    // Past `total - CHUNK` the server pulls the start back anyway, so asking for
    // more than that gains nothing.
    let highest = first
        .saturating_sub(MARGIN)
        .min(total.saturating_sub(CHUNK as u32));
    // Aligning down is a preference, not a rule: it can leave the view inside
    // the chunk's bottom margin, and an unsettled chunk is worse than an
    // unaligned one.
    let aligned = highest / ALIGN * ALIGN;
    if covers(first, count, &served(aligned, total), total) {
        aligned
    } else {
        highest
    }
}

/// The chunk to fetch for a view of `count` rows at `first`, or `None` when the
/// cache already covers it with room to spare.
pub fn window_needed(first: u32, count: u32, cached: Range<u32>, total: u32) -> Option<(u32, u16)> {
    if total == 0 || covers(first, count, &cached, total) {
        return None;
    }
    Some((chunk_start(first, count, total), CHUNK))
}

/// Whether the view should follow the PC, which it does exactly while the PC's
/// row is on screen.
///
/// Derived at the moment it is needed rather than kept as a flag. Slint runs
/// `changed` handlers from the event loop rather than at the assignment, so a
/// scroll report can land after a step has already decided to follow, and a
/// flag would let it overrule that.
pub fn follows_pc(visible: &Range<u32>, pc_index: Option<u32>) -> bool {
    pc_index.is_some_and(|pc| visible.contains(&pc))
}

/// Whether `a` describes a later trace than `b`.
///
/// Generations wrap -- the server counts with `wrapping_add` -- so this is a
/// distance rather than a comparison.
fn newer(a: u32, b: u32) -> bool {
    a != b && a.wrapping_sub(b) < u32::MAX / 2
}

/// What Slint has to be told after a window lands.
///
/// A reset destroys and rebuilds every instantiated row -- several hundred items
/// for this list -- so it is only warranted when the list length itself moved,
/// which is what `row_count` reports. A window landing is otherwise a pure
/// content change, however many rows it covers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Notify {
    Rows,
    Reset,
}

fn notification(old_total: u32, new_total: u32) -> Notify {
    if old_total == new_total {
        Notify::Rows
    } else {
        Notify::Reset
    }
}

/// Which of the two caches a window belongs in.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Which {
    Pc,
    Browse,
}

/// One cached run of rows.
#[derive(Default)]
struct Window {
    start: u32,
    rows: Vec<DisasmRow>,
}

impl Window {
    fn range(&self) -> Range<u32> {
        self.start..self.start + self.rows.len() as u32
    }

    fn get(&self, row: u32) -> Option<&DisasmRow> {
        row.checked_sub(self.start)
            .and_then(|offset| self.rows.get(offset as usize))
    }

    fn clear(&mut self) {
        self.start = 0;
        self.rows.clear();
    }
}

/// Rows the list shows, backed by windows of the server's trace.
pub struct DisasmModel {
    inner: RefCell<Inner>,
    notify: ModelNotify,
}

#[derive(Default)]
struct Inner {
    total: u32,
    /// The newest trace shape seen. Anything older names shifted indices.
    generation: u32,
    /// Pushed on every pause, always centred on the PC.
    pc: Window,
    /// Fetched by scrolling and by following a jump.
    browse: Window,
    /// Where the PC is in the trace, when it is traced at all.
    pc_index: Option<u32>,
    /// Breakpoint addresses, so the gutter can be drawn without a second model.
    breakpoints: Vec<u16>,
}

impl Default for DisasmModel {
    fn default() -> Self {
        Self {
            inner: RefCell::new(Inner::default()),
            notify: ModelNotify::default(),
        }
    }
}

impl DisasmModel {
    /// Folds in the PC-centred window that rides on every debug update.
    ///
    /// Returns whether the trace has grown since the last window, which shifts
    /// every index after the new code and so invalidates a scroll position.
    pub fn set_pc_window(&self, window: &DisassemblyWindow) -> bool {
        self.apply(window, Which::Pc)
    }

    /// Folds in a window fetched by scrolling or by following a jump.
    pub fn set_browse_window(&self, window: &DisassemblyWindow) -> bool {
        self.apply(window, Which::Browse)
    }

    fn apply(&self, window: &DisassemblyWindow, which: Which) -> bool {
        let mut touched: Vec<Range<u32>> = Vec::with_capacity(3);
        let grew;
        let notify = {
            let mut inner = self.inner.borrow_mut();

            // A reply rendered before the trace last grew describes indices that
            // have since shifted. There is nothing in it worth salvaging.
            if newer(inner.generation, window.generation) {
                return false;
            }

            let old_total = inner.total;
            grew = newer(window.generation, inner.generation);
            if grew {
                // The trace grew, so every index the other window holds moved
                // with it.
                inner.generation = window.generation;
                let other = match which {
                    Which::Pc => &mut inner.browse,
                    Which::Browse => &mut inner.pc,
                };
                touched.push(other.range());
                other.clear();
            }

            inner.total = window.total;
            if which == Which::Pc {
                inner.pc_index = window.pc_index;
            }

            let breakpoints = inner.breakpoints.clone();
            let rows: Vec<DisasmRow> = window
                .lines
                .iter()
                .map(|line| row_of(line, &breakpoints))
                .collect();
            let target = match which {
                Which::Pc => &mut inner.pc,
                Which::Browse => &mut inner.browse,
            };
            touched.push(target.range());
            target.start = window.start_index;
            target.rows = rows;
            touched.push(target.range());

            notification(old_total, inner.total)
        };
        self.announce(notify, touched);
        grew
    }

    pub fn set_breakpoints(&self, breakpoints: Vec<u16>) {
        let touched = {
            let mut inner = self.inner.borrow_mut();
            inner.breakpoints = breakpoints;
            let Inner {
                pc,
                browse,
                breakpoints,
                ..
            } = &mut *inner;
            for row in pc.rows.iter_mut().chain(browse.rows.iter_mut()) {
                row.has_breakpoint = breakpoints.contains(&(row.address as u16));
            }
            vec![pc.range(), browse.range()]
        };
        // The list is the same length, so this is a content change like any other.
        self.announce(Notify::Rows, touched);
    }

    pub fn clear(&self) {
        *self.inner.borrow_mut() = Inner::default();
        self.notify.reset();
    }

    fn announce(&self, notify: Notify, touched: Vec<Range<u32>>) {
        match notify {
            Notify::Reset => self.notify.reset(),
            Notify::Rows => {
                for row in touched.into_iter().flatten() {
                    self.notify.row_changed(row as usize);
                }
            }
        }
    }

    /// The range of trace indices the browse window holds.
    ///
    /// Deliberately not the union with the PC window: that one is replaced
    /// wholesale on every pause, so counting it as covered would suppress the
    /// prefetch and the next step would drop the rows back to placeholders.
    pub fn cached(&self) -> Range<u32> {
        self.inner.borrow().browse.range()
    }

    pub fn total(&self) -> u32 {
        self.inner.borrow().total
    }

    /// The address of the row at `index`, for re-anchoring once the trace has
    /// grown and the indices have shifted underneath.
    pub fn address_of(&self, index: u32) -> Option<(u16, u16)> {
        let inner = self.inner.borrow();
        let row = inner.browse.get(index).or_else(|| inner.pc.get(index))?;
        Some((row.bank as u16, row.in_bank as u16))
    }
}

/// A row the client has not fetched yet. Rendered greyed, so a fast scroll shows
/// the shape of the listing rather than a blank panel.
fn placeholder() -> DisasmRow {
    DisasmRow {
        loaded: false,
        ..Default::default()
    }
}

fn row_of(line: &gameroy_server::protocol::DisasmLine, breakpoints: &[u16]) -> DisasmRow {
    DisasmRow {
        // Bank-qualified: `pc` alone is ambiguous, because it folds every bank
        // above zero onto 0x4000..0x7fff, and rows are ordered by bank first --
        // so the column appears to count backwards at each bank boundary.
        address_text: format!("{:02x}_{:04x}", line.address.bank, line.pc).into(),
        label: line.label.clone().unwrap_or_default().into(),
        mnemonic: line.mnemonic.clone().into(),
        operand_prefix: line.operand_prefix.clone().into(),
        link_text: line.link_text.clone().into(),
        operand_suffix: line.operand_suffix.clone().into(),
        // -1 rather than an Option: Slint structs have no sum types.
        jump_index: line.jump_index.map_or(-1, |i| i as i32),
        jump_bank: line.jump_target.map_or(-1, |a| a.bank as i32),
        jump_address: line.jump_target.map_or(-1, |a| a.address as i32),
        // The CPU address, which is what a breakpoint takes.
        address: line.pc as i32,
        bank: line.address.bank as i32,
        in_bank: line.address.address as i32,
        // `is_current` is derived in `row_data`, not taken from the wire.
        is_current: false,
        has_breakpoint: breakpoints.contains(&line.pc),
        loaded: true,
    }
}

impl Model for DisasmModel {
    type Data = DisasmRow;

    fn row_count(&self) -> usize {
        self.inner.borrow().total as usize
    }

    fn row_data(&self, row: usize) -> Option<DisasmRow> {
        let inner = self.inner.borrow();
        if row >= inner.total as usize {
            return None;
        }
        let index = row as u32;
        // A row before a window is as much a placeholder as one after it.
        // Returning `None` here instead would tell Slint the row does not exist,
        // which is a different thing entirely from "not fetched yet".
        //
        // The PC window wins where they overlap: it is the more recently
        // rendered of the two.
        let mut out = inner
            .pc
            .get(index)
            .or_else(|| inner.browse.get(index))
            .cloned()
            .unwrap_or_else(placeholder);
        // Derived rather than trusted: a window rendered before the last jump
        // still claims the PC where it was, and two highlighted rows is worse
        // than none.
        out.is_current = inner.pc_index == Some(index);
        Some(out)
    }

    fn model_tracker(&self) -> &dyn ModelTracker {
        &self.notify
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use gameroy_server::protocol::{Address, DisasmLine};

    fn window(start: u32, len: u32, total: u32) -> DisassemblyWindow {
        window_at(start, len, total, 1)
    }

    fn window_at(start: u32, len: u32, total: u32, generation: u32) -> DisassemblyWindow {
        DisassemblyWindow {
            start_index: start,
            total,
            generation,
            lines: (0..len)
                .map(|i| DisasmLine {
                    pc: (start + i) as u16,
                    mnemonic: "nop".into(),
                    ..Default::default()
                })
                .collect(),
            pc_index: None,
            focus_index: None,
        }
    }

    /// A window whose rows are recognisable, so precedence between the two
    /// caches can be asserted on something other than an index.
    fn named(start: u32, len: u32, total: u32, mnemonic: &str) -> DisassemblyWindow {
        let mut w = window(start, len, total);
        for line in &mut w.lines {
            line.mnemonic = mnemonic.into();
        }
        w
    }

    #[test]
    fn the_list_is_as_long_as_the_trace_not_the_window() {
        let model = DisasmModel::default();
        model.set_browse_window(&window(100, 10, 5000));
        assert_eq!(model.row_count(), 5000);
        assert_eq!(model.cached(), 100..110);
    }

    #[test]
    fn rows_outside_the_window_are_placeholders() {
        let model = DisasmModel::default();
        model.set_browse_window(&window(100, 10, 5000));

        let inside = model.row_data(105).unwrap();
        assert!(inside.loaded);
        assert_eq!(inside.address_text, "00_0069"); // 105 in hex

        // Both directions, and the far end.
        for row in [0, 99, 110, 4999] {
            assert!(
                !model.row_data(row).unwrap().loaded,
                "row {row} should be a placeholder"
            );
        }
        // Past the end there is no row at all.
        assert!(model.row_data(5000).is_none());
    }

    #[test]
    fn a_comfortable_view_asks_for_nothing() {
        // 24 rows in the middle of a 256-row cache, well clear of both margins.
        assert_eq!(window_needed(200, 24, 100..356, 5000), None);
    }

    #[test]
    fn nearing_either_edge_asks_for_more() {
        // Within MARGIN of the top of the cache.
        assert!(window_needed(110, 24, 100..356, 5000).is_some());
        // Within MARGIN of the bottom.
        assert!(window_needed(320, 24, 100..356, 5000).is_some());
        // Completely outside it.
        assert!(window_needed(4000, 24, 100..356, 5000).is_some());
    }

    #[test]
    fn the_ends_of_the_trace_are_not_edges_to_flee() {
        // Against the very start, with the cache already starting at 0.
        assert_eq!(window_needed(0, 24, 0..256, 5000), None);
        // Against the very end, with the cache reaching it.
        assert_eq!(window_needed(4970, 24, 4744..5000, 5000), None);
    }

    #[test]
    fn an_empty_trace_asks_for_nothing() {
        assert_eq!(window_needed(0, 24, 0..0, 0), None);
    }

    /// The chunk a scroll report asks for has to settle the very next report.
    /// When it does not, every mouse-move during a drag re-requests a chunk the
    /// client already holds in full.
    #[test]
    fn the_chunk_asked_for_settles_the_next_report() {
        for total in [600u32, 2000, 5000] {
            for count in [24u32, 30, 39, 64] {
                for first in 0..total {
                    let Some((start, want)) = window_needed(first, count, 0..0, total) else {
                        continue;
                    };
                    let cached = served(start, total);
                    assert_eq!(
                        window_needed(first, count, cached.clone(), total),
                        None,
                        "asked for {start}..+{want} at first={first} count={count} \
                         total={total}, which lands as {cached:?} and asks again"
                    );
                }
            }
        }
    }

    /// The case that showed it: the aligned chunk left the view inside its own
    /// bottom margin.
    #[test]
    fn a_chunk_is_not_aligned_past_its_own_margin() {
        let (start, _) = window_needed(470, 30, 0..0, 2000).unwrap();
        assert_eq!(window_needed(470, 30, served(start, 2000), 2000), None);
    }

    #[test]
    fn requests_are_aligned_where_alignment_still_settles() {
        let (start, count) = window_needed(4000, 24, 0..0, 5000).unwrap();
        assert_eq!(start % ALIGN, 0, "unaligned request at {start}");
        assert_eq!(count, CHUNK);
        assert!(start < 5000);

        // Near the start it must not go negative.
        let (start, _) = window_needed(5, 24, 0..0, 5000).unwrap();
        assert_eq!(start, 0);
    }

    #[test]
    fn breakpoints_mark_the_rows_they_are_on() {
        let model = DisasmModel::default();
        model.set_browse_window(&window(0, 10, 10));
        model.set_breakpoints(vec![3, 7]);
        for row in 0..10u32 {
            let marked = model.row_data(row as usize).unwrap().has_breakpoint;
            assert_eq!(marked, row == 3 || row == 7, "row {row}");
        }
    }

    #[test]
    fn a_step_does_not_throw_away_what_was_scrolled_to() {
        let model = DisasmModel::default();
        model.set_browse_window(&named(0, 256, 9000, "browse"));
        model.set_pc_window(&named(300, 64, 9000, "pc"));

        assert_eq!(model.row_data(100).unwrap().mnemonic, "browse");
        assert_eq!(model.row_data(320).unwrap().mnemonic, "pc");
        assert!(!model.row_data(280).unwrap().loaded);
        // The browse window is what the prefetch reasons about, and a PC window
        // landing elsewhere must not look like coverage.
        assert_eq!(model.cached(), 0..256);
    }

    #[test]
    fn the_pc_window_wins_where_they_overlap() {
        let model = DisasmModel::default();
        model.set_browse_window(&named(0, 256, 9000, "browse"));
        model.set_pc_window(&named(200, 64, 9000, "pc"));
        assert_eq!(model.row_data(199).unwrap().mnemonic, "browse");
        assert_eq!(model.row_data(200).unwrap().mnemonic, "pc");
    }

    #[test]
    fn a_grown_trace_drops_the_window_it_shifted() {
        let model = DisasmModel::default();
        model.set_browse_window(&named(0, 256, 9000, "browse"));
        model.set_pc_window(&named(300, 64, 9000, "pc"));

        // The trace grew: a fresh PC window arrives at the next generation.
        let mut grown = named(300, 64, 9100, "grown");
        grown.generation = 2;
        model.set_pc_window(&grown);

        assert_eq!(model.row_data(320).unwrap().mnemonic, "grown");
        assert!(!model.row_data(100).unwrap().loaded, "stale rows kept");
        assert_eq!(model.cached(), 0..0);
    }

    #[test]
    fn a_reply_from_before_the_trace_grew_is_ignored() {
        let model = DisasmModel::default();
        let mut fresh = named(300, 64, 9100, "pc");
        fresh.generation = 2;
        model.set_pc_window(&fresh);

        // A prefetch issued before the growth finally lands.
        let stale = named(0, 256, 9000, "stale");
        model.set_browse_window(&stale);

        assert_eq!(model.row_data(320).unwrap().mnemonic, "pc");
        assert!(!model.row_data(100).unwrap().loaded);
        assert_eq!(model.cached(), 0..0, "stale window was installed");
        assert_eq!(model.row_count(), 9100, "total walked backwards");
    }

    #[test]
    fn exactly_one_row_is_current() {
        let model = DisasmModel::default();
        // A window rendered when the PC was at 300 still says so.
        let mut browse = named(256, 256, 9000, "browse");
        browse.lines[44].is_current = true; // row 300
        model.set_browse_window(&browse);

        let mut pc = named(8968, 64, 9000, "pc");
        pc.pc_index = Some(9000 - 1);
        model.set_pc_window(&pc);

        assert!(!model.row_data(300).unwrap().is_current);
        assert!(model.row_data(8999).unwrap().is_current);
    }

    #[test]
    fn follows_the_pc_only_while_its_row_is_on_screen() {
        assert!(!follows_pc(&(100..140), None));
        assert!(follows_pc(&(100..140), Some(100)));
        assert!(follows_pc(&(100..140), Some(139)));
        assert!(!follows_pc(&(100..140), Some(99)));
        assert!(!follows_pc(&(100..140), Some(140)));
        assert!(!follows_pc(&(0..0), Some(0)));
    }

    #[test]
    fn only_a_length_change_forces_a_reset() {
        assert_eq!(notification(5000, 5000), Notify::Rows);
        assert_eq!(notification(5000, 5100), Notify::Reset);
    }

    #[test]
    fn the_address_column_tells_the_banks_apart() {
        let model = DisasmModel::default();
        let mut w = window(0, 2, 2);
        w.lines[0].address = Address {
            bank: 0,
            address: 0x0150,
        };
        w.lines[0].pc = 0x0150;
        w.lines[1].address = Address {
            bank: 7,
            address: 0x0a12,
        };
        w.lines[1].pc = 0x4a12;
        model.set_browse_window(&w);

        assert_eq!(model.row_data(0).unwrap().address_text, "00_0150");
        assert_eq!(model.row_data(1).unwrap().address_text, "07_4a12");
        // The breakpoint gutter still sends the CPU address.
        assert_eq!(model.row_data(1).unwrap().address, 0x4a12);
    }
}
