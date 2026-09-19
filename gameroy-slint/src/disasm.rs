//! The disassembly list's model.
//!
//! The trace runs to tens of thousands of instructions, so the client holds a
//! window of rendered rows and tells Slint the list is as long as the whole
//! trace. `ListView` only asks for the rows it is about to draw, so the ones
//! outside the window are never requested unless the user scrolls to them --
//! at which point [`window_needed`] says to go and fetch them.

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

/// The chunk to fetch for a view of `count` rows at `first`, or `None` when the
/// cache already covers it with room to spare.
pub fn window_needed(first: u32, count: u32, cached: Range<u32>, total: u32) -> Option<(u32, u16)> {
    if total == 0 {
        return None;
    }
    let last = (first + count).min(total);

    let inside = !cached.is_empty() && first >= cached.start && last <= cached.end;
    // A margin is only worth having when there is somewhere for it to be: at
    // either end of the trace, being flush against the edge is as good as it gets.
    let room_above = cached.start == 0 || first >= cached.start.saturating_add(MARGIN);
    let room_below = cached.end >= total || last.saturating_add(MARGIN) <= cached.end;
    if inside && room_above && room_below {
        return None;
    }

    // Centre the chunk on the view, then align it.
    let centre = first + count / 2;
    let start = centre.saturating_sub(CHUNK as u32 / 2) / ALIGN * ALIGN;
    let start = start.min(total.saturating_sub(1));
    Some((start, CHUNK))
}

/// Rows the list shows, backed by a window of the server's trace.
pub struct DisasmModel {
    inner: RefCell<Inner>,
    notify: ModelNotify,
}

struct Inner {
    total: u32,
    generation: u32,
    start: u32,
    rows: Vec<DisasmRow>,
    /// Breakpoint addresses, so the gutter can be drawn without a second model.
    breakpoints: Vec<u16>,
}

impl Default for DisasmModel {
    fn default() -> Self {
        Self {
            inner: RefCell::new(Inner {
                total: 0,
                generation: 0,
                start: 0,
                rows: Vec::new(),
                breakpoints: Vec::new(),
            }),
            notify: ModelNotify::default(),
        }
    }
}

impl DisasmModel {
    /// Folds in a window the server sent.
    pub fn set_window(&self, window: &DisassemblyWindow) {
        {
            let mut inner = self.inner.borrow_mut();
            inner.total = window.total;
            inner.generation = window.generation;
            inner.start = window.start_index;
            let breakpoints = inner.breakpoints.clone();
            inner.rows = window
                .lines
                .iter()
                .map(|l| row_of(l, &breakpoints))
                .collect();
        }
        // The length can change with the trace, so this is a reset rather than a
        // range change.
        self.notify.reset();
    }

    pub fn set_breakpoints(&self, breakpoints: Vec<u16>) {
        {
            let mut inner = self.inner.borrow_mut();
            inner.breakpoints = breakpoints;
            let Inner {
                rows, breakpoints, ..
            } = &mut *inner;
            for row in rows {
                row.has_breakpoint = breakpoints.contains(&(row.address as u16));
            }
        }
        self.notify.reset();
    }

    pub fn clear(&self) {
        *self.inner.borrow_mut() = Inner {
            total: 0,
            generation: 0,
            start: 0,
            rows: Vec::new(),
            breakpoints: Vec::new(),
        };
        self.notify.reset();
    }

    /// The range of trace indices currently held.
    pub fn cached(&self) -> Range<u32> {
        let inner = self.inner.borrow();
        inner.start..inner.start + inner.rows.len() as u32
    }

    pub fn total(&self) -> u32 {
        self.inner.borrow().total
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
        address_text: format!("{:04x}", line.pc).into(),
        label: line.label.clone().unwrap_or_default().into(),
        mnemonic: line.mnemonic.clone().into(),
        operand_prefix: line.operand_prefix.clone().into(),
        link_text: line.link_text.clone().into(),
        operand_suffix: line.operand_suffix.clone().into(),
        // -1 rather than an Option: Slint structs have no sum types.
        jump_index: line.jump_index.map_or(-1, |i| i as i32),
        jump_bank: line.jump_target.map_or(-1, |a| a.bank as i32),
        jump_address: line.jump_target.map_or(-1, |a| a.address as i32),
        address: line.pc as i32,
        bank: line.address.bank as i32,
        in_bank: line.address.address as i32,
        is_current: line.is_current,
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
        // A row before the window is as much a placeholder as one after it.
        // Returning `None` here instead would tell Slint the row does not exist,
        // which is a different thing entirely from "not fetched yet".
        Some(
            (row as u32)
                .checked_sub(inner.start)
                .and_then(|offset| inner.rows.get(offset as usize))
                .cloned()
                .unwrap_or_else(placeholder),
        )
    }

    fn model_tracker(&self) -> &dyn ModelTracker {
        &self.notify
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use gameroy_server::protocol::DisasmLine;

    fn window(start: u32, len: u32, total: u32) -> DisassemblyWindow {
        DisassemblyWindow {
            start_index: start,
            total,
            generation: 1,
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

    #[test]
    fn the_list_is_as_long_as_the_trace_not_the_window() {
        let model = DisasmModel::default();
        model.set_window(&window(100, 10, 5000));
        assert_eq!(model.row_count(), 5000);
        assert_eq!(model.cached(), 100..110);
    }

    #[test]
    fn rows_outside_the_window_are_placeholders() {
        let model = DisasmModel::default();
        model.set_window(&window(100, 10, 5000));

        let inside = model.row_data(105).unwrap();
        assert!(inside.loaded);
        assert_eq!(inside.address_text, "0069"); // 105 in hex

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
    fn requests_are_aligned_and_stay_in_range() {
        let (start, count) = window_needed(4000, 24, 0..0, 5000).unwrap();
        assert_eq!(start % ALIGN, 0, "unaligned request at {start}");
        assert_eq!(count, CHUNK);
        assert!(start < 5000);

        // Near the start it must not go negative.
        let (start, _) = window_needed(5, 24, 0..0, 5000).unwrap();
        assert_eq!(start, 0);
    }

    #[test]
    fn an_empty_trace_asks_for_nothing() {
        assert_eq!(window_needed(0, 24, 0..0, 0), None);
    }

    #[test]
    fn breakpoints_mark_the_rows_they_are_on() {
        let model = DisasmModel::default();
        model.set_window(&window(0, 10, 10));
        model.set_breakpoints(vec![3, 7]);
        for row in 0..10u32 {
            let marked = model.row_data(row as usize).unwrap().has_breakpoint;
            assert_eq!(marked, row == 3 || row == 7, "row {row}");
        }
    }
}
