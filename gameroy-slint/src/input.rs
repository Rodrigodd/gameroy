//! Keyboard mapping: the joypad, the hold actions and the shortcuts.
//!
//! Every binding is configurable, so none of this can be a `match` on a literal.
//! Keys are named the way a person would write them in a config file -- `Left`,
//! `F5`, `X` -- and resolved once into the string Slint reports for that key,
//! which is what a key event actually carries.

use serde::{Deserialize, Serialize};
use slint::SharedString;
use slint::platform::Key;

/// Resolves a configured key name to the text Slint reports when it is pressed.
///
/// A single character stands for itself; everything else is a named key. Slint
/// reports letters as their lowercase character, so the comparison downstream is
/// case-insensitive rather than this normalising here.
pub fn key_from_name(name: &str) -> Option<SharedString> {
    let named = |k: Key| Some(SharedString::from(k));
    match name.trim() {
        "" => None,
        "Left" => named(Key::LeftArrow),
        "Right" => named(Key::RightArrow),
        "Up" => named(Key::UpArrow),
        "Down" => named(Key::DownArrow),
        "Return" | "Enter" => named(Key::Return),
        "Backspace" => named(Key::Backspace),
        "Shift" => named(Key::Shift),
        "Control" | "Ctrl" => named(Key::Control),
        "Alt" => named(Key::Alt),
        "Tab" => named(Key::Tab),
        "Escape" | "Esc" => named(Key::Escape),
        "Space" => Some(SharedString::from(" ")),
        "F1" => named(Key::F1),
        "F2" => named(Key::F2),
        "F3" => named(Key::F3),
        "F4" => named(Key::F4),
        "F5" => named(Key::F5),
        "F6" => named(Key::F6),
        "F7" => named(Key::F7),
        "F8" => named(Key::F8),
        "F9" => named(Key::F9),
        "F10" => named(Key::F10),
        "F11" => named(Key::F11),
        "F12" => named(Key::F12),
        // Anything else has to be a single character to be a key at all.
        other => {
            let mut chars = other.chars();
            let c = chars.next()?;
            chars
                .next()
                .is_none()
                .then(|| SharedString::from(c.to_lowercase().collect::<String>()))
        }
    }
}

/// True when a key event matches a configured binding.
fn matches(text: &SharedString, binding: &str) -> bool {
    key_from_name(binding).is_some_and(|k| k.eq_ignore_ascii_case(text.as_str()))
}

/// Every binding, as the config file spells them.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(default)]
pub struct KeyMap {
    pub left: String,
    pub right: String,
    pub up: String,
    pub down: String,
    pub a: String,
    pub b: String,
    pub select: String,
    pub start: String,

    pub speed: String,
    pub rewind: String,
    pub save_state: String,
    pub load_state: String,
    pub menu: String,

    pub open_debugger: String,
    pub debug_step: String,
    pub debug_stepback: String,
    pub debug_run: String,
}

impl Default for KeyMap {
    fn default() -> Self {
        Self {
            left: "Left".into(),
            right: "Right".into(),
            up: "Up".into(),
            down: "Down".into(),
            a: "X".into(),
            b: "Z".into(),
            select: "Backspace".into(),
            start: "Return".into(),

            speed: "Shift".into(),
            rewind: "R".into(),
            save_state: "F5".into(),
            load_state: "F6".into(),
            menu: "M".into(),

            open_debugger: "F12".into(),
            debug_step: "F8".into(),
            debug_stepback: "F7".into(),
            debug_run: "F9".into(),
        }
    }
}

impl KeyMap {
    /// The joypad bit this key drives, if any.
    ///
    /// Bit 0-3 are the directions (right, left, up, down) and 4-7 the buttons
    /// (A, B, select, start), where 0 means pressed.
    pub fn joypad_bit(&self, text: &SharedString) -> Option<u8> {
        let bindings = [
            (&self.right, 0u8),
            (&self.left, 1),
            (&self.up, 2),
            (&self.down, 3),
            (&self.a, 4),
            (&self.b, 5),
            (&self.select, 6),
            (&self.start, 7),
        ];
        bindings
            .iter()
            .find(|(binding, _)| matches(text, binding))
            .map(|&(_, bit)| bit)
    }

    pub fn hold_key(&self, text: &SharedString) -> Option<HoldKey> {
        if matches(text, &self.speed) {
            Some(HoldKey::FastForward)
        } else if matches(text, &self.rewind) {
            Some(HoldKey::Rewind)
        } else {
            None
        }
    }

    /// The one-shot action this key triggers, if any.
    pub fn shortcut(&self, text: &SharedString) -> Option<Shortcut> {
        let bindings = [
            (&self.save_state, Shortcut::SaveState),
            (&self.load_state, Shortcut::LoadState),
            (&self.menu, Shortcut::Menu),
            (&self.open_debugger, Shortcut::ToggleDebugger),
            (&self.debug_step, Shortcut::Step),
            (&self.debug_stepback, Shortcut::StepBack),
            (&self.debug_run, Shortcut::ToggleRun),
        ];
        bindings
            .iter()
            .find(|(binding, _)| matches(text, binding))
            .map(|&(_, action)| action)
    }

    /// True when the key needs press/release tracking or fires an action, so the
    /// focus scope knows to consume it.
    pub fn is_handled(&self, text: &SharedString) -> bool {
        self.joypad_bit(text).is_some()
            || self.hold_key(text).is_some()
            || self.shortcut(text).is_some()
    }
}

/// A momentary action.
///
/// These used to be declarative `KeyBinding`s in the .slint file, which cannot
/// take a key chosen at runtime. Making them configurable means dispatching them
/// here instead.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Shortcut {
    SaveState,
    LoadState,
    Menu,
    ToggleDebugger,
    Step,
    StepBack,
    ToggleRun,
}

/// Tracks which keys are held, as the bitmask the emulator expects.
pub struct Joypad(u8);

impl Default for Joypad {
    fn default() -> Self {
        // All released.
        Self(0xff)
    }
}

impl Joypad {
    /// Returns the new bitmask if this key changed anything.
    pub fn press(&mut self, keymap: &KeyMap, text: &SharedString) -> Option<u8> {
        let bit = keymap.joypad_bit(text)?;
        self.0 &= !(1 << bit);
        Some(self.0)
    }

    pub fn release(&mut self, keymap: &KeyMap, text: &SharedString) -> Option<u8> {
        let bit = keymap.joypad_bit(text)?;
        self.0 |= 1 << bit;
        Some(self.0)
    }
}

/// An action that is held rather than triggered, so it needs both the press and
/// the release.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HoldKey {
    FastForward,
    Rewind,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn maps_every_default_joypad_key_and_nothing_else() {
        let keymap = KeyMap::default();
        let expected = [
            (Key::RightArrow, 0),
            (Key::LeftArrow, 1),
            (Key::UpArrow, 2),
            (Key::DownArrow, 3),
            (Key::X, 4),
            (Key::Z, 5),
            (Key::Backspace, 6),
            (Key::Return, 7),
        ];
        for (key, bit) in expected {
            assert_eq!(
                keymap.joypad_bit(&SharedString::from(key)),
                Some(bit),
                "{key:?}"
            );
        }
        assert_eq!(keymap.joypad_bit(&SharedString::from("q")), None);
        assert_eq!(keymap.joypad_bit(&SharedString::from(Key::Tab)), None);
    }

    #[test]
    fn press_clears_and_release_sets() {
        let keymap = KeyMap::default();
        let mut jp = Joypad::default();
        assert_eq!(
            jp.press(&keymap, &SharedString::from(Key::Return)),
            Some(0b0111_1111)
        );
        assert_eq!(
            jp.press(&keymap, &SharedString::from(Key::LeftArrow)),
            Some(0b0111_1101)
        );
        assert_eq!(
            jp.release(&keymap, &SharedString::from(Key::Return)),
            Some(0b1111_1101)
        );
        assert_eq!(jp.press(&keymap, &SharedString::from("q")), None);
    }

    #[test]
    fn recognises_the_two_hold_actions() {
        let keymap = KeyMap::default();
        assert_eq!(
            keymap.hold_key(&SharedString::from(Key::Shift)),
            Some(HoldKey::FastForward)
        );
        assert_eq!(
            keymap.hold_key(&SharedString::from("r")),
            Some(HoldKey::Rewind)
        );
        assert_eq!(
            keymap.hold_key(&SharedString::from("R")),
            Some(HoldKey::Rewind)
        );
        assert_eq!(keymap.hold_key(&SharedString::from("q")), None);
    }

    #[test]
    fn maps_every_default_shortcut() {
        let keymap = KeyMap::default();
        let expected = [
            (Key::F5, Shortcut::SaveState),
            (Key::F6, Shortcut::LoadState),
            (Key::F7, Shortcut::StepBack),
            (Key::F8, Shortcut::Step),
            (Key::F9, Shortcut::ToggleRun),
            (Key::F12, Shortcut::ToggleDebugger),
        ];
        for (key, action) in expected {
            assert_eq!(
                keymap.shortcut(&SharedString::from(key)),
                Some(action),
                "{key:?}"
            );
        }
        assert_eq!(
            keymap.shortcut(&SharedString::from("m")),
            Some(Shortcut::Menu)
        );
        assert_eq!(keymap.shortcut(&SharedString::from("q")), None);
    }

    /// A key doing two jobs at once would fire both on a single press.
    #[test]
    fn the_default_bindings_do_not_overlap() {
        let keymap = KeyMap::default();
        let every_binding = [
            &keymap.left,
            &keymap.right,
            &keymap.up,
            &keymap.down,
            &keymap.a,
            &keymap.b,
            &keymap.select,
            &keymap.start,
            &keymap.speed,
            &keymap.rewind,
            &keymap.save_state,
            &keymap.load_state,
            &keymap.menu,
            &keymap.open_debugger,
            &keymap.debug_step,
            &keymap.debug_stepback,
            &keymap.debug_run,
        ];
        for binding in every_binding {
            let text = key_from_name(binding).expect("a default binding must resolve");
            let roles = keymap.joypad_bit(&text).is_some() as u8
                + keymap.hold_key(&text).is_some() as u8
                + keymap.shortcut(&text).is_some() as u8;
            assert_eq!(roles, 1, "{binding:?} has {roles} roles, not exactly one");
            assert!(keymap.is_handled(&text), "{binding:?} is not consumed");
        }
    }

    #[test]
    fn a_rebound_key_takes_effect_and_the_old_one_stops() {
        let keymap = KeyMap {
            a: "Q".into(),
            ..KeyMap::default()
        };
        assert_eq!(keymap.joypad_bit(&SharedString::from("q")), Some(4));
        assert_eq!(keymap.joypad_bit(&SharedString::from(Key::X)), None);
    }

    #[test]
    fn key_names_resolve_or_are_rejected() {
        assert_eq!(
            key_from_name("Left"),
            Some(SharedString::from(Key::LeftArrow))
        );
        assert_eq!(key_from_name("F12"), Some(SharedString::from(Key::F12)));
        // Letters are reported lowercase by Slint, whatever the config says.
        assert_eq!(key_from_name("X").as_deref(), Some("x"));
        assert_eq!(key_from_name("x").as_deref(), Some("x"));
        // Nonsense must be rejected rather than silently binding nothing usable.
        assert_eq!(key_from_name("NotAKey"), None);
        assert_eq!(key_from_name(""), None);
    }

    /// An unbound action must not swallow every key: the empty string resolves to
    /// nothing, so no press can match it.
    #[test]
    fn an_empty_binding_matches_nothing() {
        let keymap = KeyMap {
            menu: String::new(),
            ..KeyMap::default()
        };
        assert_eq!(keymap.shortcut(&SharedString::from("m")), None);
        assert!(!keymap.is_handled(&SharedString::from("m")));
    }
}
