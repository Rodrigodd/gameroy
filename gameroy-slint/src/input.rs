//! Keyboard to joypad mapping.

use slint::SharedString;
use slint::platform::Key;

/// The bit of each key in the joypad bitmask, where 0 means pressed and 1 released.
/// Bits 0-3 are the direction keys, bits 4-7 the buttons.
const JOYPAD_KEYS: [(Key, u8); 8] = [
    (Key::RightArrow, 0),
    (Key::LeftArrow, 1),
    (Key::UpArrow, 2),
    (Key::DownArrow, 3),
    (Key::X, 4),         // A
    (Key::Z, 5),         // B
    (Key::Backspace, 6), // Select
    (Key::Return, 7),    // Start
];

pub fn joypad_bit(text: &SharedString) -> Option<u8> {
    JOYPAD_KEYS
        .iter()
        .find(|(key, _)| *text == SharedString::from(*key))
        .map(|&(_, bit)| bit)
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
    pub fn press(&mut self, text: &SharedString) -> Option<u8> {
        let bit = joypad_bit(text)?;
        self.0 &= !(1 << bit);
        Some(self.0)
    }

    pub fn release(&mut self, text: &SharedString) -> Option<u8> {
        let bit = joypad_bit(text)?;
        self.0 |= 1 << bit;
        Some(self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn maps_every_key_and_nothing_else() {
        for (key, bit) in JOYPAD_KEYS {
            assert_eq!(joypad_bit(&SharedString::from(key)), Some(bit), "{key:?}");
        }
        assert_eq!(joypad_bit(&SharedString::from("q")), None);
        assert_eq!(joypad_bit(&SharedString::from(Key::Tab)), None);
    }

    #[test]
    fn press_clears_and_release_sets() {
        let mut jp = Joypad::default();
        assert_eq!(
            jp.press(&SharedString::from(Key::Return)),
            Some(0b0111_1111)
        );
        assert_eq!(
            jp.press(&SharedString::from(Key::LeftArrow)),
            Some(0b0111_1101)
        );
        assert_eq!(
            jp.release(&SharedString::from(Key::Return)),
            Some(0b1111_1101)
        );
        assert_eq!(jp.press(&SharedString::from("q")), None);
    }
}

/// An action that is held rather than triggered.
///
/// Only these two live here. Every momentary shortcut (save/load state, step,
/// run, debugger, menu) is a `KeyBinding` in the .slint file, which cannot report
/// a release -- fine for those, but useless for anything hold-to-act.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HoldKey {
    FastForward,
    Rewind,
}

pub fn hold_key(text: &SharedString) -> Option<HoldKey> {
    if *text == SharedString::from(Key::Shift) {
        Some(HoldKey::FastForward)
    } else if text.eq_ignore_ascii_case("r") {
        Some(HoldKey::Rewind)
    } else {
        None
    }
}

/// True when the key needs press/release tracking, so the UI knows to consume it.
/// Shortcut keys are deliberately absent: `KeyBinding` claims those itself.
pub fn is_handled(text: &SharedString) -> bool {
    joypad_bit(text).is_some() || hold_key(text).is_some()
}

#[cfg(test)]
mod hold_key_tests {
    use super::*;

    #[test]
    fn recognises_the_two_hold_actions() {
        assert_eq!(
            hold_key(&SharedString::from(Key::Shift)),
            Some(HoldKey::FastForward)
        );
        assert_eq!(hold_key(&SharedString::from("r")), Some(HoldKey::Rewind));
        assert_eq!(hold_key(&SharedString::from("R")), Some(HoldKey::Rewind));
        assert_eq!(hold_key(&SharedString::from("q")), None);
    }

    /// Shortcut keys belong to KeyBinding now. If one also claimed a press/release
    /// handler, both would fire for a single press.
    #[test]
    fn shortcut_keys_are_left_to_key_binding() {
        for key in [Key::F5, Key::F6, Key::F7, Key::F8, Key::F9, Key::F12] {
            let text = SharedString::from(key);
            assert!(!is_handled(&text), "{key:?} must be left to KeyBinding");
        }
        assert!(!is_handled(&SharedString::from("m")));
    }

    #[test]
    fn joypad_and_hold_keys_do_not_overlap() {
        for (key, _) in JOYPAD_KEYS {
            let text = SharedString::from(key);
            assert!(
                hold_key(&text).is_none(),
                "{key:?} is both a joypad key and a hold action"
            );
            assert!(is_handled(&text));
        }
    }
}
