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

/// A non-joypad action bound to a key.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Hotkey {
    /// Held, not toggled.
    FastForward,
    /// Held, not toggled.
    Rewind,
    SaveState,
    LoadState,
    ToggleDebugger,
    DebugStep,
    ToggleRun,
    ToggleMenu,
}

/// Mirrors the old front-end's defaults, minus the keys that had no binding.
pub fn hotkey(text: &SharedString) -> Option<Hotkey> {
    let named = |k: Key| *text == SharedString::from(k);
    Some(match () {
        _ if named(Key::Shift) => Hotkey::FastForward,
        _ if text.as_str() == "r" || text.as_str() == "R" => Hotkey::Rewind,
        _ if named(Key::F5) => Hotkey::SaveState,
        _ if named(Key::F6) => Hotkey::LoadState,
        _ if named(Key::F12) => Hotkey::ToggleDebugger,
        _ if named(Key::F8) => Hotkey::DebugStep,
        _ if named(Key::F9) => Hotkey::ToggleRun,
        _ if text.as_str() == "m" || text.as_str() == "M" => Hotkey::ToggleMenu,
        _ => return None,
    })
}

/// True when the key does something, so the UI knows whether to consume it.
pub fn is_handled(text: &SharedString) -> bool {
    joypad_bit(text).is_some() || hotkey(text).is_some()
}

#[cfg(test)]
mod hotkey_tests {
    use super::*;

    #[test]
    fn recognises_holds_and_actions() {
        assert_eq!(
            hotkey(&SharedString::from(Key::Shift)),
            Some(Hotkey::FastForward)
        );
        assert_eq!(hotkey(&SharedString::from("r")), Some(Hotkey::Rewind));
        assert_eq!(
            hotkey(&SharedString::from(Key::F5)),
            Some(Hotkey::SaveState)
        );
        assert_eq!(hotkey(&SharedString::from("q")), None);
    }

    #[test]
    fn joypad_and_hotkeys_do_not_overlap() {
        for (key, _) in JOYPAD_KEYS {
            let text = SharedString::from(key);
            assert!(
                hotkey(&text).is_none(),
                "{key:?} is both a joypad key and a hotkey"
            );
            assert!(is_handled(&text));
        }
    }
}
