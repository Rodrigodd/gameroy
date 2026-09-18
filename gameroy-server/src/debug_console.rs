//! The debugger console's command language.
//!
//! Reimplemented rather than delegated to `gameroy::debugger::Debugger::execute_command`,
//! for two reasons. That method writes files (`dump <path>`, `save <dir>`), which
//! this crate must not do — the front-end owns the filesystem, and a web worker has
//! none. And it drives a `Box<dyn FnMut>` callback stored *inside* the struct it
//! mutates, which is why every mutator in core does a `take()`/restore dance; here
//! a command simply returns what changed and the session emits an event.
//!
//! The core's structured API (`add_break`, `add_watch`, `run_until`, …) is reused
//! as-is. Only the parsing is ours.

use gameroy::debugger::{Debugger, break_flags};

/// What the session should do after a console command.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct CommandOutcome {
    /// Text for the console.
    pub message: Option<String>,
    pub breakpoints_changed: bool,
    pub watches_changed: bool,
    pub action: Option<ConsoleAction>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConsoleAction {
    Step,
    StepBack,
    Run,
    Reset,
    RunToAddress(u16),
    RunForClocks(u64),
    RunUntilClock(u64),
}

impl CommandOutcome {
    fn message(text: impl Into<String>) -> Self {
        Self {
            message: Some(text.into()),
            ..Self::default()
        }
    }

    fn action(action: ConsoleAction) -> Self {
        Self {
            action: Some(action),
            ..Self::default()
        }
    }
}

/// Parses and applies one console line.
///
/// `Err` carries a message to show the user; it is never fatal.
pub fn execute(debugger: &mut Debugger, line: &str) -> Result<CommandOutcome, String> {
    let args: Vec<&str> = line.split_whitespace().collect();
    match args.as_slice() {
        // Enter on an empty line steps, as the old console did.
        [] | ["step"] => Ok(CommandOutcome::action(ConsoleAction::Step)),
        ["stepback"] => Ok(CommandOutcome::action(ConsoleAction::StepBack)),
        ["reset"] => Ok(CommandOutcome::action(ConsoleAction::Reset)),
        ["run"] => Ok(CommandOutcome::action(ConsoleAction::Run)),
        ["run", "for", n] => {
            parse_u64(n).map(|n| CommandOutcome::action(ConsoleAction::RunForClocks(n)))
        }
        ["run", "until", n] => {
            parse_u64(n).map(|n| CommandOutcome::action(ConsoleAction::RunUntilClock(n)))
        }
        ["runto", addr] => {
            parse_u16(addr).map(|a| CommandOutcome::action(ConsoleAction::RunToAddress(a)))
        }
        ["break", flags, addr] => {
            let flags = parse_flags(flags)?;
            let address = parse_u16(addr)?;
            debugger.add_break(flags, address);
            Ok(CommandOutcome {
                message: Some(format!("breakpoint at {address:04x}")),
                breakpoints_changed: true,
                ..CommandOutcome::default()
            })
        }
        ["unbreak", addr] | ["delete", addr] => {
            let address = parse_u16(addr)?;
            debugger.remove_break(address);
            Ok(CommandOutcome {
                message: Some(format!("removed breakpoint at {address:04x}")),
                breakpoints_changed: true,
                ..CommandOutcome::default()
            })
        }
        ["watch", addr] => {
            let address = parse_u16(addr)?;
            debugger.add_watch(address);
            Ok(CommandOutcome {
                message: Some(format!("watching {address:04x}")),
                watches_changed: true,
                ..CommandOutcome::default()
            })
        }
        ["unwatch", addr] => {
            let address = parse_u16(addr)?;
            debugger.remove_watch(address);
            Ok(CommandOutcome {
                message: Some(format!("stopped watching {address:04x}")),
                watches_changed: true,
                ..CommandOutcome::default()
            })
        }
        ["echo", rest @ ..] => Ok(CommandOutcome::message(rest.join(" "))),
        ["help"] => Ok(CommandOutcome::message(HELP)),
        [cmd, ..] => Err(format!("unknown command {cmd:?}; try `help`")),
    }
}

const HELP: &str = "\
step                  run one instruction (also: empty line)
stepback              step one instruction backwards
run                   continue until a breakpoint
run for <clocks>      run for N clocks
run until <clock>     run until the clock reaches N
runto <addr>          run until PC reaches addr (hex)
break <flags> <addr>  break on addr; flags from w r x j
unbreak <addr>        remove a breakpoint
watch <addr>          show the value at addr
unwatch <addr>        stop watching addr
reset                 reset the Game Boy
echo <text>           print text";

fn parse_u16(s: &str) -> Result<u16, String> {
    u16::from_str_radix(s.trim_start_matches("0x"), 16)
        .map_err(|_| format!("{s:?} is not a 16-bit hex address"))
}

fn parse_u64(s: &str) -> Result<u64, String> {
    // Decimal here: clock counts are naturally read as decimal, and that is how
    // the old console took them.
    s.parse::<u64>()
        .map_err(|_| format!("{s:?} is not a number"))
}

fn parse_flags(s: &str) -> Result<u8, String> {
    let mut flags = 0;
    for c in s.chars() {
        flags |= match c {
            'w' => break_flags::WRITE,
            'r' => break_flags::READ,
            'x' => break_flags::EXECUTE,
            'j' => break_flags::JUMP,
            other => return Err(format!("unknown break flag {other:?}; use w, r, x or j")),
        };
    }
    if flags == 0 {
        return Err("give at least one flag from w, r, x, j".into());
    }
    Ok(flags)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn debugger() -> Debugger {
        Debugger::default()
    }

    #[test]
    fn parses_run_control() {
        let mut d = debugger();
        assert_eq!(
            execute(&mut d, "").unwrap().action,
            Some(ConsoleAction::Step)
        );
        assert_eq!(
            execute(&mut d, "step").unwrap().action,
            Some(ConsoleAction::Step)
        );
        assert_eq!(
            execute(&mut d, "runto 0150").unwrap().action,
            Some(ConsoleAction::RunToAddress(0x150))
        );
        assert_eq!(
            execute(&mut d, "run for 400").unwrap().action,
            Some(ConsoleAction::RunForClocks(400))
        );
    }

    #[test]
    fn adds_and_removes_breakpoints() {
        let mut d = debugger();
        let out = execute(&mut d, "break rw ff45").unwrap();
        assert!(out.breakpoints_changed);
        assert_eq!(
            d.breakpoints().get(&0xff45).copied(),
            Some(break_flags::READ | break_flags::WRITE)
        );

        execute(&mut d, "unbreak ff45").unwrap();
        assert!(d.breakpoints().get(&0xff45).is_none());
    }

    #[test]
    fn adds_and_removes_watches() {
        let mut d = debugger();
        assert!(execute(&mut d, "watch ff05").unwrap().watches_changed);
        assert!(d.watchs().contains(&0xff05));
        assert!(execute(&mut d, "unwatch ff05").unwrap().watches_changed);
        assert!(!d.watchs().contains(&0xff05));
    }

    #[test]
    fn reports_errors_without_panicking() {
        let mut d = debugger();
        assert!(execute(&mut d, "brk 0150").is_err());
        assert!(execute(&mut d, "break zz 0150").is_err());
        assert!(execute(&mut d, "break x nothex").is_err());
        assert!(execute(&mut d, "watch").is_err());
        // The session must survive all of that.
        assert!(execute(&mut d, "step").is_ok());
    }
}
