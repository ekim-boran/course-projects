use std::borrow::Borrow;
use std::fs::File;

use crate::debugger_command::{BreakPointType, DebuggerCommand};
use crate::dwarf_data::Error::*;
use crate::dwarf_data::{DwarfData, Line};
use crate::inferior::{Inferior, Status};
use nix::sys::ptrace;
use rustyline::error::ReadlineError;
use rustyline::Editor;

pub struct Debugger {
    target: String,
    history_path: String,
    readline: Editor<()>,
    inferior: Option<Inferior>,
    debug_data: DwarfData,
    breakpoints: Vec<(usize, Option<u8>)>,
}

impl Debugger {
    pub fn new(target: &str) -> Debugger {
        let history_path = format!("{}/.deet_history", std::env::var("HOME").unwrap());
        let mut readline = Editor::<()>::new();
        // Attempt to load history from ~/.deet_history if it exists
        let _ = readline.load_history(&history_path);
        let debug_data = match DwarfData::from_file(target) {
            Ok(val) => val,
            Err(ErrorOpeningFile) => {
                println!("Could not open file {}", target);
                std::process::exit(1);
            }
            Err(DwarfFormatError(err)) => {
                println!("Could not debugging symbols from {}: {:?}", target, err);
                std::process::exit(1);
            }
        };
        Debugger {
            target: target.to_string(),
            history_path,
            readline,
            inferior: None,
            debug_data: debug_data,
            breakpoints: vec![],
        }
    }

    pub fn add_breakpoint(&mut self, addr: usize) {
        self.breakpoints.push((addr, None));
    }

    pub fn handle_breakpoint(&mut self, addr: usize) -> Option<()> {
        let oldbyte = self
            .breakpoints
            .iter_mut()
            .find(|(a, b)| (*a) == addr - 1)?;

        let oldbyte = (&mut oldbyte.1).take()?;
        let inferior = self.inferior.as_mut()?;
        let _ = inferior.write_byte(addr - 1, oldbyte).unwrap();
        let mut regs = ptrace::getregs(inferior.pid()).unwrap();
        regs.rip -= 1;
        ptrace::setregs(inferior.pid(), regs).unwrap();
        ptrace::step(inferior.pid(), None).unwrap();
        Some(())
    }

    pub fn register_breakpoints(&mut self) {
        if let Some(inferior) = &mut self.inferior {
            for (addr, byte) in self.breakpoints.iter_mut() {
                if byte.is_none() {
                    let newbyte = inferior.write_byte(*addr, 0xcc).unwrap();
                    *byte = Some(newbyte);
                }
            }
        } else {
            panic!("x")
        }
    }

    pub fn print_helper(&self, rip: usize) -> (Line, String) {
        let line = DwarfData::get_line_from_addr(&self.debug_data, rip).unwrap_or(Line {
            file: String::new(),
            number: 0,
            address: 0,
        });
        let func =
            DwarfData::get_function_from_addr(&self.debug_data, rip).unwrap_or(String::new());
        (line, func)
    }

    //pub fn print_current_line(&self) {
    //    let rip = self.inferior.as_ref().unwrap().get_current_rip();
    //    let (line, func) = self.print_helper(rip);
    //    println!("{} {}", line, func);
    //}

    pub fn print_line_func(&self) {
        let rip = self.inferior.as_ref().unwrap().get_current_rip();
        let (line, func) = self.print_helper(rip);
        println!("{} {}", line, func);
    }
    pub fn process_break(&mut self, status: &Status) {
        match status {
            Status::Stopped(nix::sys::signal::Signal::SIGTRAP, addr) => {
                self.handle_breakpoint(*addr);
            }
            _ => {}
        }
    }
    pub fn process_status(&mut self, status: &Status) {
        match status {
            Status::Stopped(nix::sys::signal::Signal::SIGTRAP, addr) => {
                print!("break point at ");
                self.print_line_func();
            }
            Status::Stopped(s, _) => {
                print!("Stopped with signal {} at ", s);
                self.print_line_func()
            }
            Status::Signaled(s) => {
                print!("Signalled with signal {} at ", s);
                self.print_line_func()
            }
            Status::Exited(_) => {}
        }
    }

    pub fn print_backtrace(&self) -> Result<(), nix::Error> {
        if let Some(inferior) = &self.inferior {
            let regs = ptrace::getregs(inferior.pid())?;
            let mut rip = regs.rip as usize;
            let mut rbp = regs.rbp as usize;

            loop {
                let (line, func) = self.print_helper(rip);
                println!("{} {}", &line, func);
                if func == "main" {
                    break;
                }
                rip = ptrace::read(inferior.pid(), (rbp + 8) as ptrace::AddressType)? as usize;
                rbp = ptrace::read(inferior.pid(), rbp as ptrace::AddressType)? as usize;
            }
        }
        Ok(())
    }

    pub fn cont(&mut self) {
        if self.inferior.is_none() {
            return;
        }
        self.register_breakpoints();
        if let Some(inferior) = &mut self.inferior {
            inferior.cont().unwrap();
            let status = inferior.wait(None).unwrap();
            self.process_status(&status);
            self.process_break(&status)
        }
    }
    pub fn kill(&mut self) {
        if let Some(inferior) = &mut self.inferior {
            inferior.kill().unwrap();
            self.inferior = None;
        }
    }
    pub fn next(&mut self) {
        let rip = self.inferior.as_mut().unwrap().get_current_rip();
        let (line, _) = self.print_helper(rip);
        let mut current_line = line.number;
        loop {
            if line.number == current_line {
                self.register_breakpoints();
                ptrace::step(self.inferior.as_mut().unwrap().pid(), None).unwrap();
                let status = self.inferior.as_mut().unwrap().wait(None).unwrap();
                self.process_break(&status);
                let rip = self.inferior.as_mut().unwrap().get_current_rip();
                let (line, _) = self.print_helper(rip);
                current_line = line.number;
            } else {
                self.print_line_func();
                break;
            }
        }
    }
    pub fn run(&mut self) {
        loop {
            match self.get_next_command() {
                DebuggerCommand::Run(args) => {
                    self.kill();
                    if let Some(inferior) = Inferior::new(&self.target, &args) {
                        self.inferior = Some(inferior);
                        self.cont();
                    } else {
                        println!("Error starting subprocess");
                    }
                }
                DebuggerCommand::Quit => {
                    self.kill();
                    return;
                }
                DebuggerCommand::Cont => self.cont(),
                DebuggerCommand::Next => self.next(),

                DebuggerCommand::Backtrace => self.print_backtrace().unwrap(),
                DebuggerCommand::Breakpoint(btype) => {
                    let addr = match btype {
                        BreakPointType::Line(l) => {
                            DwarfData::get_addr_for_line(&self.debug_data, None, l)
                        }
                        BreakPointType::Addr(a) => Some(a),
                        BreakPointType::Symbol(s) => {
                            DwarfData::get_addr_for_function(&self.debug_data, None, &s)
                        }
                    };
                    if let Some(addr) = addr {
                        self.add_breakpoint(addr)
                    }
                }
            }
        }
    }

    /// This function prompts the user to enter a command, and continues re-prompting until the user
    /// enters a valid command. It uses DebuggerCommand::from_tokens to do the command parsing.
    ///
    /// You don't need to read, understand, or modify this function.
    fn get_next_command(&mut self) -> DebuggerCommand {
        loop {
            // Print prompt and get next line of user input
            match self.readline.readline("(deet) ") {
                Err(ReadlineError::Interrupted) => {
                    // User pressed ctrl+c. We're going to ignore it
                    println!("Type \"quit\" to exit");
                }
                Err(ReadlineError::Eof) => {
                    // User pressed ctrl+d, which is the equivalent of "quit" for our purposes
                    return DebuggerCommand::Quit;
                }
                Err(err) => {
                    panic!("Unexpected I/O error: {:?}", err);
                }
                Ok(line) => {
                    if line.trim().len() == 0 {
                        continue;
                    }
                    self.readline.add_history_entry(line.as_str());
                    if let Err(err) = self.readline.save_history(&self.history_path) {
                        println!(
                            "Warning: failed to save history file at {}: {}",
                            self.history_path, err
                        );
                    }
                    let tokens: Vec<&str> = line.split_whitespace().collect();
                    if let Some(cmd) = DebuggerCommand::from_tokens(&tokens) {
                        return cmd;
                    } else {
                        println!("Unrecognized command.");
                    }
                }
            }
        }
    }
}
