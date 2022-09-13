pub enum DebuggerCommand {
    Quit,
    Run(Vec<String>),
    Cont,
    Backtrace,
    Breakpoint(BreakPointType),
    Next,
}
pub enum BreakPointType {
    Addr(usize),
    Symbol(String),
    Line(usize),
}

fn parse_address(addr: &str) -> Option<usize> {
    let addr_without_0x = if addr.to_lowercase().starts_with("0x") {
        &addr[2..]
    } else {
        &addr
    };
    usize::from_str_radix(addr_without_0x, 16).ok()
}
impl DebuggerCommand {
    pub fn from_tokens(tokens: &Vec<&str>) -> Option<DebuggerCommand> {
        match tokens[0] {
            "q" | "quit" => Some(DebuggerCommand::Quit),
            "r" | "run" => {
                let args = tokens[1..].to_vec();
                Some(DebuggerCommand::Run(
                    args.iter().map(|s| s.to_string()).collect(),
                ))
            }
            "n" | "next" => Some(DebuggerCommand::Next),

            "c" | "cont" => Some(DebuggerCommand::Cont),
            "bt" | "backtrace" => Some(DebuggerCommand::Backtrace),
            "b" | "break" => {
                let c = if &tokens[1][0..1] == "*" {
                    let addr = parse_address(&tokens[1][1..])?;

                    BreakPointType::Addr(addr)
                } else {
                    let parsed = &tokens[1][..].parse::<usize>();
                    match parsed {
                        Ok(r) => BreakPointType::Line(*r),
                        Err(_) => BreakPointType::Symbol(tokens[1][..].to_owned()),
                    }
                };
                Some(DebuggerCommand::Breakpoint(c))
            }
            _ => None,
        }
    }
}
