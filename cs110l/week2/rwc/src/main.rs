use std::env;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::process;

// char counts non whitespace characters
// words are seperated by whitespace characters

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Too few arguments.");
        process::exit(1);
    }
    let filename = &args[1];
    let file = File::open(filename)?;
    let b = BufReader::new(file);
    let (mut char_count, mut word_count, mut line_count) = (0, 0, 0);
    for i in b.lines() {
        let line = i?;
        line_count += 1;
        let (w, c) = line
            .split_ascii_whitespace()
            .fold((0, 0), |(w, c), s| (w + 1, c + s.len()));
        word_count += w;
        char_count += c;
    }
    println!("{} {} {}", line_count, word_count, char_count);

    Ok(())
}
