// Simple Hangman Program
// User gets five incorrect guesses
// Word chosen randomly from words.txt
// Inspiration from: https://doc.rust-lang.org/book/ch02-00-guessing-game-tutorial.html
// This assignment will introduce you to some fundamental syntax in Rust:
// - variable declaration
// - string manipulation
// - conditional statements
// - loops
// - vectors
// - files
// - user input
// We've tried to limit/hide Rust's quirks since we'll discuss those details
// more in depth in the coming lectures.
extern crate rand;
use rand::Rng;
use std::collections::HashSet;
use std::fs;
use std::io;
use std::io::Read;
use std::io::Write;

const NUM_INCORRECT_GUESSES: u32 = 5;
const WORDS_PATH: &str = "words.txt";

fn pick_a_random_word() -> String {
    let file_string = fs::read_to_string(WORDS_PATH).expect("Unable to read file.");
    let words: Vec<&str> = file_string.split('\n').collect();
    String::from(words[rand::thread_rng().gen_range(0, words.len())].trim())
}

struct Hangman {
    chars: Vec<char>,
    picked_chars: HashSet<char>,
    guesses_left: u32,
}

#[derive(Eq, PartialEq)]
pub enum GameResult {
    Win,
    Lose,
    Continue,
}

pub enum PlayResult {
    AlreadyGuessed,
    Right,
    Wrong,
}

impl Hangman {
    fn new(chars: Vec<char>, guesses_left: u32) -> Self {
        Self {
            chars,
            picked_chars: HashSet::new(),
            guesses_left,
        }
    }
    fn play(&mut self, c: char) -> PlayResult {
        if self.picked_chars.contains(&c) {
            PlayResult::AlreadyGuessed
        } else {
            self.picked_chars.insert(c);
            if !self.chars.contains(&c) {
                self.guesses_left -= 1;
                PlayResult::Wrong
            } else {
                PlayResult::Right
            }
        }
    }

    fn winner(&self) -> GameResult {
        if self.guesses_left == 0 {
            GameResult::Lose
        } else if self.chars.iter().all(|x| self.picked_chars.contains(x)) {
            GameResult::Win
        } else {
            GameResult::Continue
        }
    }
    fn print(&self) {
        print!("The word so far is:   ");
        for i in self.chars.iter() {
            if self.picked_chars.contains(&i) {
                print!("{}", i);
            } else {
                print!("-");
            }
        }
        println!("");
        print!("You have guessed following letters: ");
        for i in self.picked_chars.iter() {
            print!("{} ", i);
        }
        println!("");
        println!("You have {} guesses left", self.guesses_left);
    }
}

fn read_a_letter(buffer: &mut String) -> Option<char> {
    io::stdin().read_line(buffer).ok()?;
    let guess = buffer.trim();
    let c = guess.chars().next();

    buffer.clear();
    c
}

fn main() {
    let secret_word = pick_a_random_word();
    // Note: given what you know about Rust so far, it's easier to pull characters out of a
    // vector than it is to pull them out of a string. You can get the ith character of
    // secret_word by doing secret_word_chars[i].
    let secret_word_chars: Vec<char> = secret_word.chars().collect();
    //println!("random word: {}", secret_word);

    let mut hangman = Hangman::new(secret_word_chars, NUM_INCORRECT_GUESSES);
    let mut buffer = String::new();

    loop {
        let result = hangman.winner();
        match result {
            GameResult::Win => {
                println!("You won!");
                break;
            }
            GameResult::Lose => {
                println!("You lose the word was {:?}!", (hangman.chars.iter()));
                break;
            }
            GameResult::Continue => {}
        }
        hangman.print();

        print!("Please guess a letter ");
        io::stdout().flush().expect("Error flushing stdout.");
        let x = match read_a_letter(&mut buffer) {
            Some(x) => x,
            None => {
                println!("not a valid letter");
                continue;
            }
        };
        println!("{}", x);
        let result = hangman.play(x);

        match result {
            PlayResult::AlreadyGuessed => println!("You already picked it"),
            PlayResult::Right => println!("You got it right"),
            PlayResult::Wrong => println!("Sorry, that letter is not in the word"),
        }
    }

    // Your code here! :)
}
