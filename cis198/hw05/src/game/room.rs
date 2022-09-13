use std::cell::RefCell;
use std::fmt::format;
use std::mem;
use std::rc::Rc;

use super::curio::Curio;
use super::hall::Hall;

pub struct Room {
    pub name: String,
    pub contents: Vec<Curio>,
    pub halls: Vec<Rc<Hall>>,
    pub wumpus: bool,
}

impl PartialEq for Room {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for Room {}

impl Room {
 
    pub fn neighbors_string(&self) -> String {
        let mut s = String::new();
        for hall in self.halls.iter() {
            let room = hall.other(self);
            s = s + &format!("{:?}\n", room.borrow().name)
        }
        s
    }
}
