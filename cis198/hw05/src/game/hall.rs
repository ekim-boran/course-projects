use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

use super::room::Room;

pub struct Hall {
    pub left: Rc<RefCell<Room>>,
    pub right: Rc<RefCell<Room>>,
}

impl Hall {
    pub fn new(left: Rc<RefCell<Room>>, right: Rc<RefCell<Room>>) -> Hall {
        Hall { left, right }
    }

    /// Given a Room `room`, find the room at the other end of Hall `self`.
    pub fn other(&self, room: &Room) -> Rc<RefCell<Room>> {
        if self.left.borrow().deref() == room {
            self.right.clone()
        } else {
            self.left.clone()
        }
    }
}
