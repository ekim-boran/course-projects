use core::num;
use std::collections::HashMap;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum Peg {
    A,
    B,
    C,
}

pub type Move = (Peg, Peg);

pub fn hanoi(num_discs: u32, src: Peg, aux: Peg, dst: Peg) -> Vec<Move> {
    if num_discs == 1 {
        return vec![(src, dst)];
    }

    let mut first = hanoi(num_discs - 1, src, dst, aux);
    first.push((src, dst));
    let mut second = hanoi(num_discs - 1, aux, src, dst);
    first.append(&mut second);
    first
}
