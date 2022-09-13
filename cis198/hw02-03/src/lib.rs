#![cfg_attr(feature = "clippy", feature(plugin))]

use first::BTree;

pub mod first;

#[test]
fn insert_search1() {
    let mut btree = BTree::new();
    for i in 1..100 {
        btree.insert(i);
    }
    for i in (1..100).rev() {
        assert_eq!(true, btree.search(i))
    }
    for i in 400..500 {
        assert_eq!(false, btree.search(i))
    }
    //already inserted
    for i in 1..100 {
        assert_eq!(false, btree.insert(i))
    }
    assert_eq!(99, btree.size())
}

#[test]

fn iterate() {
    let mut btree = BTree::new();
    for i in 1..100 {
        btree.insert(i);
    }
    for x in btree {
        println!("{}", x);
    }
    //for x in btree {
    //    println!("{}", x);
    //}
}
#[test]
fn iterate_ref() {
    let mut btree = BTree::new();
    for i in 1..100 {
        btree.insert(i);
    }
    let mut start = 1;
    for x in btree.iter() {
        assert_eq!(start, *x);
        start += 1;
    }
    start = 1;
    for x in btree.iter() {
        assert_eq!(start, *x);
        start += 1;
    }
    assert_eq!(99, btree.size());
}
