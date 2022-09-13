// TODO: everything
#[derive(Debug)]
pub struct BTree<T> {
    root: Link<T>,
    size: usize,
}
#[derive(Debug)]
pub enum Link<T> {
    Empty,
    More(Box<Node<T>>),
}
#[derive(Debug)]
pub struct Node<T> {
    elem: T,
    left: BTree<T>,
    right: BTree<T>,
}
impl<T> BTree<T> {
    pub fn new() -> Self {
        BTree {
            root: Link::Empty,
            size: 0,
        }
    }
    pub fn size(&self) -> usize {
        self.size
    }
}

impl<T: Ord> BTree<T> {
    pub fn insert(&mut self, item: T) -> bool {
        let mut root = &mut self.root;
        loop {
            let a = root;
            match *a {
                Link::Empty => {
                    *a = Link::More(Box::new(Node {
                        elem: item,
                        left: BTree::new(),
                        right: BTree::new(),
                    }));
                    self.size += 1;
                    return true;
                }
                Link::More(ref mut x) => {
                    if x.elem > item {
                        root = &mut x.left.root
                    } else if x.elem < item {
                        root = &mut x.right.root
                    } else {
                        return false;
                    }
                }
            }
        }
    }
    pub fn search(&self, item: T) -> bool {
        let mut root = &self.root;
        loop {
            match root {
                Link::Empty => {
                    return false;
                }
                Link::More(x) => {
                    if x.elem > item {
                        root = &x.left.root
                    } else if x.elem < item {
                        root = &x.right.root
                    } else {
                        return true;
                    }
                }
            }
        }
    } //

    pub fn iter<'a>(&'a self) -> IterRef<'a, T> {
        self.into_iter()
    }
}

fn add<T>(mut btree: BTree<T>, vec: &mut Iter<T>) {
    loop {
        match btree.root {
            Link::Empty => return,
            Link::More(mut x) => {
                let left = std::mem::replace(&mut x.left, BTree::new());
                vec.elems.push(x);
                btree = left;
            }
        }
    }
}
fn add_ref<'a, T>(mut btree: &'a BTree<T>, vec: &mut IterRef<'a, T>) {
    loop {
        match &btree.root {
            Link::Empty => return,
            Link::More(ref x) => {
                let left = &x.left;
                vec.elems.push(x.as_ref());
                btree = left;
            }
        }
    }
}
pub struct Iter<T> {
    elems: Vec<Box<Node<T>>>,
}

impl<T: Ord> IntoIterator for BTree<T> {
    type Item = T;

    type IntoIter = Iter<T>;

    fn into_iter(self) -> Self::IntoIter {
        let mut elems = Iter { elems: vec![] };
        add(self, &mut elems);
        return elems;
    }
}
impl<T> Iterator for Iter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let Node {
            elem,
            left: _,
            right,
        } = *(self.elems.pop()?);
        add(right, self);
        Some(elem)
    }
}

pub struct IterRef<'a, T> {
    elems: Vec<&'a Node<T>>,
}

impl<'a, T> IntoIterator for &'a BTree<T> {
    type Item = &'a T;

    type IntoIter = IterRef<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        let mut elems = IterRef { elems: vec![] };
        add_ref(self, &mut elems);
        return elems;
    }
}
impl<'a, T> Iterator for IterRef<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let Node {
            ref elem,
            left: _,
            ref right,
        } = *(self.elems.pop()?);
        add_ref(right, self);
        Some(elem)
    }
}
