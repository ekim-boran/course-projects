use std::clone;
use std::fmt;
use std::option::Option;

pub struct LinkedList<T> {
    head: Option<Box<Node<T>>>,
    size: usize,
}

struct Node<T> {
    value: T,
    next: Option<Box<Node<T>>>,
}

impl<T> Node<T> {
    pub fn new(value: T, next: Option<Box<Node<T>>>) -> Node<T> {
        Node {
            value: value,
            next: next,
        }
    }
}

impl<T> LinkedList<T> {
    pub fn new() -> LinkedList<T> {
        LinkedList {
            head: None,
            size: 0,
        }
    }

    pub fn get_size(&self) -> usize {
        self.size
    }

    pub fn is_empty(&self) -> bool {
        self.get_size() == 0
    }

    pub fn push_front(&mut self, value: T) {
        let new_node = Box::new(Node::new(value, self.head.take()));
        self.head = Some(new_node);
        self.size += 1;
    }

    pub fn pop_front(&mut self) -> Option<T> {
        let node = self.head.take()?;
        self.head = node.next;
        self.size -= 1;
        Some(node.value)
    }
}

impl<T: fmt::Display> fmt::Display for LinkedList<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut current = &self.head;
        let mut result = String::new();
        loop {
            match current {
                Some(node) => {
                    result = format!("{} {}", result, node.value);
                    current = &node.next;
                }
                None => break,
            }
        }
        write!(f, "{}", result)
    }
}

impl<T> Drop for LinkedList<T> {
    fn drop(&mut self) {
        let mut current = self.head.take();
        while let Some(mut node) = current {
            current = node.next.take();
        }
    }
}
impl<T: Clone> Clone for LinkedList<T> {
    fn clone(&self) -> Self {
        LinkedList {
            size: self.size,
            head: self.head.clone(),
        }
    }
}
impl<T: Clone> Clone for Node<T> {
    fn clone(&self) -> Self {
        Node {
            value: self.value.clone(),
            next: self.next.clone(),
        }
    }
}

impl<T: PartialEq> PartialEq for LinkedList<T> {
    fn eq(&self, other: &LinkedList<T>) -> bool {
        other.size == self.size && self.head == other.head
    }
}
impl<T: PartialEq> PartialEq for Node<T> {
    fn eq(&self, other: &Node<T>) -> bool {
        other.value == self.value && other.next == self.next
    }
}

impl<'a, T> IntoIterator for &'a LinkedList<T> {
    type Item = &'a T;

    type IntoIter = LinkedListIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        LinkedListIter {
            ptr: self.head.as_deref(),
        }
    }
}
pub struct LinkedListIter<'a, T> {
    ptr: Option<&'a Node<T>>,
}
impl<'a, T> Iterator for LinkedListIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let p = self.ptr.take()?;
        self.ptr = p.next.as_deref();
        Some(&p.value)
    }
}
pub trait ComputeNorm {
    fn compute_norm(&self) -> f64 {
        0.0
    }
}

impl ComputeNorm for LinkedList<f64> {
    fn compute_norm(&self) -> f64 {
        // an example of functional rust syntax
        self.into_iter().map(|x| x * x).sum::<f64>().sqrt()
    }
}
