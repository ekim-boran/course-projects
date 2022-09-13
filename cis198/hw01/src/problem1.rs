use core::ops::Add;

pub fn dedup<T: Copy + Eq>(xs: &[T]) -> Vec<T> {
    let mut r = vec![];
    for x in xs {
        if !r.contains(x) {
            r.push(*x);
        }
    }
    r
}
pub fn sum<'a, T: Add<Output = T> + Default + Copy>(xs: &'a [T]) -> T {
    let mut r = T::default();
    for x in xs {
        r = r + *x;
    }
    r
}
pub fn filter<T: Copy, F: Fn(T) -> bool>(xs: &[T], f: F) -> Vec<T> {
    let mut r = vec![];
    for x in xs {
        if f(*x) {
            r.push(*x)
        }
    }
    r
}
