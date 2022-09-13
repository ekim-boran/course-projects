use core::panic;
use crossbeam_channel;
use std::{sync::mpsc::channel, thread, time};

fn parallel_map<T, U, F>(mut input_vec: Vec<T>, num_threads: usize, f: F) -> Vec<U>
where
    F: FnOnce(T) -> U + Send + Copy + 'static,
    T: Send + 'static,
    U: Send + 'static + Default,
{
    let mut output_vec: Vec<U> = Vec::with_capacity(input_vec.len());
    for i in 0..input_vec.len() {
        output_vec.push(U::default())
    }

    let (sender, receiver) = crossbeam_channel::unbounded();
    let (outsender, outreceiver) = crossbeam_channel::unbounded();

    let mut jhs = vec![];

    let producer = std::thread::spawn(move || {
        for x in input_vec.into_iter().enumerate() {
            sender.send(x).unwrap()
        }
    });

    for i in 0..num_threads {
        let receiver = receiver.clone();
        let outsender = outsender.clone();

        let jh = thread::spawn(move || loop {
            let r = receiver.recv();
            match r {
                Ok((i, item)) => outsender.send((i, f(item))).unwrap(),
                Err(x) => return,
            }
        });
        jhs.push(jh)
    }

    for _ in 0..output_vec.len() {
        let (j, item) = outreceiver.recv().unwrap();
        output_vec[j] = item;
    }

    producer.join().unwrap();
    for jh in jhs.into_iter() {
        jh.join().unwrap()
    }

    output_vec
}

fn main() {
    let v = vec![6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 12, 18, 11, 5, 20];
    let squares = parallel_map(v, 10, |num| {
        println!("{} squared is {}", num, num * num);
        thread::sleep(time::Duration::from_millis(500));
        num * num
    });
    println!("squares: {:?}", squares);
}
