use async_std::prelude::FutureExt;
use async_std::task;

extern crate tide;

mod chatserver;
mod webpage;

fn main() {
    let both = async {
        let k = task::spawn(chatserver::start());
        let l = task::spawn(webpage::serve());
        k.race(l).await
    };
    let t = task::block_on(both);
    println!("{:?}", t)
}
