extern crate bbs;
extern crate hyper;
extern crate rand;

use crate::rand::SeedableRng;
use rand::{thread_rng, Rng};
use std::sync::Arc;
use std::sync::Mutex;
use tokio::io::AsyncBufReadExt;
use tokio::io::AsyncReadExt;
use tokio::io::BufReader;
use tokio::net::unix::SocketAddr;
use tokio::net::TcpListener;
use tokio::net::TcpStream;

use bbs::UserClient;
use bbs::{BOT_ADDR, HTML_ADDR};

#[tokio::main]
async fn main() -> bbs::Result<()> {
    let listener = TcpListener::bind(&BOT_ADDR).await?;
    let client = UserClient::new("bot".to_string(), HTML_ADDR.parse::<hyper::Uri>().unwrap());
    let client = Arc::new(client);
    loop {
        let (stream, addr) = listener.accept().await?;
        let client = client.clone();
        tokio::spawn(async move { if let Err(e) = process(client, stream, addr).await {} });
    }
}
async fn process(
    client: Arc<UserClient>,
    stream: TcpStream,
    addr: std::net::SocketAddr,
) -> bbs::Result<()> {
    let mut stream = BufReader::new(stream);
    let mut line = String::new();
    stream.read_line(&mut line).await.unwrap();
    let input: Vec<_> = line.split(" ").collect();
    println!("x");
    println!("{}", input[0]);

    if input.len() != 0 && input[0] == "choose" {
        let mut rng = rand::rngs::StdRng::seed_from_u64(42);
        let n1 = rng.gen_range(0..(input.len() - 2));
        client.send_msg(input[n1].to_string()).await?;
    }
    Ok(())
}
