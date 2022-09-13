extern crate bbs;
extern crate hyper;
extern crate rustc_serialize;

use bbs::Result;
use bbs::{UserClient, HTML_ADDR};
use hyper::{body::HttpBody as _, Client};
use hyper::{Body, StatusCode};
use hyper::{Method, Request};
use std::env::args;
use std::io::stdin;
use tokio::io;
use tokio::io::AsyncWriteExt;

#[tokio::main]
async fn main() -> Result<()> {
    let url = HTML_ADDR.parse::<hyper::Uri>().unwrap();
    if url.scheme_str() != Some("http") {
        println!("This example only works with 'http' URLs.");
        return Ok(());
    }
    let mut name = String::new();
    println!("enter your name");
    stdin().read_line(&mut name).unwrap();
    let mut client = UserClient::new(name, url);
    loop {
        println!("enter your message (type quit to quit)");
        let mut text = String::new();
        stdin().read_line(&mut text).unwrap();
        if text == "quit" {
            return Ok(());
        }
        client.send_msg(text.clone()).await?;
        text.clear();
    }
}
