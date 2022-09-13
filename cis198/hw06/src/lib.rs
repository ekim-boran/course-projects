extern crate hyper;
extern crate rustc_serialize;

use hyper::body::HttpBody;
use hyper::client::HttpConnector;
use hyper::Body;
use hyper::Uri;
use hyper::{Method, Request};
use rustc_serialize::json;
use rustc_serialize::{Encodable, Encoder};
use std::io::Read;

use hyper::StatusCode;

pub const SERVER_ADDR: &'static str = "127.0.0.1:1980";
pub const BOT_ADDR: &'static str = "127.0.0.1:1981";
pub const HTML_ADDR: &'static str = "http://127.0.0.1:1980";

pub const HTML_DATA: &'static str = "data/index.html";
pub const HTML_HEADER: &'static str = "html/header.html";
pub const HTML_FOOTER: &'static str = "html/footer.html";

#[derive(Debug, serde::Serialize)]
pub struct Message {
    pub user: String,
    pub text: String,
}

impl Message {
    pub fn new(user: String, text: String) -> Message {
        Message {
            text: text,
            user: user,
        }
    }
}

pub struct UserClient {
    username: String,
    server_addr: hyper::Uri,
    client: hyper::Client<HttpConnector>,
}
pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;

impl UserClient {
    pub fn new(username: String, server_addr: hyper::Uri) -> UserClient {
        UserClient {
            username: username,
            server_addr: server_addr,
            client: hyper::Client::new(),
        }
    }

    // TODO: Implement send_msg
    pub async fn send_msg(&self, text: String) -> Result<()> {
        let message = Message::new(self.username.clone(), text.clone());

        let serialized = serde_json::to_string(&message)?;
        let req = Request::builder()
            .method(Method::POST)
            .uri(self.server_addr.clone())
            .header("content-type", "application/json")
            .body(Body::from(serialized))?;
        let mut res = self.client.request(req).await?;

        println!("Response: {}", res.status());
        println!("Headers: {:#?}\n", res.headers());
        if res.status() == StatusCode::OK {
            println!("message sent");
        }
        while let Some(next) = res.data().await {}

        println!("\n\nDone!");

        Ok(())
    }

    //pub async fn get_content(&self) -> hyper::Result<(StatusCode, String)> {
    //    let mut response = self.client.get(self.server_addr).await?;
    //    if let Some(x) = response.body_mut().data().await {
    //        let buf = std::str::from_utf8(&x?[..]).unwrap().to_owned();
    //        return Ok((response.status(), buf));
    //    }
    //    panic!()
    //}
}
