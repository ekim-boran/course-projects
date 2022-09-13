extern crate bbs;
extern crate hyper;
extern crate rustc_serialize;

use crate::hyper::body::Buf;
use bbs::Message;
use bbs::{BOT_ADDR, HTML_DATA, HTML_FOOTER, HTML_HEADER, SERVER_ADDR};
use hyper::server::Server;
use hyper::{Method, StatusCode};
use hyper::{Request, Response};
use std::convert::Infallible;
use std::sync::Arc;
use std::sync::Mutex;
use tokio::fs::File;
use tokio::net::TcpStream;

use hyper::service::{make_service_fn, service_fn};
use hyper::Body;
use tokio::io::{self, AsyncReadExt, AsyncWriteExt};
// Returns val from Ok(val) or sets the response to return an InternalServerError.

async fn generate_index(state: Arc<Mutex<Vec<Message>>>) -> Result<Response<Body>, Infallible> {
    let mut header = File::open(HTML_HEADER).await.expect("cannot open header");
    let mut footer = File::open(HTML_FOOTER).await.expect("cannot open footer");
    let mut header_str = vec![];
    header
        .read_to_end(&mut header_str)
        .await
        .expect("file error");

    for message in state.lock().unwrap().iter() {
        header_str.append(&mut format!("{} {}\n", message.user, message.text).into_bytes());
    }

    footer
        .read_to_end(&mut header_str)
        .await
        .expect("file error");

    Ok(Response::new(Body::from(header_str)))
}

async fn hello(
    req: Request<Body>,
    state: Arc<Mutex<Vec<Message>>>,
) -> Result<Response<Body>, Infallible> {
    match req.method() {
        &Method::GET => generate_index(state).await,
        &Method::POST => {
            let whole_body = hyper::body::aggregate(req).await.unwrap();
            // Decode as JSON...
            let params: serde_json::Value = serde_json::from_reader(whole_body.reader()).unwrap();
            let message = Message::new(
                (&params["user"]).as_str().unwrap().to_owned(),
                (&params["text"]).as_str().unwrap().to_owned(),
            );
            let mut stream = TcpStream::connect(&BOT_ADDR).await.unwrap();
            stream.write(message.text.as_bytes()).await.unwrap();
            println!("message {:?}", message);
            state.lock().unwrap().push(message);
            generate_index(state).await
        }
        _ => Ok(Response::new(Body::from("Hello World!"))),
    }
}

#[tokio::main]
pub async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let counter = Arc::new(Mutex::new(vec![]));
    let make_service = make_service_fn(move |_conn| {
        let counter = counter.clone();
        async move {
            Ok::<_, Infallible>(service_fn(move |_req: Request<Body>| {
                let counter = counter.clone();

                async move { Ok::<_, Infallible>(hello(_req, counter).await?) }
            }))
        }
    });
    let addr = SERVER_ADDR.parse().expect("Unable to parse socket address");
    let server = Server::bind(&addr).serve(make_service);
    println!("Listening on http://{}", addr);
    server.await?;
    Ok(())
}
