mod request;
mod response;

use async_std::net::TcpListener;
use async_std::net::TcpStream;
use async_std::prelude::FutureExt;
use async_std::stream;
use async_std::stream::StreamExt;
use async_std::sync::Arc;
use async_std::sync::Mutex;
use async_std::sync::RwLock;
use async_std::task;
use clap::Clap;
use core::time::Duration;
use http::StatusCode;
use rand::{Rng, SeedableRng};
use std::collections::HashMap;
use std::collections::HashSet;

/// Contains information parsed from the command-line invocation of balancebeam. The Clap macros
/// provide a fancy way to automatically construct a command-line argument parser.
#[derive(Clap, Debug)]
#[clap(about = "Fun with load balancing")]
struct CmdOptions {
    #[clap(
        short,
        long,
        about = "IP/port to bind to",
        default_value = "0.0.0.0:1100"
    )]
    bind: String,
    #[clap(short, long, about = "Upstream host to forward requests to")]
    upstream: Vec<String>,
    #[clap(
        long,
        about = "Perform active health checks on this interval (in seconds)",
        default_value = "10"
    )]
    active_health_check_interval: usize,
    #[clap(
        long,
        about = "Path to send request to for active health checks",
        default_value = "/"
    )]
    active_health_check_path: String,
    #[clap(
        long,
        about = "Maximum number of requests to accept per IP per minute (0 = unlimited)",
        default_value = "0"
    )]
    max_requests_per_minute: usize,
}

struct RateLimiterTable {
    start: std::time::SystemTime,
    end: Option<std::time::SystemTime>,
    table: HashMap<String, u64>,
}
impl RateLimiterTable {
    fn get(&self, addr: &str) -> (f64, f64) {
        let x = match self.table.get(addr) {
            Some(x) => x,
            None => return (0f64, 1f64),
        };
        let end = self.end.unwrap_or(std::time::SystemTime::now());
        let duration = end.duration_since(self.start).unwrap();
        let dur = duration.as_secs_f64() as f64;
        println!("dur {}", dur);
        if dur == 0f64 {
            return (0f64, 0f64);
        }
        let per = if dur / 60f64 < 1f64 {
            dur / 60f64
        } else {
            1f64
        };
        ((*x as f64) / dur, per)
    }
}

struct RateLimiter {
    old: RateLimiterTable,
    current: RateLimiterTable,
}
impl RateLimiter {
    fn new() -> Self {
        let current_time = std::time::SystemTime::now();

        let old = RateLimiterTable {
            start: current_time,
            end: Some(current_time),
            table: HashMap::new(),
        };
        let new = RateLimiterTable {
            start: current_time,
            end: None,
            table: HashMap::new(),
        };
        RateLimiter {
            old: old,
            current: new,
        }
    }

    fn get(&self, addr: &str) -> usize {
        let (old_rate, _) = self.old.get(addr);
        let (new_rate, percentage) = self.current.get(addr);
        println!("{:?} {:?} {:?}", old_rate, new_rate, percentage);
        let calculated_rate = (new_rate) + (old_rate * 60f64 * (1f64 - percentage));
        calculated_rate as usize
    }
    fn put(&mut self, addr: &str) {
        *self.current.table.entry(addr.to_string()).or_insert(0) += 1;
    }

    fn swap(&mut self) {
        let current_time = std::time::SystemTime::now();
        self.current.end = Some(current_time);
        std::mem::swap(&mut self.current, &mut self.old);
        self.current.end = None;
        self.current.start = current_time;
        self.current.table.clear();
    }
}

/// Contains information about the state of balancebeam (e.g. what servers we are currently proxying
/// to, what servers have failed, rate limiting counts, etc.)
///
/// You should add fields to this struct in later milestones.
///
///

struct ProxyState {
    /// How frequently we check whether upstream servers are alive (Milestone 4)
    active_health_check_interval: usize,
    /// Where we should send requests when doing active health checks (Milestone 4)
    active_health_check_path: String,
    /// Maximum number of requests an individual IP can make in a minute (Milestone 5)
    max_requests_per_minute: usize,
    /// Addresses of servers that we are proxying to
    upstream_addresses: RwLock<(Vec<String>, HashSet<String>)>, // active - deleted

    rate_limiter: Mutex<RateLimiter>,
}

impl ProxyState {
    async fn remove_address(&self, addr: String) {
        let mut guard = self.upstream_addresses.write().await;
        guard.0.retain(|x| x != &addr);
        guard.1.insert(addr);
    }
    async fn add_address(&self, addr: String) {
        let mut guard = self.upstream_addresses.write().await;
        guard.1.remove(&addr);
        guard.0.push(addr);
    }
}

fn main() {
    task::block_on(main_async());
}

async fn check(addr: String, path: String) -> Result<(), std::io::Error> {
    let mut stream = connect_to_upstream_server(&addr).await?;
    let request = http::Request::builder()
        .method(http::Method::GET)
        .uri(path)
        .header("Host", addr)
        .body(Vec::<u8>::new())
        .unwrap();
    request::write_to_stream(&request, &mut stream).await?;
    let response = response::read_from_stream(&mut stream, request.method())
        .await
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, "response error"))?;
    if response.status() == StatusCode::OK {
        Ok(())
    } else {
        Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            "all addresses are down",
        ))
    }
}

async fn active_health_check(state: Arc<ProxyState>) {
    let mut interval = stream::interval(Duration::from_secs(
        state.active_health_check_interval as u64,
    ));
    while let Some(_) = interval.next().await {
        let (active, removed) = state.upstream_addresses.write().await.clone();

        for addr in active {
            match check(addr.clone(), state.active_health_check_path.clone()).await {
                Ok(_) => {}
                Err(_) => state.remove_address(addr).await,
            }
        }
        for addr in removed {
            match check(addr.clone(), state.active_health_check_path.clone()).await {
                Ok(_) => state.add_address(addr).await,
                Err(_) => {}
            }
        }
    }
}

async fn main_async() {
    // Initialize the logging library. You can print log messages using the `log` macros:
    // https://docs.rs/log/0.4.8/log/ You are welcome to continue using print! statements; this
    // just looks a little prettier.
    if let Err(_) = std::env::var("RUST_LOG") {
        std::env::set_var("RUST_LOG", "debug");
    }
    pretty_env_logger::init();

    // Parse the command line arguments passed to this program
    let options = CmdOptions::parse();
    if options.upstream.len() < 1 {
        log::error!("At least one upstream server must be specified using the --upstream option.");
        std::process::exit(1);
    }

    // Start listening for connections
    let listener = match TcpListener::bind(&options.bind).await {
        Ok(listener) => listener,
        Err(err) => {
            log::error!("Could not bind to {}: {}", options.bind, err);
            std::process::exit(1);
        }
    };
    log::info!("Listening for requests on {}", options.bind);

    // Handle incoming connections
    let state = ProxyState {
        upstream_addresses: RwLock::new((options.upstream, HashSet::new())),
        active_health_check_interval: options.active_health_check_interval,
        active_health_check_path: options.active_health_check_path,
        max_requests_per_minute: options.max_requests_per_minute,
        rate_limiter: Mutex::new(RateLimiter::new()),
    };

    let state = Arc::new(state);
    let cloned = state.clone();
    task::spawn(active_health_check(cloned));
    let cloned = state.clone();
    task::spawn(rate_limiter_rotate(cloned));

    let mut incoming = listener.incoming();

    while let Some(stream) = incoming.next().await {
        if let Ok(stream) = stream {
            let state = state.clone();

            let _ = task::spawn(handle_connection(stream, state));
        }
    }
}

async fn connect_to_upstream_server(upstream_ip: &str) -> Result<TcpStream, std::io::Error> {
    TcpStream::connect(upstream_ip).await.or_else(|err| {
        log::error!("Failed to connect to upstream {}: {}", upstream_ip, err);
        Err(err)
    })
}

async fn connect_to_upstream(state: &ProxyState) -> Result<TcpStream, std::io::Error> {
    let mut rng = rand::rngs::StdRng::from_entropy();
    loop {
        let guard = state.upstream_addresses.read().await;
        if guard.0.len() == 0 {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "all addresses are down",
            ));
        }
        let upstream_idx = rng.gen_range(0, guard.0.len());
        let upstream_ip = guard.0[upstream_idx].clone();
        drop(guard);
        match connect_to_upstream_server(&upstream_ip).await {
            Ok(x) => return Ok(x),
            Err(e) => {
                state.remove_address(upstream_ip).await;
            }
        }
    }
}
async fn send_response(client_conn: &mut TcpStream, response: &http::Response<Vec<u8>>) {
    let client_ip = client_conn.peer_addr().unwrap().ip().to_string();
    log::info!(
        "{} <- {}",
        client_ip,
        response::format_response_line(&response)
    );
    if let Err(error) = response::write_to_stream(&response, client_conn).await {
        log::warn!("Failed to send response to client: {}", error);
        return;
    }
}
async fn rate_limiter_rotate(state: Arc<ProxyState>) {
    let mut interval = stream::interval(Duration::from_secs(60));
    while let Some(_) = interval.next().await {
        let mut guard = state.rate_limiter.lock().await;
        guard.swap();
        drop(guard);
    }
}

async fn rate_limiter(client_ip: String, state: Arc<ProxyState>) -> bool {
    let mut guard = state.rate_limiter.lock().await;
    guard.put(&client_ip);
    let rate = guard.get(&client_ip);
    drop(guard);
    println!("rate {} {}", rate, state.max_requests_per_minute);
    rate < state.max_requests_per_minute
}

async fn handle_connection(mut client_conn: TcpStream, state: Arc<ProxyState>) {
    let client_ip = client_conn.peer_addr().unwrap().ip().to_string();
    log::info!("Connection received from {}", client_ip);
    if state.max_requests_per_minute != 0 && !rate_limiter(client_ip.clone(), state.clone()).await {
        println!("rate limit exceeded");
        let response = response::make_http_error(http::StatusCode::TOO_MANY_REQUESTS);
        send_response(&mut client_conn, &response).await;
        return;
    }
    // Open a connection to a random destination server
    let mut upstream_conn = match connect_to_upstream(&state).await {
        Ok(stream) => stream,
        Err(_error) => {
            let response = response::make_http_error(http::StatusCode::BAD_GATEWAY);
            send_response(&mut client_conn, &response).await;
            return;
        }
    };
    let upstream_ip = client_conn.peer_addr().unwrap().ip().to_string();

    // The client may now send us one or more requests. Keep trying to read requests until the
    // client hangs up or we get an error.
    loop {
        // Read a request from the client
        let mut request = match request::read_from_stream(&mut client_conn).await {
            Ok(request) => request,
            // Handle case where client closed connection and is no longer sending requests
            Err(request::Error::IncompleteRequest(0)) => {
                log::debug!("Client finished sending requests. Shutting down connection");
                return;
            }
            // Handle I/O error in reading from the client
            Err(request::Error::ConnectionError(io_err)) => {
                log::info!("Error reading request from client stream: {}", io_err);
                return;
            }
            Err(error) => {
                log::debug!("Error parsing request: {:?}", error);
                let response = response::make_http_error(match error {
                    request::Error::IncompleteRequest(_)
                    | request::Error::MalformedRequest(_)
                    | request::Error::InvalidContentLength
                    | request::Error::ContentLengthMismatch => http::StatusCode::BAD_REQUEST,
                    request::Error::RequestBodyTooLarge => http::StatusCode::PAYLOAD_TOO_LARGE,
                    request::Error::ConnectionError(_) => http::StatusCode::SERVICE_UNAVAILABLE,
                });
                send_response(&mut client_conn, &response).await;
                continue;
            }
        };
        log::info!(
            "{} -> {}: {}",
            client_ip,
            upstream_ip,
            request::format_request_line(&request)
        );

        // Add X-Forwarded-For header so that the upstream server knows the client's IP address.
        // (We're the ones connecting directly to the upstream server, so without this header, the
        // upstream server will only know our IP, not the client's.)
        request::extend_header_value(&mut request, "x-forwarded-for", &client_ip);

        // Forward the request to the server
        if let Err(error) = request::write_to_stream(&request, &mut upstream_conn).await {
            log::error!(
                "Failed to send request to upstream {}: {}",
                upstream_ip,
                error
            );
            let response = response::make_http_error(http::StatusCode::BAD_GATEWAY);
            send_response(&mut client_conn, &response).await;
            return;
        }
        log::debug!("Forwarded request to server");

        // Read the server's response
        let response = match response::read_from_stream(&mut upstream_conn, request.method()).await
        {
            Ok(response) => response,
            Err(error) => {
                log::error!("Error reading response from server: {:?}", error);
                let response = response::make_http_error(http::StatusCode::BAD_GATEWAY);
                send_response(&mut client_conn, &response).await;
                return;
            }
        };
        // Forward the response to the client
        send_response(&mut client_conn, &response).await;
        log::debug!("Forwarded response to client");
    }
}
