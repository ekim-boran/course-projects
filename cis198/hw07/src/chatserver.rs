use async_std::channel::unbounded;
use async_std::net::SocketAddr;
use async_std::net::TcpListener;
use async_std::net::TcpStream;
use async_std::sync::Arc;
use async_std::sync::Mutex;
use async_std::task;
use async_tungstenite::accept_async;
use async_tungstenite::tungstenite::Message;
use async_tungstenite::tungstenite::WebSocket;
use futures::prelude::*;
use std::collections::HashMap;

use async_std::prelude::FutureExt;

const WS_ADDR: &'static str = "0.0.0.0:1981";

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
/// Represents a single, atomic action taken by a chat member.
///
/// DO NOT MODIFY: the JavaScript relies on this!
#[serde(tag = "variant")]
enum ChatAction {
    Connect { addr: String },
    Disconnect { addr: String },
    Msg { user: String, text: String },
}

struct State {
    clients: HashMap<String, WebSocket<TcpStream>>,
}

type Result<T> = std::result::Result<T, std::io::Error>;

/// Spawn a WebSocket listener thread.
pub async fn start() -> Result<()> {
    let state = PeerMap::new(Mutex::new(HashMap::new()));
    let try_socket = TcpListener::bind(&WS_ADDR).await;
    let listener = try_socket.expect("Failed to bind");
    println!("Listening on: {}", WS_ADDR);
    while let Ok((stream, addr)) = listener.accept().await {
        task::spawn(handle_connection(state.clone(), stream, addr));
    }
    Ok(())
}

/// Create the relay MPSC (multi-producer/single-consumer) channel, spawn the
/// relay thread, then listen for WebSocket clients and spawn their threads.

/// The relay thread handles all `ChatActserde_json::to_string(&msg).unwrap())
/// * If the client sends any other message (i.e. `ChatAction::Msg`), it will be relayed verbatim.
///   (But you should still deserialize and reserialize the `ChatAction` to make sure it is valid!)

type PeerMap = Arc<Mutex<HashMap<SocketAddr, async_std::channel::Sender<ChatAction>>>>;
async fn handle_connection(peer_map: PeerMap, raw_stream: TcpStream, addr: SocketAddr) {
    println!("Incoming TCP connection from: {}", addr);

    let ws_stream = async_tungstenite::accept_async(raw_stream).await.unwrap();
    println!("WebSocket connection established: {}", addr);

    // Insert the write part of this peer to the peer map.
    let (tx, rx) = unbounded();
    peer_map.lock().await.insert(addr, tx);
    let cloned = peer_map.clone();
    let (outgoing, incoming) = ws_stream.split();

    let broadcast_incoming = incoming
        .try_filter(|msg| future::ready(!msg.is_close()))
        .try_for_each(move |msg| {
            let peer_map = peer_map.clone();
            async move {
                println!(
                    "Received a message from {}: {}",
                    addr,
                    msg.to_text().unwrap()
                );
                let parsed: ChatAction = serde_json::from_str(msg.to_text().unwrap()).unwrap();

                let peers = peer_map.lock().await;

                // We want to broadcast the message to everyone except ourselves.
                let broadcast_recipients = peers
                    .iter()
                    .filter(|(peer_addr, _)| peer_addr != &&addr)
                    .map(|(_, ws_sink)| ws_sink);

                for recp in broadcast_recipients {
                    recp.send(parsed.clone()).await;
                }
                Ok(())
            }
        });

    let receive_from_others = rx
        .map(|x| Ok(Message::Text(serde_json::to_string(&x).unwrap())))
        .forward(outgoing);

    let both = async {
        let k = task::spawn(receive_from_others);
        let l = task::spawn(broadcast_incoming);
        k.race(l).await
    };
    task::block_on(both);

    println!("{} disconnected", &addr);
    cloned.lock().await.remove(&addr);
}
