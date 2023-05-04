use closur::bancho::{Client, ClientOptions, Event};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;

// This example shows how to create a simple echo bot.
#[tokio::main]
async fn main() -> Result<()> {
    // "BANCHO_USERNAME" and "BANCHO_PASSWORD" are **compile-time** environment variables.
    // You need to set username and password, e.g. running
    // `BANCHO_USERNAME=username BANCHO_PASSWORD=password cargo run --example echobot`
    let options = ClientOptions::default()
        .username(env!("BANCHO_USERNAME").to_string())
        .password(env!("BANCHO_PASSWORD").to_string());

    let mut client = Client::new(options).await?;
    let mut writer = client.writer();
    client.auth().await?;

    println!("[ECHOBOT] Connected and authenticated, waiting for events...");
    let mut subscriber = client.events();
    loop {
        let event = subscriber.recv().await?;
        match event {
            Event::Message { from, body, .. } => {
                println!("[PM] {}: {}", from.to_string(), body);
                if body.starts_with("!echo") {
                    let message = body.trim_start_matches("!echo").trim();
                    println!("[EVENT] Sent echo message to {}: {}", from, message);
                    writer.send_private_chat(from, message).await?;
                }
            },
            _ => {},
        }
    }
}