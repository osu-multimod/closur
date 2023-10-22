use closur::bancho::{Client, ClientOptionsBuilder, EventKind};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;

/// This example shows how to create a simple echo bot.
#[tokio::main]
async fn main() -> Result<()> {
    let options = ClientOptionsBuilder::default()
        .username(std::env::var("BANCHO_USERNAME").unwrap())
        .password(std::env::var("BANCHO_PASSWORD").unwrap())
        .build();

    let mut client = Client::new(options);
    client.run().await?;
    client.auth().await?;
    let operator = client.operator();
    let mut subscriber = operator.subscribe();
    loop {
        let event = subscriber.recv().await?;
        match event.kind() {
            EventKind::Message(m) => {
                println!("[PM] {}: {}", m.user(), m.content());
                operator.send_user_chat(m.user(), m.content()).await?;
            }
            EventKind::Closed => break,
            _ => {}
        }
    }
    Ok(())
}
