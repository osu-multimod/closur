use closur::bancho::{bot, multiplayer, Channel, Chat, Client, ClientOptions, Event};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;

// This example shows how to create a simple lobby bot.
#[tokio::main]
async fn main() -> Result<()> {
    // "BANCHO_USERNAME" and "BANCHO_PASSWORD" are **compile-time** environment variables.
    // You need to set username and password, e.g. running
    // `BANCHO_USERNAME=username BANCHO_PASSWORD=password cargo run --example tbot`
    let options = ClientOptions::default()
        .username(env!("BANCHO_USERNAME").to_string())
        .password(env!("BANCHO_PASSWORD").to_string());

    let mut client = Client::new(options).await?;
    let mut writer = client.writer();
    client.auth().await?;

    println!("[TBOT] Connected and authenticated, waiting for events...");
    let title = "TBOT LOBBY".to_string();
    let password = "tbot".to_string();
    let response = writer
        .send_private_bot_command(bot::Command::Make(title.clone()))
        .await?;
    let response = response.make();

    let mut lobby = client.join_match(response.id()).await?;
    lobby
        .send_bot_command(bot::Command::Password(password.clone()))
        .await?;
    println!("[TBOT] Set lobby password to \"{}\"", password);

    // This event subscriber will only receive events from the lobby.
    let mut subscriber = lobby.events();
    loop {
        let event = subscriber.recv().await?;
        match event {
            Event::Message { from, body, .. } => {
                println!("[LOBBY] {}: {}", from.to_string(), body);
                if body.starts_with("!info") {
                    lobby.send_chat("TBOT from closur examples").await?;
                } else if body.starts_with("!echo") {
                    let message = body.trim_start_matches("!echo").trim();
                    lobby.send_chat(message).await?;
                } else if body.starts_with("!start") {
                    lobby.send_bot_command(bot::Command::Start(None)).await?;
                } else if body.starts_with("!invite") {
                    let message = Chat::new("Join my multiplayer game: ")
                        .append_link(title.as_str(), lobby.invite_link(None).as_str())
                        .to_string();
                    lobby.send_chat(message.as_str()).await?;
                } else if body.starts_with("!test") {
                    // Fetch settings for current lobby
                    let response = lobby.send_bot_command(bot::Command::Settings).await?;
                    let settings = response.settings();

                    let slots = settings.valid_slots();
                    let with_highlight = slots
                        .iter()
                        .map(|x| x.user().name())
                        .collect::<Vec<_>>()
                        .join(", ");
                    let without_highlight = slots
                        .iter()
                        .map(|x| x.user().name_without_highlight())
                        .collect::<Vec<_>>()
                        .join(", ");

                    lobby
                        .send_chat(format!("Players (highlighted): {}", with_highlight).as_str())
                        .await?;
                    lobby
                        .send_chat(
                            format!("Players (without highlight): {}", without_highlight).as_str(),
                        )
                        .await?;
                }
            }
            Event::Multiplayer(_, event) => match event {
                multiplayer::Event::PlayerJoined { user, .. } => {
                    lobby
                        .send_chat(
                            format!("Hey, {}! This is TBOT lobby, have fun here.", user.name())
                                .as_str(),
                        )
                        .await?;
                }
                _ => {}
            },
            Event::Part(_) => {
                println!("See you next time.");
                break;
            }
            _ => {}
        }
    }
    Ok(())
}
