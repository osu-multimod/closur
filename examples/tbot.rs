use closur::bancho::{bot, multiplayer, Channel, Client, ClientOptions, Event};

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
    let response = writer.send_private_bot_command(bot::Command::Make(title.clone())).await?;
    let response = response.make();

    // Channel is automatically joined after `!mp make`
    let lobby = Channel::Multiplayer(response.id());
    println!("[TBOT] Made and joined lobby \"{}\" ({})", title, lobby);
    let mut writer = writer.into_channel_writer(lobby.clone());

    writer.send_bot_command(bot::Command::Password(password.clone())).await?;
    println!("[TBOT] Set lobby password to \"{}\"", password);

    let mut subscriber = client.events();
    loop {
        let event = subscriber.recv().await?;
        if event.channel() == Some(&lobby) {
            match event {
                Event::Message { from, body, .. } => {
                    println!("[LOBBY] {}: {}", from.to_string(), body);
                    if body.starts_with("!info") {
                        writer.send_chat("TBOT from closur examples").await?;
                    } else if body.starts_with("!echo") {
                        let message = body.trim_start_matches("!echo").trim();
                        writer.send_chat(message).await?;
                    } else if body.starts_with("!start") {
                        writer.send_bot_command(bot::Command::Start(None)).await?;
                    } else if body.starts_with("!test") {
                        // Fetch settings for current lobby
                        let response = writer.send_bot_command(bot::Command::Settings).await?;
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

                        writer
                            .send_chat(
                                format!("Players (highlighted): {}", with_highlight).as_str(),
                            )
                            .await?;
                        writer
                            .send_chat(
                                format!("Players (without highlight): {}", without_highlight)
                                    .as_str(),
                            )
                            .await?;
                    }
                }
                Event::Multiplayer(_, event) => match event {
                    multiplayer::Event::PlayerJoined { user, .. } => {
                        writer
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
                },
                _ => {},
            }
        }
    }
    Ok(())
}
