use closur::bancho::bot::command::{MpMake, MpPassword, MpSettings, MpStart};
use closur::bancho::multiplayer;
use closur::bancho::{ChatBuilder, Client, ClientOptionsBuilder, EventKind};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;

/// This example shows how to create a simple lobby bot.
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

    let title = "TBOT LOBBY".to_string();
    let password = "tbot".to_string();
    let response = operator
        .send_bot_command(None, MpMake(title.clone()))
        .await?;
    let match_id = response.body().unwrap().id();

    let lobby = operator.join_match(match_id).await?.unwrap();
    println!("[TBOT] Create multiplayer game ID: {}", match_id);
    lobby.send_bot_command(MpPassword(None)).await?;
    println!("[TBOT] Set lobby password to \"{}\"", password);

    let mut subscriber = operator.subscribe();
    loop {
        let event = subscriber.recv().await?;
        match event.kind() {
            EventKind::Message(m) if event.relates_to_match(match_id) => {
                println!("[LOBBY] {}: {}", m.user(), m.content());
                let body = m.content();
                if body.starts_with("!info") {
                    lobby.send_chat("TBOT from closur examples").await?;
                } else if body.starts_with("!echo") {
                    let message = body.trim_start_matches("!echo").trim();
                    lobby.send_chat(message).await?;
                } else if body.starts_with("!start") {
                    lobby.send_bot_command(MpStart(None)).await?;
                } else if body.starts_with("!invite") {
                    lobby
                        .send_chat(
                            ChatBuilder::new()
                                .push("Join my multiplayer game: ")
                                .push_link(&title, lobby.invite_url())
                                .chat(),
                        )
                        .await?;
                } else if body.starts_with("!test") {
                    let response = lobby.send_bot_command(MpSettings).await?;
                    let settings = response.body().unwrap();

                    let with_highlight = settings
                        .slots()
                        .valid_slots()
                        .map(|(_, x)| x.user().name())
                        .collect::<Vec<_>>()
                        .join(", ");
                    let without_highlight = settings
                        .slots()
                        .valid_slots()
                        .map(|(_, x)| x.user().name_without_highlight())
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
            EventKind::Multiplayer(e) if event.relates_to_match(match_id) => {
                use multiplayer::EventKind;
                match e.kind() {
                    EventKind::PlayerJoined { user, .. } => {
                        lobby
                            .send_chat(format!(
                                "Hey, {}! This is TBOT lobby, have fun here.",
                                user.name()
                            ))
                            .await?;
                    }
                    _ => {}
                }
            }
            EventKind::Part(_) | EventKind::Closed => {
                println!("See you next time.");
                break;
            }
            _ => {}
        }
    }
    Ok(())
}
