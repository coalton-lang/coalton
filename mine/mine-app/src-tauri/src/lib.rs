#[cfg(unix)]
use portable_pty::{CommandBuilder, NativePtySystem, PtySize, PtySystem, MasterPty};

use std::io::{Read, Write};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use tauri::{AppHandle, Emitter, State};

trait PtyResizer: Send + Sync {
    fn resize(&self, rows: u16, cols: u16) -> Result<(), String>;
}

#[cfg(unix)]
struct UnixResizer(Mutex<Box<dyn MasterPty + Send>>);

#[cfg(unix)]
impl PtyResizer for UnixResizer {
    fn resize(&self, rows: u16, cols: u16) -> Result<(), String> {
        self.0.lock().unwrap()
            .resize(PtySize { rows, cols, pixel_width: 0, pixel_height: 0 })
            .map_err(|e| e.to_string())
    }
}

/// On Windows, resize is sent as an in-band escape sequence through stdin.
/// mine's input parser handles CSI 8;rows;cols t to update its size.
#[cfg(windows)]
struct PipeResizer(Arc<Mutex<Option<Box<dyn Write + Send>>>>);

#[cfg(windows)]
impl PtyResizer for PipeResizer {
    fn resize(&self, rows: u16, cols: u16) -> Result<(), String> {
        if let Some(ref mut writer) = *self.0.lock().unwrap() {
            let seq = format!("\x1b[8;{};{}t", rows, cols);
            writer.write_all(seq.as_bytes()).map_err(|e| e.to_string())?;
            writer.flush().map_err(|e| e.to_string())?;
        }
        Ok(())
    }
}

struct PtyState {
    spawned: AtomicBool,
    writer: Arc<Mutex<Option<Box<dyn Write + Send>>>>,
    resizer: Mutex<Option<Box<dyn PtyResizer>>>,
}

#[tauri::command]
fn spawn_pty(app: AppHandle, state: State<PtyState>, rows: u16, cols: u16) -> Result<(), String> {
    // Idempotent: only the first call spawns. Any subsequent call (e.g. from a
    // misbehaving frontend reload) is a silent no-op.
    if state.spawned.swap(true, Ordering::SeqCst) {
        return Ok(());
    }

    let mine_bin = std::env::current_exe()
        .map_err(|e| e.to_string())?
        .parent()
        .ok_or("no parent directory")?
        .join(if cfg!(windows) { "mine-core.exe" } else { "mine-core" });

    #[cfg(unix)]
    let (reader, writer, resizer): (Box<dyn Read + Send>, Box<dyn Write + Send>, Box<dyn PtyResizer>) = {
        let pty_system = NativePtySystem::default();
        let pair = pty_system
            .openpty(PtySize { rows, cols, pixel_width: 0, pixel_height: 0 })
            .map_err(|e| e.to_string())?;

        let mut cmd = CommandBuilder::new(&mine_bin);
        cmd.env("TERM", "xterm-256color");
        let _child = pair.slave.spawn_command(cmd).map_err(|e| e.to_string())?;
        drop(pair.slave);

        let reader = pair.master.try_clone_reader().map_err(|e| e.to_string())?;
        let writer = pair.master.take_writer().map_err(|e| e.to_string())?;
        let resizer: Box<dyn PtyResizer> = Box::new(UnixResizer(Mutex::new(pair.master)));

        (reader, writer, resizer)
    };

    #[cfg(windows)]
    let (reader, writer, resizer): (Box<dyn Read + Send>, Box<dyn Write + Send>, Box<dyn PtyResizer>) = {
        use std::os::windows::process::CommandExt;
        use std::process::{Command, Stdio};

        const CREATE_NO_WINDOW: u32 = 0x08000000;

        let resizer: Box<dyn PtyResizer> = Box::new(PipeResizer(Arc::clone(&state.writer)));

        let mut child = Command::new(&mine_bin)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .env("TERM", "xterm-256color")
            .env("COLUMNS", cols.to_string())
            .env("LINES", rows.to_string())
            .creation_flags(CREATE_NO_WINDOW)
            .spawn()
            .map_err(|e| e.to_string())?;

        let stdin = child.stdin.take().ok_or("failed to take child stdin")?;
        let stdout = child.stdout.take().ok_or("failed to take child stdout")?;

        // Wait for the child process to exit and close the app.
        // On Windows, pipe EOF alone may not fire reliably.
        let exit_app = app.clone();
        std::thread::spawn(move || {
            let _ = child.wait();
            exit_app.exit(0);
        });

        (Box::new(stdout), Box::new(stdin), resizer)
    };

    *state.writer.lock().unwrap() = Some(writer);
    *state.resizer.lock().unwrap() = Some(resizer);

    // Decouple pipe reading from event emission so that mine's stdout
    // is always drained, even if app.emit() stalls (e.g. during resize).
    let (tx, rx) = std::sync::mpsc::channel::<Vec<u8>>();

    std::thread::spawn(move || {
        let mut reader = reader;
        let mut buf = [0u8; 4096];
        loop {
            match reader.read(&mut buf) {
                Ok(0) | Err(_) => break,
                Ok(n) => {
                    let _ = tx.send(buf[..n].to_vec());
                }
            }
        }
    });

    std::thread::spawn(move || {
        while let Ok(data) = rx.recv() {
            let _ = app.emit("pty-data", data);
        }
        let _ = app.emit("pty-exit", ());
    });

    Ok(())
}

#[tauri::command]
fn write_pty(state: State<PtyState>, data: String) -> Result<(), String> {
    let mut guard = state.writer.lock().unwrap();
    let writer = guard.as_mut().ok_or("pty not ready")?;
    writer.write_all(data.as_bytes()).map_err(|e| e.to_string())?;
    writer.flush().map_err(|e| e.to_string())?;
    Ok(())
}

#[tauri::command]
fn resize_pty(state: State<PtyState>, rows: u16, cols: u16) -> Result<(), String> {
    if let Some(ref resizer) = *state.resizer.lock().unwrap() {
        resizer.resize(rows, cols)?;
    }
    Ok(())
}

#[derive(serde::Serialize)]
struct FontConfig {
    font_family: String,
    font_size: u16,
}

#[tauri::command]
fn get_font_config() -> FontConfig {
    let defaults = FontConfig {
        font_family: "Iosevka Fixed Slab Extended".into(),
        font_size: 14,
    };

    let Some(home) = dirs::home_dir() else {
        return defaults;
    };
    let path = home.join(".mine");
    let Ok(content) = std::fs::read_to_string(&path) else {
        return defaults;
    };

    FontConfig {
        font_family: extract_string(&content, ":app-font-family")
            .unwrap_or(defaults.font_family),
        font_size: extract_number(&content, ":app-font-size")
            .unwrap_or(defaults.font_size),
    }
}

fn extract_string(content: &str, key: &str) -> Option<String> {
    let idx = content.find(key)? + key.len();
    let rest = &content[idx..];
    let open = rest.find('"')? + 1;
    let close = rest[open..].find('"')?;
    Some(rest[open..open + close].to_string())
}

fn extract_number(content: &str, key: &str) -> Option<u16> {
    let idx = content.find(key)? + key.len();
    let rest = content[idx..].trim_start();
    let end = rest.find(|c: char| !c.is_ascii_digit()).unwrap_or(rest.len());
    rest[..end].parse().ok()
}

#[tauri::command]
fn close_window(app: AppHandle) {
    app.exit(0);
}

#[cfg(target_os = "macos")]
fn macos_menu<R: tauri::Runtime>(handle: &tauri::AppHandle<R>) -> tauri::Result<tauri::menu::Menu<R>> {
    use tauri::menu::{AboutMetadataBuilder, MenuBuilder, PredefinedMenuItem, SubmenuBuilder};

    let about = PredefinedMenuItem::about(
        handle,
        Some("About mine"),
        Some(AboutMetadataBuilder::new().build()),
    )?;

    let app_menu = SubmenuBuilder::new(handle, "mine")
        .item(&about)
        .separator()
        .item(&PredefinedMenuItem::hide(handle, None)?)
        .item(&PredefinedMenuItem::hide_others(handle, None)?)
        .item(&PredefinedMenuItem::show_all(handle, None)?)
        .separator()
        .item(&PredefinedMenuItem::quit(handle, None)?)
        .build()?;

    MenuBuilder::new(handle)
        .item(&app_menu)
        .build()
}

pub fn run() {
    let builder = tauri::Builder::default()
        .manage(PtyState {
            spawned: AtomicBool::new(false),
            writer: Arc::new(Mutex::new(None)),
            resizer: Mutex::new(None),
        })
        .invoke_handler(tauri::generate_handler![spawn_pty, write_pty, resize_pty, close_window, get_font_config]);

    #[cfg(target_os = "macos")]
    let builder = builder.menu(macos_menu);

    builder
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
