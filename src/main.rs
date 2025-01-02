use crossterm::{
    cursor,
    event::{Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers},
    execute, queue,
    style::{self, Stylize},
    terminal, Command,
};
use freedesktop_desktop_entry as fdde;
use image::{EncodableLayout, RgbaImage};
use kitty_image as ki;
use nucleo::Nucleo;
use ropey::RopeSlice;
use std::{io, iter, num::NonZeroU32, os::unix::process::CommandExt, sync::Arc};

struct DesktopEntry<'a> {
    entry: fdde::DesktopEntry<'a>,
}

enum EntryKind {
    Desktop(Arc<DesktopEntry<'static>>),
    Action {
        entry: Arc<DesktopEntry<'static>>,
        id: String,
    },
}

struct Entry {
    kind: EntryKind,
    name: String,
    image: Option<Image>,
    command: std::process::Command,
}

impl EntryKind {
    pub fn image(&self) -> Option<RgbaImage> {
        let entry = match self {
            EntryKind::Desktop(entry) => entry,
            EntryKind::Action { entry, .. } => entry,
        };
        let icon = entry.entry.icon()?;
        let path = freedesktop_icons::lookup(icon).find()?;

        let image = if path.extension() == Some(std::ffi::OsStr::new("svg")) {
            let size = 128;
            let content = std::fs::read_to_string(&path).ok()?;
            let tree = resvg::usvg::Tree::from_str(&content, &Default::default()).ok()?;
            let mut bytes = vec![0u8; (size * size * 4) as usize];
            let mut pixmap =
                resvg::tiny_skia::PixmapMut::from_bytes(&mut bytes, size, size).unwrap();
            let svg_size = tree.size();
            resvg::render(
                &tree,
                resvg::usvg::Transform::from_scale(
                    size as f32 / svg_size.width(),
                    size as f32 / svg_size.height(),
                ),
                &mut pixmap,
            );
            RgbaImage::from_raw(size, size, bytes).unwrap()
        } else {
            let image = image::open(&path).ok()?;
            image.to_rgba8()
        };

        Some(image)
    }

    pub fn name(&self, locales: &[impl AsRef<str>]) -> Option<String> {
        match self {
            EntryKind::Desktop(entry) => entry
                .entry
                .name(locales)
                .as_deref()
                .map(ToString::to_string),
            EntryKind::Action { id, entry } => entry
                .entry
                .name(locales)
                .as_deref()
                .map(ToString::to_string)
                .and_then(|entry_name| {
                    entry
                        .entry
                        .action_name(id, locales)
                        .as_deref()
                        .map(ToString::to_string)
                        .map(|action_name| format!("{entry_name} - {action_name}"))
                }),
        }
    }

    pub fn command(&self, terminal: &str) -> Result<std::process::Command, fdde::ExecError> {
        let is_terminal = match &self {
            Self::Desktop(entry) => entry.entry.terminal(),
            Self::Action { entry, id } => entry.entry.action_entry(id, "Terminal") == Some("true"),
        };

        let mut cmd = if is_terminal {
            let mut cmd = std::process::Command::new(terminal);
            cmd.arg("-e");
            cmd
        } else {
            let mut cmd = std::process::Command::new("sh");
            cmd.arg("-c");
            cmd
        };

        cmd.args(match &self {
            Self::Desktop(entry) => entry.entry.parse_exec()?,
            Self::Action { entry, id } => entry.entry.parse_exec_action(id)?,
        });

        Ok(cmd)
    }
}

pub struct Image {
    image: RgbaImage,
    id: NonZeroU32,
    size: (u32, u32),
}

impl Image {
    pub fn load(&self) -> LoadImage {
        LoadImage(self)
    }

    pub fn display(&self, col: u16, row: u16) -> DisplayImage {
        DisplayImage {
            image: self,
            position: (col, row),
        }
    }
}

pub struct LoadImage<'a>(&'a Image);

impl crossterm::Command for LoadImage<'_> {
    fn write_ansi(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        let mut command = ki::Command::new(ki::Action::Delete(ki::ActionDelete {
            hard: true,
            target: ki::DeleteTarget::ID {
                placement: ki::Placement(Some(self.0.id)),
            },
        }));
        command.id = Some(ki::ID(self.0.id));

        let mut command = ki::WrappedCommand::new(command);
        command.double_escape = true;

        write!(f, "{command}")?;

        let mut command = ki::Command::new(ki::Action::TransmitAndDisplay(
            ki::ActionTransmission {
                format: ki::Format::Rgba32,
                medium: ki::Medium::Direct,
                width: self.0.image.width(),
                height: self.0.image.height(),
                ..Default::default()
            },
            ki::ActionPut {
                unicode_placeholder: true,
                columns: self.0.size.0,
                rows: self.0.size.1,
                ..Default::default()
            },
        ));

        command.payload = std::borrow::Cow::Borrowed(self.0.image.as_bytes());
        command.quietness = ki::Quietness::SuppressAll;
        command.id = Some(ki::ID(self.0.id));

        let mut command = ki::WrappedCommand::new(command);
        command.double_escape = true;

        write!(f, "\x1bPtmux;{command}\x1b\\")?;

        Ok(())
    }
}

pub struct DisplayImage<'a> {
    image: &'a Image,
    position: (u16, u16),
}

impl Command for DisplayImage<'_> {
    fn write_ansi(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        cursor::MoveToRow(self.position.1).write_ansi(f)?;
        write!(f, "\x1b[38;5;{}m", self.image.id)?;

        for y in ki::UNICODE_DIACRITICS.iter().take(self.image.size.1 as _) {
            cursor::MoveToColumn(self.position.0).write_ansi(f)?;
            for x in ki::UNICODE_DIACRITICS.iter().take(self.image.size.0 as _) {
                write!(f, "\u{10EEEE}{}{}", y, x)?;
            }
            cursor::MoveToNextLine(1).write_ansi(f)?;
        }
        write!(f, "\x1b[39m")?;

        Ok(())
    }
}

struct Terminal<'a, W: io::Write>(&'a mut W);

impl<'a, W: io::Write> Terminal<'a, W> {
    pub fn new(w: &'a mut W) -> io::Result<Self> {
        execute!(w, terminal::EnterAlternateScreen)?;
        terminal::enable_raw_mode()?;
        Ok(Self(w))
    }

    pub fn exit(&mut self) {
        execute!(
            self.0,
            crossterm::style::ResetColor,
            crossterm::cursor::Show,
            crossterm::terminal::LeaveAlternateScreen,
            cursor::SetCursorStyle::DefaultUserShape
        )
        .expect("reset terminal");
        terminal::disable_raw_mode().expect("reset terminal");
    }
}

impl<W: io::Write> Drop for Terminal<'_, W> {
    fn drop(&mut self) {
        self.exit();
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum TextInputMode {
    Insert,
    Normal,
}

struct TextInput {
    input: ropey::Rope,
    pos: usize,
    mode: TextInputMode,
}

impl TextInput {
    fn new() -> Self {
        Self {
            input: ropey::Rope::new(),
            pos: 0,
            mode: TextInputMode::Insert,
        }
    }

    fn clamp_pos(&mut self) {
        match self.mode {
            TextInputMode::Normal => {
                self.pos = self.pos.clamp(0, self.input.len_chars().saturating_sub(1))
            }
            TextInputMode::Insert => self.pos = self.pos.clamp(0, self.input.len_chars()),
        }
    }

    fn update(&mut self, event: Event) {
        match event {
            Event::Key(KeyEvent {
                code,
                kind: KeyEventKind::Press,
                ..
            }) if self.mode == TextInputMode::Insert => match code {
                KeyCode::Char(ch) => {
                    self.input.insert_char(self.pos, ch);
                    self.pos += 1;
                }
                KeyCode::Backspace => {
                    self.pos = self.pos.saturating_sub(1);
                    _ = self.input.try_remove(self.pos..self.pos + 1);
                }
                KeyCode::Esc => {
                    self.mode = TextInputMode::Normal;
                    self.clamp_pos();
                }
                _ => {}
            },
            Event::Key(KeyEvent {
                code,
                kind: KeyEventKind::Press,
                ..
            }) if self.mode == TextInputMode::Normal => match code {
                KeyCode::Char('i') => self.mode = TextInputMode::Insert,
                KeyCode::Char('I') => {
                    self.pos = 0;
                    self.mode = TextInputMode::Insert;
                }
                KeyCode::Char('a') => {
                    self.mode = TextInputMode::Insert;
                    self.pos += 1;
                    self.clamp_pos();
                }
                KeyCode::Char('A') => {
                    self.pos = self.input.len_chars();
                    self.mode = TextInputMode::Insert;
                }
                KeyCode::Char('h') => {
                    self.pos = self.pos.saturating_sub(1);
                    self.clamp_pos();
                }
                KeyCode::Char('x') => {
                    _ = self.input.try_remove(self.pos..self.pos + 1);
                    self.clamp_pos();
                }
                KeyCode::Char('d') => {
                    _ = self.input.try_remove(..);
                    self.clamp_pos();
                }
                KeyCode::Char('l') => {
                    self.pos += 1;
                    self.clamp_pos();
                }
                _ => {}
            },
            _ => {}
        }
    }

    fn sides(&self) -> (RopeSlice<'_>, RopeSlice<'_>) {
        (self.input.slice(..self.pos), self.input.slice(self.pos..))
    }
}

enum TickResult<'a> {
    Continue,
    Run(&'a mut std::process::Command),
    ManualCommand(String),
    Quit,
}

struct Ui {
    entries: Vec<Entry>,
    nucleo: Nucleo<(usize, String)>,
    input: TextInput,
    locales: Vec<String>,
    selected_item: usize,
}

impl Ui {
    fn new(terminal: &str) -> Self {
        let locales = fdde::get_languages_from_env();
        let entries = fdde::Iter::new(fdde::default_paths())
            .entries(Some(&locales))
            .flat_map(|entry| {
                let desktop_entry = Arc::new(DesktopEntry { entry });

                let actions = desktop_entry
                    .entry
                    .actions()
                    .into_iter()
                    .flatten()
                    .map(|name| EntryKind::Action {
                        entry: Arc::clone(&desktop_entry),
                        id: name.to_string(),
                    })
                    .collect::<Vec<_>>();

                iter::once(EntryKind::Desktop(desktop_entry))
                    .chain(actions)
                    .collect::<Vec<_>>()
            })
            .enumerate()
            .filter_map(|(i, entry)| {
                Some(Entry {
                    image: entry.image().map(|image| Image {
                        image,
                        id: NonZeroU32::new((i + 1) as _).unwrap(),
                        size: (2, 1),
                    }),
                    command: entry.command(terminal).ok()?,
                    name: entry.name(&locales)?,
                    kind: entry,
                })
            })
            .collect::<Vec<_>>();

        let mut nucleo = Nucleo::new(nucleo::Config::DEFAULT, Arc::new(|| {}), None, 1);

        let injector = nucleo.injector();
        for item in entries.iter().map(|entry| entry.name.clone()).enumerate() {
            injector.push(item, |(_, name), row| {
                row[0] = name.clone().into();
            });
        }

        nucleo.tick(10);

        Self {
            entries,
            locales,
            nucleo,
            input: TextInput::new(),
            selected_item: 0,
        }
    }

    fn current_entry_index(&self) -> Option<usize> {
        let (i, _) = self
            .nucleo
            .snapshot()
            .get_matched_item(self.selected_item as u32)?
            .data;

        Some(*i)
    }

    fn update(&mut self, event: Event) -> TickResult<'_> {
        match event {
            Event::Key(KeyEvent {
                code: KeyCode::Char('q'),
                kind: KeyEventKind::Press,
                ..
            }) if self.input.mode == TextInputMode::Normal => TickResult::Quit,
            Event::Key(KeyEvent {
                code: KeyCode::Char('n'),
                kind: KeyEventKind::Press,
                modifiers,
                ..
            }) if modifiers.contains(KeyModifiers::CONTROL) => {
                let item_count = self.nucleo.snapshot().matched_item_count() as usize;
                self.selected_item = (self.selected_item + 1).min(item_count.saturating_sub(1));
                TickResult::Continue
            }
            Event::Key(KeyEvent {
                code: KeyCode::Char('p'),
                kind: KeyEventKind::Press,
                modifiers,
                ..
            }) if modifiers.contains(KeyModifiers::CONTROL) => {
                self.selected_item = self.selected_item.saturating_sub(1);
                TickResult::Continue
            }
            Event::Key(KeyEvent {
                code: KeyCode::Enter,
                kind: KeyEventKind::Press,
                ..
            }) => {
                let Some(index) = self.current_entry_index() else {
                    return TickResult::ManualCommand(self.input.input.to_string());
                };

                TickResult::Run(&mut self.entries[index].command)
            }
            _ => {
                self.input.update(event);
                self.nucleo.pattern.reparse(
                    0,
                    &self.input.input.to_string(),
                    nucleo::pattern::CaseMatching::Ignore,
                    nucleo::pattern::Normalization::Smart,
                    false,
                );
                self.nucleo.tick(10);

                let item_count = self.nucleo.snapshot().matched_item_count() as usize;
                self.selected_item = (self.selected_item).min(item_count.saturating_sub(1));

                TickResult::Continue
            }
        }
    }

    fn draw(&self, terminal: &mut Terminal<impl io::Write>) -> io::Result<()> {
        let (lhs, rhs) = self.input.sides();

        queue!(
            terminal.0,
            terminal::Clear(terminal::ClearType::All),
            cursor::MoveTo(0, 0),
            match self.input.mode {
                TextInputMode::Insert => cursor::SetCursorStyle::SteadyBar,
                TextInputMode::Normal => cursor::SetCursorStyle::SteadyBlock,
            },
            style::Print(">> "),
            style::Print(lhs),
            cursor::SavePosition,
            style::Print(rhs),
            style::Print("\n\r"),
        )?;

        if let Some(entry) = self.current_entry_index().map(|i| &self.entries[i]) {
            if let Some(image) = &entry.image {
                let (col, row) = cursor::position()?;
                queue!(
                    terminal.0,
                    image.display(col, row),
                    cursor::MoveTo(col + 3, row)
                )?;
            }

            queue!(
                terminal.0,
                style::PrintStyledContent(entry.name.clone().attribute(style::Attribute::Bold)),
                style::Print(format!(
                    " ({})",
                    match &entry.kind {
                        EntryKind::Desktop(entry) => &entry.entry.appid,
                        EntryKind::Action { id, .. } => id.as_str(),
                    }
                ))
            )?;

            if let Some(desc) = match &entry.kind {
                EntryKind::Desktop(entry) => entry.entry.comment(&self.locales),
                EntryKind::Action { .. } => None,
            } {
                queue!(terminal.0, style::Print("\n\r"), style::Print(desc),)?;
            }

            queue!(terminal.0, style::Print("\n\r"))?;
        }

        let (_, height) = terminal::size()?;
        let (_, row) = cursor::position()?;

        let matches = self
            .nucleo
            .snapshot()
            .matched_items(..)
            .take(height as usize - row as usize - 1);

        for (match_index, item) in matches.enumerate() {
            let (index, _) = item.data;
            let entry = &self.entries[*index];

            queue!(terminal.0, style::Print("\n\r"))?;

            if let Some(image) = &entry.image {
                let (col, row) = cursor::position()?;
                queue!(
                    terminal.0,
                    image.display(col, row),
                    cursor::MoveTo(col + 3, row)
                )?;
            }

            if match_index == self.selected_item {
                queue!(
                    terminal.0,
                    style::PrintStyledContent(
                        entry.name.clone().attribute(style::Attribute::Reverse)
                    )
                )?;
            } else {
                queue!(terminal.0, style::Print(entry.name.clone()))?;
            }
        }

        queue!(terminal.0, cursor::RestorePosition)?;
        terminal.0.flush()?;

        Ok(())
    }
}

fn main() -> anyhow::Result<()> {
    let mut stdout = std::io::stdout();
    let mut terminal = Terminal::new(&mut stdout)?;
    let terminal_command = std::env::var("TERMINAL")?;
    let mut ui = Ui::new(&terminal_command);

    for entry in &ui.entries {
        if let Some(image) = &entry.image {
            execute!(terminal.0, image.load())?;
        }
    }

    loop {
        ui.draw(&mut terminal)?;
        match ui.update(crossterm::event::read()?) {
            TickResult::Quit => break,
            TickResult::Continue => {}
            TickResult::Run(entry) => {
                terminal.exit();
                anyhow::bail!(entry.exec())
            }
            TickResult::ManualCommand(cmd) => {
                terminal.exit();
                anyhow::bail!(std::process::Command::new(terminal_command)
                    .arg("-e")
                    .arg("sh")
                    .arg("-c")
                    .arg(cmd)
                    .exec());
            }
        }
    }

    Ok(())
}
