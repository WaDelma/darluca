extern crate libdarluca;
#[macro_use]
extern crate clap;
extern crate termcolor;
extern crate failure;
#[macro_use]
extern crate failure_derive;

use clap::{Arg, App, ArgMatches};

use termcolor::{StandardStream, ColorSpec, Color, WriteColor};
use termcolor::ColorChoice as CC;

use std::io::prelude::*;
use std::io::stdin;

type Result<T> = ::std::result::Result<T, CliError>;

arg_enum! {
    #[derive(PartialEq, Debug, Clone, Copy)]
    enum ColorChoice {
        Always,
        AlwaysAnsi,
        Auto,
        Never
    }
}

#[derive(Debug, Fail)]
enum CliError {
    #[fail(display = "IO error occured: {}", _0)]
    IoError(#[cause] ::std::io::Error)
}

impl From<::std::io::Error> for CliError {
    fn from(e: ::std::io::Error) -> Self {
        CliError::IoError(e)
    }
}


impl Into<CC> for ColorChoice {
    fn into(self) -> CC {
        use self::ColorChoice::*;
        match self {
            Always => CC::Always,
            AlwaysAnsi => CC::AlwaysAnsi,
            Auto => CC::Auto,
            Never => CC::Never,
        }
    }
}

const REPL_COMMANDS: [(&str, &str); 3] = [
    (":h", "In-repl help"),
    (":q", "Exit the repl"),
    (":c", "Clears the repl session")
];

fn main() {
    let mut about = String::from(" 🍄 The repl for darluca.\n\nCOMMANDS:\n");
    for &(c, h) in REPL_COMMANDS.iter() {
        about.push_str(&format!("    {}    {}\n", c, h));
    }
    let matches = App::new("Darluca")
        .version(crate_version!())
        .author("Delma")
        .about(&*about)
        .arg(Arg::with_name("INPUT")
            .value_name("FILE")
            .help("Executes darluca file in the repl.")
            .index(1))
        .arg(Arg::with_name("plain")
            .short("p")
            .long("plain")
            .help("Determines if the repl uses plain formating without fancy unicode symbols."))
        .arg(Arg::with_name("symbol")
            .short("s")
            .long("symbol")
            .takes_value(true)
            .help("Sets the symbol preceding the repl."))
        .arg(Arg::with_name("color")
                .short("c")
                .long("color")
                .takes_value(true)
                .possible_values(&ColorChoice::variants())
                .case_insensitive(true)
                .help("Sets the mode of coloring of the repl."))
        .get_matches();
    run(matches).unwrap();
}

trait WithColor {
    fn with_color<F, T>(&mut self, c: ColorSpec, f: F) -> Result<T>
        where F: Fn(&mut Self) -> Result<T>;
    fn write_color(&mut self, c: &ColorSpec, s: &str) -> Result<()>;
}

impl WithColor for StandardStream {
    fn with_color<F, T>(&mut self, c: ColorSpec, f: F) -> Result<T>
        where F: Fn(&mut Self) -> Result<T>
    {
        self.set_color(&c)?;
        let result = f(self);
        self.set_color(&ColorSpec::new())?;
        result
    }

    fn write_color(&mut self, c: &ColorSpec, s: &str) -> Result<()> {
        self.set_color(c)?;
        let result = self.write(s.as_ref());
        self.set_color(&ColorSpec::new())?;
        result?;
        Ok(())
    }
}

fn error_style() -> ColorSpec {
    let mut s = ColorSpec::new();
    s.set_fg(Some(Color::Red));
    s.set_intense(true);
    s
}

fn highlight_style() -> ColorSpec {
    let mut s = ColorSpec::new();
    s.set_fg(Some(Color::Green));
    s
}

fn title_style() -> ColorSpec {
    let mut s = ColorSpec::new();
    s.set_fg(Some(Color::Magenta));
    s.set_bold(true);
    s
}

fn fancy_plain<'a>(args: &ArgMatches, fancy: &'a str, plain: &'a str) -> &'a str {
    if args.is_present("plain") {
        plain
    } else {
        fancy
    }
}

fn repl_symbol(args: &ArgMatches, s: &mut StandardStream) -> Result<()> {
    s.with_color(highlight_style(), |s| {
        Ok(s.write(args.value_of("symbol").unwrap_or_else(|| fancy_plain(args, "𝔇𝔞⟩", "Da>")).as_ref())?)
    })?;
    Ok(s.flush()?)
}

fn print_help(args: &ArgMatches, s: &mut StandardStream) -> Result<()> {
    let hl = highlight_style();
    s.write_color(&hl, fancy_plain(args, " ❘〃", " |//"))?;
    s.write_color(&title_style(), "HELP\n")?;
    for &(c, h) in REPL_COMMANDS.iter() {
        s.write_color(&hl, fancy_plain(args, " ❘", " |"))?;
        writeln!(s, "  {}  {}", c, h)?;
    }
    Ok(())
}

fn run(args: ArgMatches) -> Result<()> {
    let mut out = StandardStream::stdout(
        value_t!(args.value_of("color"), ColorChoice)
            .unwrap_or_else(|_| ColorChoice::Auto)
            .into()
    );
    let mut line = String::new();
    let stdin = stdin();
    let mut stdin = stdin.lock();
    loop {
        repl_symbol(&args, &mut out)?;
        stdin.read_line(&mut line)?;
        match line.trim() {
            ":q" => return Ok(()),
            ":h" => print_help(&args, &mut out)?,
            ":c" => out.write_color(&error_style(), "NOT IMPLEMENTED!\n")?,
            _ => {},
        }
        line.clear();
    }
    
}