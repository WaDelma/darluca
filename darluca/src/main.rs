extern crate libdarluca;
#[macro_use]
extern crate clap;
extern crate termcolor;
extern crate failure;
#[macro_use]
extern crate failure_derive;
// TODO: Remove nom and symtern from repl dependencies.
extern crate nom;
extern crate symtern;

use symtern::prelude::*;

use clap::{Arg, App, ArgMatches};

use termcolor::{StandardStream, ColorSpec, Color, WriteColor};
use termcolor::ColorChoice as CC;

use libdarluca::parser::parse;
use libdarluca::interner::Interner;
use libdarluca::lexer::Lexer;
use libdarluca::parser::ast::Identifier;
use libdarluca::interpreter::interpret_noscope;
use libdarluca::interpreter::{Memory, TypedValue};

use std::io::prelude::*;
use std::io::stdin;

type Result<T> = ::std::result::Result<T, CliError>;

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

arg_enum! {
    #[derive(PartialEq, Debug, Clone, Copy)]
    enum ColorChoice {
        Always,
        AlwaysAnsi,
        Auto,
        Never
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

const REPL_COMMANDS: [(&str, &str); 4] = [
    (":h", "In-repl help"),
    (":q", "Exit the repl"),
    (":c", "Clears the code from the repl session"),
    (":t <variable>", "Tells the type of the variable"),
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
    s.set_bold(true);
    s
}

fn note_style() -> ColorSpec {
    let mut s = ColorSpec::new();
    s.set_fg(Some(Color::Yellow));
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

// TODO: Instead this ugly thing there should be API for doing requests where one of them is adding code to the session.
fn interpret_line(line: &str, out: &mut StandardStream, memory: &mut Memory<TypedValue>, interner: &mut Interner) -> Result<bool> {
    use nom::IResult::*;
    let tokens = match Lexer::new(interner).tokenize(line.as_ref()).1 {
        Done(left, tokens) => if left.is_empty() {
            tokens
        } else {
            out.write_color(&error_style(), "Lexing failed:\n")?;
            write!(out, "Left: {}\n", String::from_utf8_lossy(&left[..(left.len() - 1)]))?;
            write!(out, "Tokens: {:#?}\n", tokens.tokens)?;
            return Ok(false);
        },
        Error(e) => {
            out.write_color(&error_style(), "Lexing failed:\n")?;
            write!(out, "{}\n", e)?;
            return Ok(false);
        },
        Incomplete(n) => {
            out.write_color(&error_style(), "Lexing needed more: ")?;
            write!(out, "{:?}\n", n)?;
            return Ok(false);
        },
    };
    let ast = match parse(tokens.borrow()) {
        Done(left, ast) => if left.tokens.is_empty() {
            ast
        } else {
            out.write_color(&error_style(), "Parsing failed:\n")?;
            write!(out, "Left: {:#?}\n", left.tokens)?;
            write!(out, "Ast: {:#?}\n", ast.expressions)?;
            return Ok(false);
        }
        Error(e) => {
            out.write_color(&error_style(), "Parsing failed:\n")?;
            write!(out, "{}\n", e)?;
            return Ok(false);
        },
        Incomplete(n) => {
            out.write_color(&error_style(), "Parsing needed more: ")?;
            write!(out, "{:?}\n", n)?;
            return Ok(false);
        },
    };
    match interpret_noscope(&ast.expressions, memory, interner) {
        Ok(value) => {
            match value.value().display(interner) {
                Ok(val) => {
                    match value.ty().display(interner) {
                        Ok(ty) => {
                            writeln!(out, "{}: {}", val, ty)?;
                            Ok(true)
                        },
                        Err(e) => {
                            out.write_color(&error_style(), "Interpreting failed:\n")?;
                            write!(out, "{}\n", e)?;
                            Ok(false)
                        }
                    }
                },
                Err(e) => {
                    out.write_color(&error_style(), "Interpreting failed:\n")?;
                    write!(out, "{}\n", e)?;
                    Ok(false)
                }
            }
        },
        Err(e) => {
            out.write_color(&error_style(), "Interpreting failed:\n")?;
            write!(out, "{}\n", e)?;
            Ok(false)
        },
    }
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
    let mut history = vec![];
    let mut memory = Memory::<TypedValue>::new();
    memory.start_scope();
    let mut interner = Interner::new();
    loop {
        repl_symbol(&args, &mut out)?;
        stdin.read_line(&mut line)?;
        history.push(line.clone());
        if line.starts_with(":") {
            match &line.trim()[1..] {
                "q" => return Ok(()),
                "h" => print_help(&args, &mut out)?,
                "c" => {
                    memory.clear();
                    memory.start_scope();
                    out.write_color(&note_style(), "Cleared all code from the session\n")?
                },
                l if l.starts_with("t ") => {
                    let l = &l[2..];
                    // TODO: This is so bad code.
                    let val = memory.get(&Identifier(interner.intern(l).unwrap())).unwrap();
                    out.write(format!("{}\n", val.ty().display(&interner).unwrap()).as_ref())?;
                },
                _ => out.write_color(&error_style(), "Unknown repl command\n")?,
            }
        } else {
            interpret_line(&line, &mut out, &mut memory, &mut interner)?;
        }
        line.clear();
    }
}