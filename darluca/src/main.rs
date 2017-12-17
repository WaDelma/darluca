extern crate darluca_lib;
#[macro_use]
extern crate clap;
extern crate termion;
extern crate failure;
#[macro_use]
extern crate failure_derive;
extern crate unicode_width;
// TODO: Remove nom and symtern from repl dependencies.
extern crate nom;

use unicode_width::UnicodeWidthStr;

use clap::{Arg, App, ArgMatches};

use termion::{cursor, clear};
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;

use darluca_lib::parser::parse;
use darluca_lib::interner::Interner;
use darluca_lib::lexer::Lexer;
use darluca_lib::parser::ast::Identifier;
use darluca_lib::interpreter::interpret_noscope;
use darluca_lib::interpreter::{Memory, TypedValue};
use darluca_lib::typechecker::{typecheck, TypeKey};

use std::io::{Write, stdout, stdin};
use std::iter::repeat;

use history::History;
use style::{WithColor, clear_style, error_style, note_style, highlight_style, info_style, logo_theme, welcome_theme};

type Result<T> = ::std::result::Result<T, CliError>;

mod history;
mod style;

#[derive(Debug, Fail)]
pub enum CliError {
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

const REPL_COMMANDS: [(&str, &str); 4] = [
    (":h", "In-repl help"),
    (":q", "Exit the repl"),
    (":c", "Clears the code from the repl session"),
    (":t <variable>", "Tells the type of the variable"),
];

fn main() {
    let mut about = String::from(" 🍄 The repl for Darluca.\n\nCOMMANDS:\n");
    let repl_cmd_width = REPL_COMMANDS.iter().map(|&(ref c, _)| c.width()).max().unwrap_or(0);
    for &(c, h) in REPL_COMMANDS.iter() {
        about.push_str(&format!("    {}", c));
        about.push_str(&repeat(" ").take(repl_cmd_width - c.width()).collect::<String>());
        about.push_str(&format!("    {}\n", h));
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

fn fancy_plain<'a>(args: &ArgMatches, fancy: &'a str, plain: &'a str) -> &'a str {
    if args.is_present("plain") {
        plain
    } else {
        fancy
    }
}

fn repl_symbol<'a>(args: &'a ArgMatches) -> &'a str {
    args.value_of("symbol").unwrap_or_else(|| fancy_plain(args, "𝔇𝔞⟩", "Da>"))
}

fn print_repl_symbol<W: Write>(s: &mut W, args: &ArgMatches) -> Result<()> {
    s.cwrite(highlight_style(), repl_symbol(args))?;
    Ok(())
}

fn print_help<W: Write>(s: &mut W, args: &ArgMatches) -> Result<()> {
    let hl = highlight_style();
    s.cwrite(hl, fancy_plain(args, " ❘〃", " |//"))?;
    s.cwriteln(info_style(), "HELP")?;
    let repl_cmd_width = REPL_COMMANDS.iter().map(|&(ref c, _)| c.width()).max().unwrap_or(0);
    for &(c, h) in REPL_COMMANDS.iter() {
        s.cwrite(hl, fancy_plain(args, " ❘", " |"))?;
        s.cwrite(clear_style(), &format!("  {}", c))?;
        s.cwrite(clear_style(), &repeat(" ").take(repl_cmd_width - c.width()).collect::<String>())?;
        s.cwriteln(clear_style(), &format!("  {}", h))?;
    }
    Ok(())
}

// TODO: Instead this ugly thing there should be API for doing requests where one of them is adding code to the session.
fn interpret_line<W: Write>(line: &str, out: &mut W, memory: &mut Memory<TypedValue<TypeKey>>, interner: &mut Interner) -> Result<bool> {
    use nom::IResult::*;
    let tokens = match Lexer::new(interner).tokenize(line.as_ref()).1 {
        Done(left, tokens) => if left.is_empty() {
            tokens
        } else {
            out.cwriteln(error_style(), "Lexing failed:")?;
            out.cwriteln(clear_style(), &format!("Left: {}", String::from_utf8_lossy(&left[..(left.len() - 1)])))?;
            out.cwriteln(clear_style(), &format!("Tokens: {:#?}", tokens.tokens))?;
            return Ok(false);
        },
        Error(e) => {
            out.cwriteln(error_style(), "Lexing failed:")?;
            out.cwriteln(clear_style(), &format!("{}", e))?;
            return Ok(false);
        },
        Incomplete(n) => {
            out.cwrite(error_style(), "Lexing needed more: ")?;
            out.cwriteln(clear_style(), &format!("{:?}", n))?;
            return Ok(false);
        },
    };
    let ast = match parse(tokens.borrow()) {
        Done(left, ast) => if left.tokens.is_empty() {
            ast
        } else {
            out.cwriteln(error_style(), "Parsing failed:")?;
            out.cwriteln(clear_style(), &format!("Left: {:#?}", left.tokens))?;
            out.cwriteln(clear_style(), &format!("Ast: {:#?}", ast.expressions))?;
            return Ok(false);
        }
        Error(e) => {
            out.cwriteln(error_style(), "Parsing failed:")?;
            out.cwriteln(clear_style(), &format!("{}", e))?;
            return Ok(false);
        },
        Incomplete(n) => {
            out.cwrite(error_style(), "Parsing needed more: ")?;
            out.cwriteln(clear_style(), &format!("{:?}", n))?;
            return Ok(false);
        },
    };
    let mut ast = match typecheck(ast, interner) {
        Ok(ast) => ast,
        Err(e) => {
            out.cwriteln(error_style(), "Type checking failed:")?;
            out.cwriteln(clear_style(), &format!("{:?}", e))?;
            return Ok(false);
        }
    };
    match interpret_noscope(&ast.expressions, memory, interner, &mut ast.ctx) {
        Ok(value) => {
            match value.value().display(interner) {
                Ok(val) => {
                    out.cwriteln(clear_style(), &format!("{}", val))?;
                    Ok(true)
                },
                Err(e) => {
                    out.cwriteln(error_style(), "Interpreting failed:")?;
                    out.cwriteln(clear_style(), &format!("{}", e))?;
                    Ok(false)
                }
            }
        },
        Err(e) => {
            out.cwriteln(error_style(), "Interpreting failed:")?;
            out.cwriteln(clear_style(), &format!("{}", e))?;
            Ok(false)
        },
    }
}

fn run(args: ArgMatches) -> Result<()> {
    let stdin = stdin();
    let mut out = stdout().into_raw_mode()?;

    let mut history = History::new();
    let mut memory = Memory::<TypedValue<TypeKey>>::new();
    memory.start_scope();
    let mut interner = Interner::new();
    out.cwrite(logo_theme(), &format!("{}", fancy_plain(&args, "🍄", "~")))?;
    out.cwriteln(welcome_theme(), &format!(" Welcome to the repl for Darluca {}!", crate_version!()))?;
    print_repl_symbol(&mut out, &args)?;
    for c in stdin.keys() {
        use self::Key::*;
        match c? {
            Char('\n') => {
                out.cwriteln(clear_style(), "")?;
                if !history.current().trim().is_empty() {
                    if history.current().starts_with(":") {
                        match &history.current().trim()[1..] {
                            "q" => return Ok(()),
                            "h" => print_help(&mut out, &args)?,
                            "c" => {
                                memory.clear();
                                memory.start_scope();
                                out.cwrite(note_style(), fancy_plain(&args, "⚠", "!"))?;
                                out.cwriteln(note_style(), " Cleared all code from the session")?
                            },
                            l if l.starts_with("t") => {
                                if l.trim().len() > 1 {
                                    let l = &l[2..];
                                    // TODO: This is so bad code.
                                    let val = memory.get(&Identifier(interner.intern(l))).unwrap();
                                    out.cwriteln(clear_style(), &format!("{}", val.ty().display(&interner).unwrap()))?;
                                } else {
                                    out.cwriteln(info_style(), &format!("{} To get a type you need a variable:", fancy_plain(&args, "🛈", "i")))?;
                                    out.cwriteln(clear_style(), ":c <variable>")?;
                                }
                            },
                            _ => out.cwriteln(error_style(), "Unknown repl command")?,
                        }
                    } else {
                        interpret_line(history.current(), &mut out, &mut memory, &mut interner)?;
                    }
                    history.proceed();
                }
                print_repl_symbol(&mut out, &args)?;
            },
            Char(c) => {
                history.write(c);
                write!(out, "{}", c)?;
                out.flush()?;
            },
            Down => if history.go_forwards() {
                update_pos(&mut out, 0, &args)?;
                out.cwrite(clear_style(), &format!("{}", history.current()))?;
                out.flush()?;
            },
            Up => if history.go_backwards() {
                update_pos(&mut out, 0, &args)?;
                out.cwrite(clear_style(), &format!("{}", history.current()))?;
                out.flush()?;
            },
            Backspace => if history.erase().is_some() {
                update_pos(&mut out, history.current().width(), &args)?;
                out.flush()?;
            },
            PageUp => {
                history.to_history();
                update_pos(&mut out, 0, &args)?;
                out.cwrite(clear_style(), &format!("{}", history.current()))?;
                out.flush()?;
            },
            PageDown => {
                history.to_future();
                update_pos(&mut out, 0, &args)?;
                out.cwrite(clear_style(), &format!("{}", history.current()))?;
                out.flush()?;
            },
            Esc => break,
            Ctrl('c') => break,
            // TODO: Implement pasting: Ctrl('v') => ,
            _ => {},
        }
    }
    Ok(())
}

fn update_pos<W: Write>(out: &mut W, offset: usize, args: &ArgMatches) -> Result<()> {
    let place = repl_symbol(&args).width() + offset;
    write!(out, "{}{}{}", cursor::Left(!0), cursor::Right(place as u16), clear::AfterCursor)?;
    Ok(())
}