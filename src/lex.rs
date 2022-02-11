use core::fmt::{self, Display};
use logos::{Lexer, Logos};

#[derive(Logos, Debug, PartialEq)]
#[logos(type S = &str)]
pub enum Token<S> {
    #[token("(")]
    LPar,

    #[token(")")]
    RPar,

    #[token("->")]
    Arrow,

    #[token(".")]
    Dot,

    #[regex("[a-zA-Z0-9_!?][a-zA-Z0-9_!?']*")]
    #[token("{|", ident)]
    Ident(S),

    // Logos requires one token variant to handle errors,
    // it can be named anything you wish.
    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[error]
    Error,
}

impl<S> Token<S> {
    pub fn map<T>(self, f: impl Fn(S) -> T) -> Token<T> {
        use Token::*;
        match self {
            LPar => LPar,
            RPar => RPar,
            Arrow => Arrow,
            Dot => Dot,
            Ident(s) => Ident(f(s)),
            Error => Error,
        }
    }
}

impl<S: Display> Display for Token<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::LPar => "(".fmt(f),
            Self::RPar => ")".fmt(f),
            Self::Arrow => "->".fmt(f),
            Self::Dot => ".".fmt(f),
            Self::Ident(s) => s.fmt(f),
            Self::Error => Err(Default::default()),
        }
    }
}

fn ident<'s>(lex: &mut Lexer<'s, Token<&'s str>>) -> Option<&'s str> {
    let len = lex.remainder().find("|}")?;
    lex.bump(len + 2); // include len of `|}`
    Some(lex.slice())
}
