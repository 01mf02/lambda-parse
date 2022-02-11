//! Terms as application trees with separate atom type.

use crate::{AppSelf as App, Error, Result, Token, Loop};
use alloc::{boxed::Box, vec::Vec};
use core::fmt::{self, Display};

#[derive(Clone, Debug)]
pub enum Atom<C> {
    Const(C),
    Var(usize),
}

#[derive(Clone, Debug)]
pub enum Term1<C, V> {
    Atom(Atom<C>),
    // Abstraction (`x => t`)
    Abst(V, Box<App<Self>>),
}

pub type Term<C, V> = App<Term1<C, V>>;

impl<C: Display, V: Display> Display for Term1<C, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Atom(a) => a.fmt(f),
            Self::Abst(x, tm) => write!(f, "({} -> {})", x, tm),
        }
    }
}

impl<C: Display> Display for Atom<C> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Const(c) => c.fmt(f),
            Self::Var(v) => v.fmt(f),
        }
    }
}

/// A left parenthesis possibly preceded by an abstraction and/or an application.
#[derive(Debug)]
struct LPar<C, V> {
    app: Option<Term<C, V>>,
}

#[derive(Debug)]
pub(crate) enum State<S, C, V> {
    /// nothing
    Init,
    /// `s` (we do not know at this point whether it is a variable or a constant)
    Symb(S),
    /// possibly `x :`, followed by `t1 ... tn`.
    ATerm(Term<C, V>),

    Term(Term<C, V>, Token<()>),
}

impl<S, C, V> Default for State<S, C, V> {
    fn default() -> Self {
        Self::Init
    }
}

#[derive(Debug)]
enum Cont<C, V> {
    /// `x`, possibly followed by `: ty`, followed by `=>`,
    Abst(V),

    /// possibly `x :`,
    /// possibly followed by `t1 .. tn`,
    /// followed by `(`
    LPar(LPar<C, V>),
}

#[derive(Debug)]
pub struct Ctx<C, V> {
    stack: Vec<Cont<C, V>>,
}

impl<C, V> Default for Ctx<C, V> {
    fn default() -> Self {
        Self { stack: Vec::new() }
    }
}

impl<C, V> Term<C, V> {
    fn reduce(mut self, ctx: &mut Ctx<C, V>) -> (Option<LPar<C, V>>, Self) {
        while let Some(cur) = ctx.stack.pop() {
            match cur {
                Cont::Abst(x) => self = App::new(Term1::Abst(x, self.into())),
                Cont::LPar(lpar) => return (Some(lpar), self),
            }
        }
        (None, self)
    }
}

type OToken<S> = Option<Token<S>>;

impl<S: Into<C> + Into<V>, C, V> State<S, C, V> {
    pub fn parse<I>(self, ctx: &mut Ctx<C, V>, iter: &mut I) -> Result<Self>
    where
        I: Iterator<Item = Token<S>>,
    {
        match self.resume(ctx, iter)? {
            Loop::Continue(()) => Self::init(ctx, iter),
            Loop::Return(ret) => Ok(ret),
        }
    }

    fn resume<I>(self, ctx: &mut Ctx<C, V>, iter: &mut I) -> Result<Loop<Self>>
    where
        I: Iterator<Item = Token<S>>,
    {
        match self {
            Self::Init => Ok(Loop::Continue(())),
            Self::Symb(s1) => Self::ident(s1, ctx, iter),
            Self::ATerm(app) => Self::aterm(app, ctx, iter.next(), iter),
            Self::Term(_, _) => Ok(Loop::Return(self)),
        }
    }

    pub fn init<I>(ctx: &mut Ctx<C, V>, iter: &mut I) -> Result<Self>
    where
        I: Iterator<Item = Token<S>>,
    {
        while let Some(token) = iter.next() {
            match token {
                Token::Ident(s1) => match Self::ident(s1, ctx, iter)? {
                    Loop::Continue(()) => (),
                    Loop::Return(ret) => return Ok(ret),
                },
                Token::LPar => ctx.stack.push(Cont::LPar(LPar { app: None })),
                _ => return Err(Error::ExpectedIdentOrLPar),
            }
        }
        Ok(Self::Init)
    }

    fn ident<I>(s1: S, ctx: &mut Ctx<C, V>, iter: &mut I) -> Result<Loop<Self>>
    where
        I: Iterator<Item = Token<S>>,
    {
        match iter.next() {
            None => return Ok(Loop::Return(Self::Symb(s1))),
            Some(Token::Arrow) => ctx.stack.push(Cont::Abst(s1.into())),
            Some(Token::Ident(s2)) => {
                let app = App::new(Term1::Atom(Atom::Const(s1.into())));
                match Self::aterm(app, ctx, Some(Token::Ident(s2)), iter)? {
                    Loop::Continue(()) => (),
                    Loop::Return(ret) => return Ok(Loop::Return(ret)),
                }
            }
            Some(Token::LPar) => {
                let app = Some(App::new(Term1::Atom(Atom::Const(s1.into()))));
                ctx.stack.push(Cont::LPar(LPar { app }));
            }
            Some(Token::RPar) => {
                let app = App::new(Term1::Atom(Atom::Const(s1.into())));
                match Self::aterm(app, ctx, Some(Token::RPar), iter)? {
                    Loop::Continue(()) => (),
                    Loop::Return(ret) => return Ok(Loop::Return(ret)),
                }
            }
            Some(other) => match App::new(Term1::Atom(Atom::Const(s1.into()))).reduce(ctx) {
                (Some(_lpar), _) => return Err(Error::UnclosedLPar),
                (None, tm) => return Ok(Loop::Return(Self::Term(tm, other.map(|_| ())))),
            },
        }
        Ok(Loop::Continue(()))
    }

    fn aterm(
        mut app: Term<C, V>,
        ctx: &mut Ctx<C, V>,
        mut token: OToken<S>,
        iter: &mut impl Iterator<Item = Token<S>>,
    ) -> Result<Loop<State<S, C, V>>> {
        while let Some(tok) = token.take() {
            match tok {
                Token::Ident(s) => match iter.next() {
                    None => app.1.push(App::new(Term1::Atom(Atom::Const(s.into())))),
                    Some(other) => {
                        app.1.push(App::new(Term1::Atom(Atom::Const(s.into()))));
                        token = Some(other);
                        continue;
                    }
                },
                Token::Arrow => return Err(Error::AnonymousLambda),
                Token::LPar => {
                    ctx.stack.push(Cont::LPar(LPar { app: Some(app) }));
                    return Ok(Loop::Continue(()));
                }
                Token::RPar => match app.reduce(ctx) {
                    // if we found a matching left parenthesis
                    (Some(lpar), tm) => {
                        app = match lpar.app {
                            None => tm,
                            Some(mut app) => {
                                app.1.push(tm);
                                app
                            }
                        }
                    }
                    (None, tm) => return Ok(Loop::Return(State::Term(tm, Token::RPar))),
                },
                tok => match app.reduce(ctx) {
                    (Some(_lpar), _) => return Err(Error::UnclosedLPar),
                    (None, tm) => return Ok(Loop::Return(State::Term(tm, tok.map(|_| ())))),
                },
            }
            token = iter.next();
        }
        Ok(Loop::Return(State::ATerm(app)))
    }
}

impl<C, V> Term<C, V> {
    pub fn parse<S: Into<C> + Into<V>, I>(
        ctx: &mut Ctx<C, V>,
        iter: &mut I,
    ) -> Result<(Self, Token<()>)>
    where
        I: Iterator<Item = Token<S>>,
    {
        match State::init(ctx, iter)? {
            State::Init => Err(Error::ExpectedIdentOrLPar),
            // TODO: handle this case
            State::Symb(_) | State::ATerm(..) => panic!("expected input"),
            State::Term(tm, tok) => Ok((tm, tok)),
        }
    }
}

impl<'s> Term<&'s str, &'s str> {
    pub fn parse_str(s: &'s str) -> Result<Self> {
        let mut ctx = Ctx::default();
        let mut iter = crate::lex(s).chain(core::iter::once(Token::Dot));
        let (tm, tok) = Self::parse(&mut ctx, &mut iter)?;
        assert_eq!(iter.next(), None);
        assert_eq!(tok, Token::Dot);
        Ok(tm)
    }
}

#[test]
fn positive() -> Result<()> {
    Term::parse_str("x -> y -> z")?;
    Term::parse_str("(a b) (c d) e")?;
    Term::parse_str("a (b c) (d e)")?;
    Term::parse_str("((a (((b)))))")?;
    Term::parse_str("a b c")?;
    Ok(())
}

#[test]
fn negative() {
    use Error::*;
    assert_eq!(Term::parse_str("->").unwrap_err(), ExpectedIdentOrLPar);
    assert_eq!(Term::parse_str("x ->").unwrap_err(), ExpectedIdentOrLPar);
    assert_eq!(Term::parse_str("(a ").unwrap_err(), UnclosedLPar);
    assert_eq!(Term::parse_str("(a b ").unwrap_err(), UnclosedLPar);
    assert_eq!(Term::parse_str("a b -> c").unwrap_err(), AnonymousLambda);
}
