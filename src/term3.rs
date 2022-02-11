//! Terms that are either an inlined atom or
//! a combinator that handles multiple abstractions & applications,
//! where an application has a term head and combinator arguments. 
//! The combinator constructor of terms is boxed.

use crate::{App, Abst, Token, Error, Result};
use alloc::{boxed::Box, vec::Vec};
use core::fmt::{self, Display};

#[derive(Clone, Debug)]
pub struct Comb<C, V>(Abst<V, App<Term<C, V>, Self>>);

#[derive(Clone, Debug)]
pub enum Term<C, V> {
    Const(C),
    Var(usize),
    Comb(Box<Comb<C, V>>),
}

impl<C: Display, V: Display> Display for Comb<C, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<C: Display, V: Display> Display for Term<C, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Const(c) => c.fmt(f),
            Self::Var(v) => v.fmt(f),
            Self::Comb(comb) => comb.fmt(f),
        }
    }
}

/// A left parenthesis possibly preceded by an abstraction and/or an application.
#[derive(Debug)]
struct LPar<C, V> {
    bnd: Vec<V>,
    app: Option<App<Term<C, V>, Comb<C, V>>>,
}

#[derive(Debug)]
pub(crate) enum State<S, C, V> {
    /// nothing
    Init(Vec<V>),
    /// `s` (we do not know at this point whether it is a variable or a constant)
    Symb(Vec<V>, S),

    ATerm(Comb<C, V>),

    Term(Term<C, V>, Token<()>),
}

impl<S, C, V> Default for State<S, C, V> {
    fn default() -> Self {
        Self::Init(Vec::new())
    }
}

#[derive(Debug)]
pub struct Ctx<C, V> {
    stack: Vec<LPar<C, V>>,
}

impl<C, V> Default for Ctx<C, V> {
    fn default() -> Self {
        Self { stack: Vec::new() }
    }
}

impl<C, V> LPar<C, V> {
    fn close(mut self, mut comb: Comb<C, V>) -> Comb<C, V> {
        match self.app {
            None if self.bnd.is_empty() => comb,
            None => {
                self.bnd.append(&mut comb.0.bnd);
                Comb(Abst {
                    bnd: self.bnd,
                    app: comb.0.app,
                })
            }
            /*
            Some(mut app) if comb.0.bnd.is_empty() => {
                app.1.push(comb.0.app.0);
                app.1.append(&mut comb.0.app.1);
                Comb(Abst { bnd: self.bnd, app })
            }
            */
            Some(mut app) => {
                app.1.push(comb);
                Comb(Abst { bnd: self.bnd, app })
            }
        }
    }
}

type OToken<S> = Option<Token<S>>;

type Loop<S, C, V> = crate::Loop<State<S, C, V>, Vec<V>>;

impl<S: Into<C> + Into<V>, C, V> State<S, C, V> {
    pub fn parse<I>(self, ctx: &mut Ctx<C, V>, iter: &mut I) -> Result<Self>
    where
        I: Iterator<Item = Token<S>>,
    {
        match self.resume(ctx, iter)? {
            Loop::Continue(bnd) => Self::init(bnd, ctx, iter),
            Loop::Return(ret) => Ok(ret),
        }
    }

    fn resume<I>(self, ctx: &mut Ctx<C, V>, iter: &mut I) -> Result<Loop<S, C, V>>
    where
        I: Iterator<Item = Token<S>>,
    {
        match self {
            Self::Init(bnd) => Ok(Loop::Continue(bnd)),
            Self::Symb(bnd, s1) => Self::ident(bnd, s1, ctx, iter),
            Self::ATerm(app) => Self::aterm(app, ctx, iter.next(), iter),
            Self::Term(_, _) => Ok(Loop::Return(self)),
        }
    }

    pub fn init<I>(mut bnd: Vec<V>, ctx: &mut Ctx<C, V>, iter: &mut I) -> Result<Self>
    where
        I: Iterator<Item = Token<S>>,
    {
        while let Some(token) = iter.next() {
            match token {
                Token::Ident(s1) => match Self::ident(bnd, s1, ctx, iter)? {
                    Loop::Continue(bnd2) => bnd = bnd2,
                    Loop::Return(ret) => return Ok(ret),
                },
                Token::LPar => {
                    ctx.stack.push(LPar { bnd, app: None });
                    bnd = Vec::new();
                }
                _ => return Err(Error::ExpectedIdentOrLPar),
            }
        }
        Ok(Self::Init(bnd))
    }

    fn ident<I>(mut bnds: Vec<V>, s1: S, ctx: &mut Ctx<C, V>, iter: &mut I) -> Result<Loop<S, C, V>>
    where
        I: Iterator<Item = Token<S>>,
    {
        match iter.next() {
            None => return Ok(Loop::Return(Self::Symb(bnds, s1))),
            Some(Token::Arrow) => bnds.push(s1.into()),
            Some(Token::Ident(s2)) => {
                let comb = Comb(Abst {
                    bnd: bnds,
                    app: App::new(Term::Const(s1.into())),
                });
                match Self::aterm(comb, ctx, Some(Token::Ident(s2)), iter)? {
                    Loop::Continue(bnd2) => bnds = bnd2,
                    Loop::Return(ret) => return Ok(Loop::Return(ret)),
                }
            }
            Some(Token::LPar) => {
                let app = Some(App::new(Term::Const(s1.into())));
                ctx.stack.push(LPar { bnd: bnds, app });
                bnds = Vec::new();
            }
            Some(Token::RPar) => {
                let comb = Comb(Abst::new(bnds, App::new(Term::Const(s1.into()))));
                match Self::aterm(comb, ctx, Some(Token::RPar), iter)? {
                    Loop::Continue(bnds) => return Ok(Loop::Continue(bnds)),
                    Loop::Return(ret) => return Ok(Loop::Return(ret)),
                }
            }
            Some(tok) => {
                if ctx.stack.is_empty() {
                    return Ok(Loop::Return(Self::Term(
                        Term::Const(s1.into()),
                        tok.map(|_| ()),
                    )));
                } else {
                    return Err(Error::UnclosedLPar);
                }
            }
        }
        Ok(Loop::Continue(bnds))
    }

    fn aterm(
        mut comb: Comb<C, V>,
        ctx: &mut Ctx<C, V>,
        mut token: OToken<S>,
        iter: &mut impl Iterator<Item = Token<S>>,
    ) -> Result<Loop<S, C, V>> {
        while let Some(tok) = token.take() {
            match tok {
                Token::Ident(s) => match iter.next() {
                    None => comb.0.app.1.push(Comb(Abst::new(Vec::new(), App::new(Term::Const(s.into()))))),
                    Some(other) => {
                        comb.0.app.1.push(Comb(Abst::new(Vec::new(), App::new(Term::Const(s.into())))));
                        token = Some(other);
                        continue;
                    }
                },
                Token::Arrow => return Err(Error::AnonymousLambda),
                Token::LPar => {
                    ctx.stack.push(LPar {
                        bnd: comb.0.bnd,
                        app: Some(comb.0.app),
                    });
                    return Ok(Loop::Continue(Vec::new()));
                }
                Token::RPar => match ctx.stack.pop() {
                    // if we found a matching left parenthesis
                    Some(lpar) => comb = lpar.close(comb),
                    None => return Ok(Loop::Return(State::Term(Term::Comb(Box::new(comb)), Token::RPar))),
                },
                tok => {
                    if ctx.stack.is_empty() {
                        return Ok(Loop::Return(State::Term(Term::Comb(Box::new(comb)), tok.map(|_| ()))));
                    } else {
                        return Err(Error::UnclosedLPar);
                    }
                }
            }
            token = iter.next();
        }
        Ok(Loop::Return(State::ATerm(comb)))
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
        match State::init(Vec::new(), ctx, iter)? {
            State::Init(_) => Err(Error::ExpectedIdentOrLPar),
            // TODO: handle this case
            State::Symb(..) | State::ATerm(..) => panic!("expected input"),
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
