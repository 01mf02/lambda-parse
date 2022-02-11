//! Parser for the Dedukti file format
#![no_std]

extern crate alloc;

pub mod lex;
pub mod term;
pub mod term2;
pub mod term3;
pub mod term4;
pub mod term5;

pub use lex::Token;
pub use term::Term;

pub fn lex(s: &str) -> impl Iterator<Item = Token<&str>> {
    use logos::Logos;
    Token::lexer(s)
}

use alloc::vec::Vec;
use core::fmt::{self, Display};

#[derive(Clone, Debug)]
pub struct Abst<V, Tm> {
    pub bnd: Vec<V>,
    pub app: Tm,
}

impl<V, Tm> Abst<V, Tm> {
    fn new(bnd: Vec<V>, tm: Tm) -> Self {
        Self { bnd, app: tm }
    }
}

impl<V: Display, Tm: Display> Display for Abst<V, Tm> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.bnd.iter().try_for_each(|b| write!(f, "{} => ", b))?;
        self.app.fmt(f)
    }
}

#[derive(Clone, Debug)]
pub struct App<H, T = H>(H, Vec<T>);

impl<H, T> App<H, T> {
    pub fn new(tm: H) -> Self {
        Self(tm, Vec::new())
    }
}

impl<H: Display, T: Display> Display for App<H, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !self.1.is_empty() {
            write!(f, "(")?;
        }
        self.0.fmt(f)?;
        self.1.iter().try_for_each(|a| write!(f, " {}", a))?;
        if !self.1.is_empty() {
            write!(f, ")")?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct AppSelf<H>(pub H, pub Vec<Self>);

impl<H> AppSelf<H> {
    pub fn new(head: H) -> Self {
        Self(head, Vec::new())
    }
}

impl<H: Display> Display for AppSelf<H> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !self.1.is_empty() {
            write!(f, "(")?;
        }
        self.0.fmt(f)?;
        self.1.iter().try_for_each(|a| write!(f, " {}", a))?;
        if !self.1.is_empty() {
            write!(f, ")")?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    ExpectedIdent,
    ExpectedIdentOrLPar,
    AnonymousLambda,
    AbstractionWithoutRhs,
    UnclosedLPar,
}

type Result<T> = core::result::Result<T, Error>;

enum Loop<T, C = ()> {
    Return(T),
    Continue(C),
}
