use {
    super::input::Input,
    std::{
        fmt,
        fmt::{Debug, Display},
    },
};

pub trait CustomBuilder {
    type To: std::error::Error;
    fn merge(self, other: Self) -> Self;
    fn label(self, label: impl Display + 'static) -> Self;
    fn calc(self) -> Self::To;

    #[inline]
    fn label_with<T: Display>(self, label: impl Fn() -> T + 'static) -> Self
    where
        Self: Sized,
    {
        self.label(LazyDisplay(label))
    }
}
pub type Nil = std::convert::Infallible;
impl CustomBuilder for Nil {
    type To = Nil;
    fn merge(self, _: Self) -> Self {
        unreachable!()
    }
    fn label(self, _: impl Display + 'static) -> Self {
        unreachable!()
    }
    fn calc(self) -> Nil {
        unreachable!()
    }
}

pub struct Pos<P, T> {
    index: usize,
    begin: P,
    end: Option<P>,
    body: T,
}
impl<P: Display, T: Display> Display for Pos<P, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if let Some(end) = &self.end {
            write!(fmt, "{} at {}..{}", self.body, self.begin, end)
        } else {
            write!(fmt, "{} at {}", self.body, self.begin)
        }
    }
}
impl<P: Debug, T: Debug> Debug for Pos<P, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if let Some(end) = &self.end {
            write!(fmt, "{:?} at {:?}..{:?}", self.body, self.begin, end)
        } else {
            write!(fmt, "{:?} at {:?}", self.body, self.begin)
        }
    }
}
impl<P, T: CustomBuilder> Pos<P, T> {
    pub fn label(self, label: impl Display + 'static) -> Self {
        Pos { body: self.body.label(label), ..self }
    }
    pub fn label_with<L: Display>(self, label: impl Fn() -> L + 'static) -> Self {
        Pos { body: self.body.label_with(label), ..self }
    }
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Pos<P, U> {
        let Pos { index, begin, end, body } = self;
        Pos { index, begin, end, body: f(body) }
    }
}
impl<P: Ord, T: CustomBuilder> Pos<P, T> {
    pub fn merge(self, other: Self) -> Self {
        use std::cmp::Ordering::*;
        match self.index.cmp(&other.index) {
            Greater => self,
            Less => other,
            Equal => Self {
                index: self.index,
                body: self.body.merge(other.body),
                begin: self.begin.min(other.begin),
                end: match (self.end, other.end) {
                    (None, None) => None,
                    (None, Some(end)) | (Some(end), None) => Some(end),
                    (Some(end1), Some(end2)) => Some(end1.max(end2)),
                },
            },
        }
    }
    pub fn or_merge(self, other: Option<Self>) -> Self {
        match other {
            Some(other) => self.merge(other),
            None => self,
        }
    }
}

pub enum Builder<M: CustomBuilder> {
    Trivial { unexpect: Option<Box<dyn Display + 'static>>, expect: LazyTree<Box<dyn Display + 'static>> },
    Message(LazyTree<Box<dyn Display + 'static>>),
    Custom(M),
}
impl<M: CustomBuilder> CustomBuilder for Builder<M> {
    type To = Messages<M::To>;
    fn merge(self, other: Self) -> Self {
        match (self, other) {
            (Self::Trivial { unexpect: u1, expect: e1 }, Self::Trivial { unexpect: u2, expect: e2 }) => {
                Self::Trivial { unexpect: u1.or(u2), expect: LazyTree::Merge(Box::new(e1), Box::new(e2)) }
            },
            (Self::Trivial { .. }, item) => item,
            (Self::Custom(c1), Self::Custom(c2)) => Self::Custom(c1.merge(c2)),
            (item, _) => item,
        }
    }
    fn calc(self) -> Self::To {
        match self {
            Builder::Trivial { unexpect, expect } => Messages::Trivial { unexpect: unexpect.map(|u| format!("{}", u)), expect: expect.merge() },
            Builder::Message(msg) => Messages::Message(msg.merge()),
            Builder::Custom(m) => Messages::Custom(m.calc()),
        }
    }
    #[inline]
    fn label(self, label: impl Display + 'static) -> Self {
        match self {
            Self::Trivial { unexpect, expect: _ } => Self::Trivial { unexpect, expect: LazyTree::Leaf(Box::new(label)) },
            Self::Custom(c) => Self::Custom(c.label(label)),
            o => o,
        }
    }
}
struct LazyDisplay<F>(F);
impl<T: Display, F: Fn() -> T> Display for LazyDisplay<F> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0().fmt(f)
    }
}
impl<M: CustomBuilder> Builder<M> {
    #[inline]
    pub fn at<I: Input>(self, index: usize, begin: I::Pos, end: Option<I::Pos>) -> LazyError<I, M> {
        Pos { index, begin, end, body: self }
    }

    #[inline]
    pub fn expected_eoi() -> Self {
        Self::Trivial { unexpect: None, expect: LazyTree::Leaf(Box::new("eoi")) }
    }
    #[inline]
    pub fn unexpected_eoi() -> Self {
        Self::Trivial { unexpect: Some(Box::new("eoi")), expect: LazyTree::Empty }
    }
    #[inline]
    pub fn unexpected(token: impl Display + 'static) -> Self {
        Self::Trivial { expect: LazyTree::Empty, unexpect: Some(Box::new(token)) }
    }
    #[inline]
    pub fn unexpected_with<T: Display>(token: impl Fn() -> T + 'static) -> Self {
        Self::unexpected(LazyDisplay(token))
    }
    #[inline]
    pub fn message(msg: impl Display + 'static) -> Self {
        Self::Message(LazyTree::Leaf(Box::new(msg)))
    }
    #[inline]
    pub fn message_with<T: Display>(msg: impl Fn() -> T + 'static) -> Self {
        Self::message(LazyDisplay(msg))
    }
    #[inline]
    pub fn custom(msg: M) -> Self {
        Self::Custom(msg)
    }
}

pub type LazyError<I, M> = Pos<<I as Input>::Pos, Builder<M>>;
pub type Error<I, M> = Pos<<I as Input>::Pos, Messages<M>>;

pub enum LazyTree<T> {
    Empty,
    Leaf(T),
    Merge(Box<Self>, Box<Self>),
}
impl<T: Display> LazyTree<T> {
    fn merge(self) -> Vec<String> {
        let mut res = std::collections::HashSet::new();
        self.collect(&mut res);
        res.into_iter().collect()
    }
    fn collect(self, res: &mut std::collections::HashSet<String>) {
        match self {
            LazyTree::Empty => {},
            LazyTree::Leaf(t) => {
                res.insert(format!("{}", t));
            },
            LazyTree::Merge(left, right) => {
                left.collect(res);
                right.collect(res)
            },
        }
    }
}

#[derive(Debug)]
pub enum Messages<M> {
    Trivial { unexpect: Option<String>, expect: Vec<String> },
    Message(Vec<String>),
    Custom(M),
}
impl<M: Display> Display for Messages<M> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Trivial { unexpect, expect } => {
                let mut before = false;
                if let Some(unexpect) = unexpect {
                    write!(fmt, "unexpected {}", unexpect)?;
                    before = true;
                }
                if !expect.is_empty() {
                    if before {
                        write!(fmt, ", ")?
                    }
                    write!(fmt, "expecting ")?;
                    join(fmt, expect.iter(), "or")?
                }
                Ok(())
            },
            Messages::Message(msgs) => join(fmt, msgs.iter(), "and"),
            Messages::Custom(m) => m.fmt(fmt),
        }
    }
}
impl<M: std::error::Error> std::error::Error for Messages<M> {}

fn join(f: &mut fmt::Formatter, mut xs: impl Iterator<Item = impl Display>, conj: impl Display) -> fmt::Result {
    match xs.next() {
        None => Ok(()),
        Some(x) => {
            x.fmt(f)?;
            match xs.next() {
                None => Ok(()),
                Some(mut buffer) => {
                    for x in xs {
                        write!(f, ", {}", buffer)?;
                        buffer = x;
                    }
                    write!(f, " {} {}", conj, buffer)
                },
            }
        },
    }
}
