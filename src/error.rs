use std::{
    borrow::Cow,
    cmp::Ordering::*,
    collections::HashSet,
    error::Error as ErrorTrait,
    fmt::{self, Display},
    hash::Hash,
    iter,
};

use either::Either;

use super::input::{Input, Position, Ranged};

#[derive(Clone, Copy)]
pub struct Unexpected<T>(T);
#[inline(always)]
pub fn unexpected<T>(token: T) -> Unexpected<T> {
    Unexpected(token)
}

#[derive(Clone, Copy)]
pub struct Expected<T>(T);
#[inline(always)]
pub fn expected<T>(token: T) -> Expected<T> {
    Expected(token)
}

#[derive(Clone, Copy)]
pub struct Message<T>(T);
#[inline(always)]
pub fn message<T>(message: T) -> Message<T> {
    Message(message)
}

pub enum StdErrorMessage<U, E, M> {
    Unexpected(U),
    Expected(E),
    Message(M),
}
impl<U, E, M, T: Into<U>> From<Unexpected<T>> for StdErrorMessage<U, E, M> {
    #[inline(always)]
    fn from(from: Unexpected<T>) -> Self {
        StdErrorMessage::Unexpected(from.0.into())
    }
}
impl<U, E, M, T: Into<E>> From<Expected<T>> for StdErrorMessage<U, E, M> {
    #[inline(always)]
    fn from(from: Expected<T>) -> Self {
        StdErrorMessage::Expected(from.0.into())
    }
}
impl<U, E, M, T: Into<M>> From<Message<T>> for StdErrorMessage<U, E, M> {
    #[inline(always)]
    fn from(from: Message<T>) -> Self {
        StdErrorMessage::Message(from.0.into())
    }
}

pub struct EndOfInput;
pub struct Token<T>(T);
pub fn token<T>(token: T) -> Token<T> {
    Token(token)
}
pub struct Format<T>(T);
pub fn format<T>(format: T) -> Format<T> {
    Format(format)
}
pub struct Error<T>(T);
pub fn error<T>(error: T) -> Error<T> {
    Error(error)
}

#[derive(Hash, PartialEq, Eq)]
pub enum StdToken<T, L> {
    EndOfInput,
    Token(T),
    Format(L),
}
impl<T, L> From<EndOfInput> for StdToken<T, L> {
    fn from(_: EndOfInput) -> Self {
        StdToken::EndOfInput
    }
}
impl<U, T: From<U>, L> From<Token<U>> for StdToken<T, L> {
    fn from(from: Token<U>) -> Self {
        StdToken::Token(from.0.into())
    }
}
impl<U, T, L: From<U>> From<Format<U>> for StdToken<T, L> {
    fn from(from: Format<U>) -> Self {
        StdToken::Format(from.0.into())
    }
}
impl<T: Display, L: Display> Display for StdToken<T, L> {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EndOfInput => "end of input".fmt(f),
            Self::Token(token) => token.fmt(f),
            Self::Format(err) => err.fmt(f),
        }
    }
}

pub enum StdMessage<M, E> {
    Format(M),
    Error(E),
}
impl<T, M: From<T>, E> From<Format<T>> for StdMessage<M, E> {
    fn from(from: Format<T>) -> Self {
        StdMessage::Format(from.0.into())
    }
}
impl<T, M, E: From<T>> From<Error<T>> for StdMessage<M, E> {
    fn from(from: Error<T>) -> Self {
        StdMessage::Error(from.0.into())
    }
}
impl<M: Display, E: Display> Display for StdMessage<M, E> {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Format(fmt) => fmt.fmt(f),
            Self::Error(err) => err.fmt(f),
        }
    }
}

pub trait ParseError<I: Input> {
    type Message: From<I::Message>;
    type Warn;

    fn new() -> Self;
    fn add(&mut self, start: Option<I::Position>, end: I::Position) -> bool;

    fn clear_expected(&mut self);
    fn set(&mut self, message: Self::Message);

    fn warn(&mut self, start: Option<I::Position>, end: I::Position, warn: Self::Warn);

    fn save(&mut self);
}

pub struct StdParseError<Token, Position> {
    start: Option<Position>,
    end: Option<Position>,
    unexpected: Option<StdToken<Token, Cow<'static, str>>>,
    expected: HashSet<StdToken<Token, Cow<'static, str>>>,
    messages: HashSet<Cow<'static, str>>,
    errors: Vec<Box<dyn ErrorTrait>>,
    recovered: Vec<Ranged<Position, StdRecovered<Token>>>,
}
pub enum StdRecovered<Token> {
    Trivial {
        unexpected: Option<StdToken<Token, Cow<'static, str>>>,
        expected: Box<[StdToken<Token, Cow<'static, str>>]>,
    },
    Fail {
        messages: Box<[Cow<'static, str>]>,
        errors: Box<[Box<dyn ErrorTrait>]>,
    },
    Warn {
        message: StdMessage<Cow<'static, str>, Box<dyn ErrorTrait>>,
    },
}

pub type StdParseErrorFor<Token> = StdErrorMessage<
    StdToken<Token, Cow<'static, str>>,
    StdToken<Token, Cow<'static, str>>,
    StdMessage<Cow<'static, str>, Box<dyn ErrorTrait>>,
>;
impl<Token, Position> StdParseError<Token, Position> {
    #[inline(always)]
    pub fn is_message(&self) -> bool {
        !self.messages.is_empty() || !self.errors.is_empty()
    }

    #[inline(always)]
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = Ranged<&'a Position, impl Display + 'a>>
    where
        Token: Display,
    {
        let recovered = self.recovered.iter().map(|Ranged { start, end, item }| Ranged {
            start: start.as_ref(),
            end,
            item: match item {
                StdRecovered::Trivial { unexpected, expected } => {
                    StdParseErrorDisplay::Trivial { unexpected: unexpected.as_ref(), expected: Either::Left(expected) }
                },
                StdRecovered::Fail { messages, errors } => {
                    StdParseErrorDisplay::Fail { messages: Either::Left(messages), errors }
                },
                StdRecovered::Warn { message: StdMessage::Error(err) } => {
                    StdParseErrorDisplay::Warn { message: StdMessage::Error(err.as_ref()) }
                },
                StdRecovered::Warn { message: StdMessage::Format(fmt) } => {
                    StdParseErrorDisplay::Warn { message: StdMessage::Format(fmt) }
                },
            },
        });
        match &self.end {
            None => Either::Left(recovered),
            Some(end) => Either::Right(recovered.chain(iter::once_with(|| Ranged {
                start: self.start.as_ref(),
                end,
                item: if self.is_message() {
                    StdParseErrorDisplay::Fail { messages: Either::Right(&self.messages), errors: &self.errors }
                } else {
                    StdParseErrorDisplay::Trivial {
                        unexpected: self.unexpected.as_ref(),
                        expected: Either::Right(&self.expected),
                    }
                },
            }))),
        }
    }
}

impl<I: Input> ParseError<I> for Option<StdParseError<I::Token, I::Position>>
where
    I::Token: Hash + Eq,
    StdParseErrorFor<I::Token>: From<I::Message>,
{
    type Message = StdParseErrorFor<I::Token>;
    type Warn = StdMessage<Cow<'static, str>, Box<dyn ErrorTrait>>;

    #[inline(always)]
    fn new() -> Self {
        Some(<StdParseError<_, _> as ParseError<I>>::new())
    }

    #[inline(always)]
    fn add(&mut self, start: Option<<I as Input>::Position>, end: <I as Input>::Position) -> bool {
        if let Some(err) = self {
            <StdParseError<_, _> as ParseError<I>>::add(err, start, end)
        } else {
            false
        }
    }

    #[inline(always)]
    fn clear_expected(&mut self) {
        if let Some(err) = self {
            <StdParseError<_, _> as ParseError<I>>::clear_expected(err)
        }
    }

    #[inline(always)]
    fn set(&mut self, messages: Self::Message) {
        if let Some(err) = self {
            <StdParseError<_, _> as ParseError<I>>::set(err, messages)
        }
    }

    #[inline(always)]
    fn warn(&mut self, start: Option<<I as Input>::Position>, end: <I as Input>::Position, warn: Self::Warn) {
        if let Some(err) = self {
            <StdParseError<_, _> as ParseError<I>>::warn(err, start, end, warn)
        }
    }

    #[inline(always)]
    fn save(&mut self) {
        if let Some(err) = self {
            <StdParseError<_, _> as ParseError<I>>::save(err)
        }
    }
}

impl<I: Input> ParseError<I> for StdParseError<I::Token, I::Position>
where
    I::Token: Hash + Eq,
    StdParseErrorFor<I::Token>: From<I::Message>,
{
    type Message = StdParseErrorFor<I::Token>;
    type Warn = StdMessage<Cow<'static, str>, Box<dyn ErrorTrait>>;

    #[inline(always)]
    fn new() -> Self {
        Self {
            start: None,
            end: None,
            unexpected: None,
            expected: HashSet::new(),
            messages: HashSet::new(),
            errors: vec![],
            recovered: vec![],
        }
    }

    #[inline(always)]
    fn add(&mut self, start: Option<I::Position>, end: I::Position) -> bool {
        match &self.end {
            None => {
                self.start = start;
                self.end = Some(end);
                true
            },
            Some(old_end) => match old_end.offset().cmp(&end.offset()) {
                Greater => false,
                Less => {
                    self.start = start;
                    self.end = Some(end);
                    self.unexpected = None;
                    self.expected.clear();
                    self.messages.clear();
                    true
                },
                Equal => match (&self.start, start) {
                    (None, None) => true,
                    (Some(_), None) => false,
                    (None, Some(start)) => {
                        self.start = Some(start);
                        self.unexpected = None;
                        self.expected.clear();
                        self.messages.clear();
                        true
                    },
                    (Some(old_start), Some(start)) => match old_start.offset().cmp(&start.offset()) {
                        Greater => false,
                        Equal => true,
                        Less => {
                            self.start = Some(start);
                            self.unexpected = None;
                            self.expected.clear();
                            self.messages.clear();
                            true
                        },
                    },
                },
            },
        }
    }

    #[inline(always)]
    fn clear_expected(&mut self) {
        self.expected.clear()
    }

    #[inline(always)]
    fn set(&mut self, message: Self::Message) {
        match message {
            StdErrorMessage::Unexpected(unexpected) => {
                if !self.is_message() {
                    self.unexpected = Some(unexpected);
                }
            },
            StdErrorMessage::Expected(expected) => {
                if !self.is_message() {
                    self.expected.insert(expected);
                }
            },
            StdErrorMessage::Message(StdMessage::Format(fmt)) => {
                self.unexpected = None;
                self.expected.clear();
                self.messages.insert(fmt);
            },
            StdErrorMessage::Message(StdMessage::Error(err)) => {
                self.unexpected = None;
                self.expected.clear();
                self.errors.push(err);
            },
        }
    }

    #[inline(always)]
    fn warn(&mut self, start: Option<I::Position>, end: I::Position, warn: Self::Warn) {
        self.recovered.push(Ranged { start, end, item: StdRecovered::Warn { message: warn } })
    }

    #[inline(always)]
    fn save(&mut self) {
        match self.end.take() {
            None => (),
            Some(end) => {
                if self.messages.is_empty() {
                    self.recovered.push(Ranged {
                        start: self.start.take(),
                        end,
                        item: StdRecovered::Trivial {
                            unexpected: self.unexpected.take(),
                            expected: self.expected.drain().collect(),
                        },
                    });
                } else {
                    self.recovered.push(Ranged {
                        start: self.start.take(),
                        end,
                        item: StdRecovered::Fail {
                            messages: self.messages.drain().collect(),
                            errors: self.errors.drain(..).collect(),
                        },
                    })
                }
            },
        }
    }
}

#[inline(always)]
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

pub enum StdParseErrorDisplay<'a, Token> {
    Trivial {
        unexpected: Option<&'a StdToken<Token, Cow<'static, str>>>,
        expected: Either<&'a [StdToken<Token, Cow<'static, str>>], &'a HashSet<StdToken<Token, Cow<'static, str>>>>,
    },
    Fail {
        messages: Either<&'a [Cow<'static, str>], &'a HashSet<Cow<'static, str>>>,
        errors: &'a [Box<dyn ErrorTrait>],
    },
    Warn {
        message: StdMessage<&'a str, &'a dyn ErrorTrait>,
    },
}
impl<'a, Token: Display> Display for StdParseErrorDisplay<'a, Token> {
    #[inline(always)]
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Trivial { unexpected: Some(unexpected), expected } => {
                write!(fmt, "error: unexpected {}", unexpected)?;
                match expected {
                    Either::Left(expected) => {
                        if !expected.is_empty() {
                            write!(fmt, ", expecting ")?;
                            join(fmt, expected.iter(), "or")?;
                        }
                    },
                    Either::Right(expected) => {
                        if !expected.is_empty() {
                            write!(fmt, ", expecting ")?;
                            join(fmt, expected.iter(), "or")?;
                        }
                    },
                }
                Ok(())
            },
            Self::Trivial { unexpected: None, expected } => {
                write!(fmt, "error: expecting ")?;
                match expected {
                    Either::Left(expected) => join(fmt, expected.iter(), "or"),
                    Either::Right(expected) => join(fmt, expected.iter(), "or"),
                }
            },
            Self::Fail { messages, errors } => {
                write!(fmt, "error: ")?;
                match messages {
                    Either::Left(messages) => {
                        join(fmt, messages.iter().map(Either::Left).chain(errors.iter().map(Either::Right)), "and")
                    },
                    Either::Right(messages) => {
                        join(fmt, messages.iter().map(Either::Left).chain(errors.iter().map(Either::Right)), "and")
                    },
                }
            },
            Self::Warn { message } => write!(fmt, "warning: {}", message),
        }
    }
}
