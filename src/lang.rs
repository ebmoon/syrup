use egg::{Id, Symbol, Language, FromOp};
use std::{
    convert::Infallible,
    fmt::{self, Debug, Display, Formatter},
    hash::Hash,
    slice,
    str::FromStr,
    vec,
};
use thiserror::Error;

/// Simple language with conditional and match
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub enum Op {
	/// An integer literal
	Int(i32),
	/// A boolean literal
	Bool(bool),
    /// A conditional expression
    If,
    /// A function application
    Match,
    /// An uninterpreted symbol
    Symbol(Symbol),
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::If => "if",
            Self::Match => "match",
			Self::Bool(b) => {
                return write!(f, "{}", b);
            }
            Self::Int(i) => {
                return write!(f, "{}", i);
            }
            Self::Symbol(sym) => {
                return write!(f, "{}", sym);
            }
        };
        f.write_str(s)
    }
}

impl FromStr for Op {
    type Err = Infallible;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let op = match input {
            "if" => Self::If,
            "match" => Self::Match,
            input => input
                .parse()
                .map(Self::Int)
                .or_else(|_| input.parse().map(Self::Bool))
                .unwrap_or_else(|_| Self::Symbol(input.into())),
        };
        Ok(op)
    }
}

/// An abstract syntax tree node representing an operation of type `Op` applied
/// to arguments of type `T`.
///
/// This type implements [`Language`] for arguments of type [`Id`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AstNode<T = Id> {
    operation: Op,
    args: Vec<T>,
}

impl<T> AstNode<T> {
    /// Returns the operation the node represents.
    #[must_use]
    pub fn operation(&self) -> &Op {
        &self.operation
    }

    /// Returns a slice containing the operation's arguments.
    #[must_use]
    pub fn args(&self) -> &[T] {
        &self.args
    }

    /// Returns a slice which allows modifying the operation's arguments.
    #[must_use]
    pub fn args_mut(&mut self) -> &mut [T] {
        &mut self.args
    }

    /// Returns `true` if the operation has no arguments.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.args.is_empty()
    }

    /// Returns the number of arguments the operation has.
    #[must_use]
    pub fn len(&self) -> usize {
        self.args.len()
    }

    /// Converts an `AstNode<Op, T>` into an `AstNode<Op, U>` by applying a
    /// function to each of its arguments.
    #[must_use]
    pub fn map<U, F>(self, f: F) -> AstNode<U>
    where
        F: FnMut(T) -> U,
    {
        AstNode {
            operation: self.operation,
            args: self.args.into_iter().map(f).collect(),
        }
    }

    /// Returns an iterator over the operation's arguments.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.into_iter()
    }

    /// Returns an iterator that allows modifying the operation's arguments.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.into_iter()
    }

    /// Returns a reference to the node's operation and a slice of the operation's arguments.
    #[must_use]
    pub fn as_parts(&self) -> (&Op, &[T]) {
        (&self.operation, &self.args)
    }

    /// Decomposes the node into the operation and its arguments.
    #[must_use]
    pub fn into_parts(self) -> (Op, Vec<T>) {
        (self.operation, self.args)
    }
}

impl<T> AstNode<T> {
    /// Creates a node with the given operation and arguments.
    ///
    /// See also [`AstNode::into_parts`].
    ///
    /// # Panics
    ///
    /// Panics if the number of arguments does not match the
    /// [`Arity`] of the operation.
    #[must_use]
    pub fn new<I>(operation: Op, args: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        let args: Vec<_> = args.into_iter().collect();
        Self { operation, args }
    }

    /// Creates a leaf node with the given operation.
    ///
    /// # Panics
    ///
    /// Panics if the [`Arity`] of the operation cannot be zero.
    #[must_use]
    pub fn leaf(operation: Op) -> Self {
        Self::new(operation, [])
    }
}

impl<T> AsRef<[T]> for AstNode<T> {
    /// Returns a reference to the operation's arguments.
    fn as_ref(&self) -> &[T] {
        self.args()
    }
}

impl<T> AsRef<Op> for AstNode<T> {
    /// Returns a reference to the node's operation.
    fn as_ref(&self) -> &Op {
        self.operation()
    }
}

impl<T> AsMut<[T]> for AstNode<T> {
    /// Returns a reference which allows modifying the operation's arguments.
    fn as_mut(&mut self) -> &mut [T] {
        self.args_mut()
    }
}

impl<'a, T> IntoIterator for &'a AstNode<T> {
    type Item = &'a T;

    type IntoIter = slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.args.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut AstNode<T> {
    type Item = &'a mut T;

    type IntoIter = slice::IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.args.iter_mut()
    }
}

impl<T> IntoIterator for AstNode<T> {
    type Item = T;

    type IntoIter = vec::IntoIter<T>;

    /// Converts the node into an iterator over its arguments.
    fn into_iter(self) -> Self::IntoIter {
        self.args.into_iter()
    }
}

impl Language for AstNode
where
    Op: Ord + Debug + Clone + Hash,
{
    fn matches(&self, other: &Self) -> bool {
        self.operation == other.operation && self.len() == other.len()
    }

    fn children(&self) -> &[Id] {
        self.args()
    }

    // Default methods

    fn children_mut(&mut self) -> &mut [Id] {
        self.args_mut()
    }

    fn for_each<F: FnMut(Id)>(&self, f: F) {
        self.iter().copied().for_each(f);
    }

    fn for_each_mut<F: FnMut(&mut Id)>(&mut self, f: F) {
        self.iter_mut().for_each(f);
    }

    fn try_for_each<E, F>(&self, f: F) -> Result<(), E>
    where
        F: FnMut(Id) -> Result<(), E>,
        E: Clone,
    {
        self.iter().copied().try_for_each(f)
    }

    fn len(&self) -> usize {
        self.len()
    }

    fn is_leaf(&self) -> bool {
        self.is_empty()
    }

    fn fold<F, T>(&self, init: T, f: F) -> T
    where
        F: FnMut(T, Id) -> T,
        T: Clone,
    {
        self.iter().copied().fold(init, f)
    }

    fn all<F>(&self, f: F) -> bool
    where
        F: FnMut(Id) -> bool,
    {
        self.iter().copied().all(f)
    }

    fn any<F>(&self, f: F) -> bool
    where
        F: FnMut(Id) -> bool,
    {
        self.iter().copied().any(f)
    }
}

/// An error which can be returned when parsing an expression using [`FromOp`].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Error)]
pub enum ParseNodeError<E> {
    /// The operator failed to parse.
    #[error(transparent)]
    ParseError(E),
}

impl FromOp for AstNode
{
    type Error = ParseNodeError<<Op as FromStr>::Err>;

    fn from_op(operation: &str, args: Vec<Id>) -> Result<Self, Self::Error> {
        let operation = operation.parse().map_err(ParseNodeError::ParseError)?;
        Ok(Self::new(operation, args))
    }
}

/// [Egg][egg] expects the [`Display`] implementation of a [`Language`] to
/// display only a node's operation, not its children. This implementation is
/// unexpected, so we only implement [`Display`] for the concrete type
/// [`AstNode<Op, Id>`].
impl Display for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        std::fmt::Display::fmt(&self.operation, f)
    }
}

