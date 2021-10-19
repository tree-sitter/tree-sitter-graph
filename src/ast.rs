// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

//! Defines the AST structure of a graph DSL file

use regex::Regex;
use std::fmt;
use tree_sitter::Language;
use tree_sitter::Query;

use crate::Context;
use crate::DisplayWithContext;
use crate::Identifier;

/// A graph DSL file
#[derive(Debug)]
pub struct File {
    pub language: Language,
    /// The list of stanzas in the file
    pub stanzas: Vec<Stanza>,
}

impl File {
    pub fn new(language: Language) -> File {
        File {
            language,
            stanzas: Vec::new(),
        }
    }
}

/// One stanza within a file
#[derive(Debug)]
pub struct Stanza {
    /// The tree-sitter query for this stanza
    pub query: Query,
    /// The list of statements in the stanza
    pub statements: Vec<Statement>,
}

/// A statement that can appear in a graph DSL stanza
#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
    // Variables
    DeclareImmutable(DeclareImmutable),
    DeclareMutable(DeclareMutable),
    Assign(Assign),
    // Graph nodes
    CreateGraphNode(CreateGraphNode),
    AddGraphNodeAttribute(AddGraphNodeAttribute),
    // Edges
    CreateEdge(CreateEdge),
    AddEdgeAttribute(AddEdgeAttribute),
    // Regular expression
    Scan(Scan),
    // Debugging
    Print(Print),
}

impl DisplayWithContext for Statement {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context) -> fmt::Result {
        match self {
            Statement::DeclareImmutable(stmt) => stmt.fmt(f, ctx),
            Statement::DeclareMutable(stmt) => stmt.fmt(f, ctx),
            Statement::Assign(stmt) => stmt.fmt(f, ctx),
            Statement::CreateGraphNode(stmt) => stmt.fmt(f, ctx),
            Statement::AddGraphNodeAttribute(stmt) => stmt.fmt(f, ctx),
            Statement::CreateEdge(stmt) => stmt.fmt(f, ctx),
            Statement::AddEdgeAttribute(stmt) => stmt.fmt(f, ctx),
            Statement::Scan(stmt) => stmt.fmt(f, ctx),
            Statement::Print(stmt) => stmt.fmt(f, ctx),
        }
    }
}

/// An `attr` statement that adds an attribute to an edge
#[derive(Debug, Eq, PartialEq)]
pub struct AddEdgeAttribute {
    pub source: Expression,
    pub sink: Expression,
    pub attributes: Vec<Attribute>,
}

impl From<AddEdgeAttribute> for Statement {
    fn from(statement: AddEdgeAttribute) -> Statement {
        Statement::AddEdgeAttribute(statement)
    }
}

impl DisplayWithContext for AddEdgeAttribute {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context) -> fmt::Result {
        write!(
            f,
            "attr ({} -> {})",
            self.source.display_with(ctx),
            self.sink.display_with(ctx),
        )?;
        for attr in &self.attributes {
            write!(f, " {}", attr.display_with(ctx))?;
        }
        write!(f, "")
    }
}

/// An `attr` statement that adds an attribute to a graph node
#[derive(Debug, Eq, PartialEq)]
pub struct AddGraphNodeAttribute {
    pub node: Expression,
    pub attributes: Vec<Attribute>,
}

impl From<AddGraphNodeAttribute> for Statement {
    fn from(statement: AddGraphNodeAttribute) -> Statement {
        Statement::AddGraphNodeAttribute(statement)
    }
}

impl DisplayWithContext for AddGraphNodeAttribute {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context) -> fmt::Result {
        write!(f, "attr ({})", self.node.display_with(ctx),)?;
        for attr in &self.attributes {
            write!(f, " {}", attr.display_with(ctx),)?;
        }
        write!(f, "")
    }
}

/// A `set` statement that updates the value of a mutable variable
#[derive(Debug, Eq, PartialEq)]
pub struct Assign {
    pub variable: Variable,
    pub value: Expression,
}

impl From<Assign> for Statement {
    fn from(statement: Assign) -> Statement {
        Statement::Assign(statement)
    }
}

impl DisplayWithContext for Assign {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context) -> fmt::Result {
        write!(
            f,
            "set {} = {}",
            self.variable.display_with(ctx),
            self.value.display_with(ctx),
        )
    }
}

/// The name and value of an attribute
#[derive(Debug, Eq, PartialEq)]
pub struct Attribute {
    pub name: Identifier,
    pub value: Expression,
}

impl DisplayWithContext for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context) -> fmt::Result {
        write!(
            f,
            "{} = {}",
            self.name.display_with(ctx),
            self.value.display_with(ctx),
        )
    }
}

/// An `edge` statement that creates a new edge
#[derive(Debug, Eq, PartialEq)]
pub struct CreateEdge {
    pub source: Expression,
    pub sink: Expression,
}

impl From<CreateEdge> for Statement {
    fn from(statement: CreateEdge) -> Statement {
        Statement::CreateEdge(statement)
    }
}

impl DisplayWithContext for CreateEdge {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context) -> fmt::Result {
        write!(
            f,
            "edge {} -> {}",
            self.source.display_with(ctx),
            self.sink.display_with(ctx),
        )
    }
}

/// A `node` statement that creates a new graph node
#[derive(Debug, Eq, PartialEq)]
pub struct CreateGraphNode {
    pub node: Variable,
}

impl From<CreateGraphNode> for Statement {
    fn from(statement: CreateGraphNode) -> Statement {
        Statement::CreateGraphNode(statement)
    }
}

impl DisplayWithContext for CreateGraphNode {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context) -> fmt::Result {
        write!(f, "node {}", self.node.display_with(ctx),)
    }
}

/// A `let` statement that declares a new immutable variable
#[derive(Debug, Eq, PartialEq)]
pub struct DeclareImmutable {
    pub variable: Variable,
    pub value: Expression,
}

impl From<DeclareImmutable> for Statement {
    fn from(statement: DeclareImmutable) -> Statement {
        Statement::DeclareImmutable(statement)
    }
}

impl DisplayWithContext for DeclareImmutable {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context) -> fmt::Result {
        write!(
            f,
            "let {} = {}",
            self.variable.display_with(ctx),
            self.value.display_with(ctx),
        )
    }
}

/// A `var` statement that declares a new mutable variable
#[derive(Debug, Eq, PartialEq)]
pub struct DeclareMutable {
    pub variable: Variable,
    pub value: Expression,
}

impl From<DeclareMutable> for Statement {
    fn from(statement: DeclareMutable) -> Statement {
        Statement::DeclareMutable(statement)
    }
}

impl DisplayWithContext for DeclareMutable {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context) -> fmt::Result {
        write!(
            f,
            "var {} = {}",
            self.variable.display_with(ctx),
            self.value.display_with(ctx),
        )
    }
}

/// A `print` statement that prints out some debugging information
#[derive(Debug, Eq, PartialEq)]
pub struct Print {
    pub values: Vec<Expression>,
}

impl From<Print> for Statement {
    fn from(statement: Print) -> Statement {
        Statement::Print(statement)
    }
}

impl DisplayWithContext for Print {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context) -> fmt::Result {
        write!(f, "print")?;
        for val in &self.values {
            write!(f, " {},", val.display_with(ctx),)?;
        }
        write!(f, "")
    }
}

/// A `scan` statement that matches regular expressions against a string
#[derive(Debug, Eq, PartialEq)]
pub struct Scan {
    pub value: Expression,
    pub arms: Vec<ScanArm>,
}

impl From<Scan> for Statement {
    fn from(statement: Scan) -> Statement {
        Statement::Scan(statement)
    }
}

impl DisplayWithContext for Scan {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context) -> fmt::Result {
        write!(f, "scan {} {{ ... }}", self.value.display_with(ctx))
    }
}

/// One arm of a `scan` statement
#[derive(Debug)]
pub struct ScanArm {
    pub regex: Regex,
    pub statements: Vec<Statement>,
}

impl Eq for ScanArm {}

impl PartialEq for ScanArm {
    fn eq(&self, other: &ScanArm) -> bool {
        self.regex.as_str() == other.regex.as_str() && self.statements == other.statements
    }
}

impl DisplayWithContext for ScanArm {
    fn fmt(&self, f: &mut fmt::Formatter, _ctx: &Context) -> fmt::Result {
        write!(f, "{:?} {{ ... }}", self.regex.as_str())
    }
}

/// A reference to a variable
#[derive(Debug, Eq, PartialEq)]
pub enum Variable {
    Scoped(ScopedVariable),
    Unscoped(UnscopedVariable),
}

impl DisplayWithContext for Variable {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context) -> fmt::Result {
        match self {
            Variable::Scoped(variable) => variable.fmt(f, ctx),
            Variable::Unscoped(variable) => variable.fmt(f, ctx),
        }
    }
}

/// A reference to a scoped variable
#[derive(Debug, Eq, PartialEq)]
pub struct ScopedVariable {
    pub scope: Box<Expression>,
    pub name: Identifier,
}

impl From<ScopedVariable> for Variable {
    fn from(variable: ScopedVariable) -> Variable {
        Variable::Scoped(variable)
    }
}

impl DisplayWithContext for ScopedVariable {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context) -> fmt::Result {
        write!(
            f,
            "{}.{}",
            self.scope.display_with(ctx),
            self.name.display_with(ctx),
        )
    }
}

/// A reference to a global or local variable
#[derive(Debug, Eq, PartialEq)]
pub struct UnscopedVariable {
    pub name: Identifier,
}

impl From<UnscopedVariable> for Variable {
    fn from(variable: UnscopedVariable) -> Variable {
        Variable::Unscoped(variable)
    }
}

impl From<Identifier> for Variable {
    fn from(name: Identifier) -> Variable {
        UnscopedVariable { name }.into()
    }
}

impl DisplayWithContext for UnscopedVariable {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context) -> fmt::Result {
        write!(f, "{}", self.name.display_with(ctx))
    }
}

/// An expression that can appear in a graph DSL file
#[derive(Debug, Eq, PartialEq)]
pub enum Expression {
    // Literals
    FalseLiteral,
    NullLiteral,
    TrueLiteral,
    // Constants
    IntegerConstant(IntegerConstant),
    StringConstant(StringConstant),
    // Comprehensions
    List(ListComprehension),
    Set(SetComprehension),
    // Syntax nodes
    Capture(Capture),
    // Variables
    Variable(Variable),
    // Functions
    Call(Call),
    // Regular expression
    RegexCapture(RegexCapture),
}

impl DisplayWithContext for Expression {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context) -> fmt::Result {
        match self {
            Expression::FalseLiteral => write!(f, "false"),
            Expression::NullLiteral => write!(f, "#null"),
            Expression::TrueLiteral => write!(f, "true"),
            Expression::IntegerConstant(expr) => expr.fmt(f, ctx),
            Expression::StringConstant(expr) => expr.fmt(f, ctx),
            Expression::List(expr) => expr.fmt(f, ctx),
            Expression::Set(expr) => expr.fmt(f, ctx),
            Expression::Capture(expr) => expr.fmt(f, ctx),
            Expression::Variable(expr) => expr.fmt(f, ctx),
            Expression::Call(expr) => expr.fmt(f, ctx),
            Expression::RegexCapture(expr) => expr.fmt(f, ctx),
        }
    }
}

/// A function call
#[derive(Debug, Eq, PartialEq)]
pub struct Call {
    pub function: Identifier,
    pub parameters: Vec<Expression>,
}

impl From<Call> for Expression {
    fn from(expr: Call) -> Expression {
        Expression::Call(expr)
    }
}

impl DisplayWithContext for Call {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context) -> fmt::Result {
        write!(f, "({}", self.function.display_with(ctx))?;
        for arg in &self.parameters {
            write!(f, " {}", arg.display_with(ctx))?;
        }
        write!(f, ")")
    }
}

/// A capture expression that references a syntax node
#[derive(Debug, Eq, PartialEq)]
pub struct Capture {
    /// The index of this capture in the block's tree-sitter query
    pub index: usize,
    /// The name of the capture
    pub name: Identifier,
}

impl From<Capture> for Expression {
    fn from(expr: Capture) -> Expression {
        Expression::Capture(expr)
    }
}

impl DisplayWithContext for Capture {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context) -> fmt::Result {
        write!(f, "{}", self.name.display_with(ctx))
    }
}

/// An integer constant
#[derive(Debug, Eq, PartialEq)]
pub struct IntegerConstant {
    pub value: u32,
}

impl From<IntegerConstant> for Expression {
    fn from(expr: IntegerConstant) -> Expression {
        Expression::IntegerConstant(expr)
    }
}

impl DisplayWithContext for IntegerConstant {
    fn fmt(&self, f: &mut fmt::Formatter, _ctx: &Context) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

/// An ordered list of values
#[derive(Debug, Eq, PartialEq)]
pub struct ListComprehension {
    pub elements: Vec<Expression>,
}

impl From<ListComprehension> for Expression {
    fn from(expr: ListComprehension) -> Expression {
        Expression::List(expr)
    }
}

impl DisplayWithContext for ListComprehension {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context) -> fmt::Result {
        write!(f, "[")?;
        let mut first = true;
        for elem in &self.elements {
            if first {
                write!(f, "{}", elem.display_with(ctx))?;
                first = false;
            } else {
                write!(f, ", {}", elem.display_with(ctx))?;
            }
        }
        write!(f, "]")
    }
}

/// A reference to one of the regex captures in a `scan` statement
#[derive(Debug, Eq, PartialEq)]
pub struct RegexCapture {
    pub match_index: usize,
}

impl From<RegexCapture> for Expression {
    fn from(expr: RegexCapture) -> Expression {
        Expression::RegexCapture(expr)
    }
}

impl DisplayWithContext for RegexCapture {
    fn fmt(&self, f: &mut fmt::Formatter, _ctx: &Context) -> fmt::Result {
        write!(f, "${}", self.match_index)
    }
}

/// An unordered set of values
#[derive(Debug, Eq, PartialEq)]
pub struct SetComprehension {
    pub elements: Vec<Expression>,
}

impl From<SetComprehension> for Expression {
    fn from(expr: SetComprehension) -> Expression {
        Expression::Set(expr)
    }
}

impl DisplayWithContext for SetComprehension {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context) -> fmt::Result {
        write!(f, "{{")?;
        let mut first = true;
        for elem in &self.elements {
            if first {
                write!(f, "{}", elem.display_with(ctx))?;
                first = false;
            } else {
                write!(f, ", {}", elem.display_with(ctx))?;
            }
        }
        write!(f, "}}")
    }
}

/// A string constant
#[derive(Debug, Eq, PartialEq)]
pub struct StringConstant {
    pub value: String,
}

impl From<StringConstant> for Expression {
    fn from(expr: StringConstant) -> Expression {
        Expression::StringConstant(expr)
    }
}

impl DisplayWithContext for StringConstant {
    fn fmt(&self, f: &mut fmt::Formatter, _ctx: &Context) -> fmt::Result {
        write!(f, "{:?}", self.value)
    }
}

impl From<String> for Expression {
    fn from(value: String) -> Expression {
        Expression::StringConstant(StringConstant { value }.into())
    }
}

impl From<Identifier> for Expression {
    fn from(name: Identifier) -> Expression {
        Expression::Variable(UnscopedVariable { name }.into())
    }
}

impl From<ScopedVariable> for Expression {
    fn from(variable: ScopedVariable) -> Expression {
        Expression::Variable(variable.into())
    }
}
