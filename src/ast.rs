// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

//! Defines the AST structure of a graph DSL file

use regex::Regex;
use tree_sitter::Language;
use tree_sitter::Query;

use crate::Identifier;
use crate::Location;

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
    pub location: Location,
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

/// An `attr` statement that adds an attribute to an edge
#[derive(Debug, Eq, PartialEq)]
pub struct AddEdgeAttribute {
    pub source: Expression,
    pub sink: Expression,
    pub attributes: Vec<Attribute>,
    pub location: Location,
}

impl From<AddEdgeAttribute> for Statement {
    fn from(statement: AddEdgeAttribute) -> Statement {
        Statement::AddEdgeAttribute(statement)
    }
}

/// An `attr` statement that adds an attribute to a graph node
#[derive(Debug, Eq, PartialEq)]
pub struct AddGraphNodeAttribute {
    pub node: Expression,
    pub attributes: Vec<Attribute>,
    pub location: Location,
}

impl From<AddGraphNodeAttribute> for Statement {
    fn from(statement: AddGraphNodeAttribute) -> Statement {
        Statement::AddGraphNodeAttribute(statement)
    }
}

/// A `set` statement that updates the value of a mutable variable
#[derive(Debug, Eq, PartialEq)]
pub struct Assign {
    pub variable: Variable,
    pub value: Expression,
    pub location: Location,
}

impl From<Assign> for Statement {
    fn from(statement: Assign) -> Statement {
        Statement::Assign(statement)
    }
}

/// The name and value of an attribute
#[derive(Debug, Eq, PartialEq)]
pub struct Attribute {
    pub name: Identifier,
    pub value: Expression,
}

/// An `edge` statement that creates a new edge
#[derive(Debug, Eq, PartialEq)]
pub struct CreateEdge {
    pub source: Expression,
    pub sink: Expression,
    pub location: Location,
}

impl From<CreateEdge> for Statement {
    fn from(statement: CreateEdge) -> Statement {
        Statement::CreateEdge(statement)
    }
}

/// A `node` statement that creates a new graph node
#[derive(Debug, Eq, PartialEq)]
pub struct CreateGraphNode {
    pub node: Variable,
    pub location: Location,
}

impl From<CreateGraphNode> for Statement {
    fn from(statement: CreateGraphNode) -> Statement {
        Statement::CreateGraphNode(statement)
    }
}

/// A `let` statement that declares a new immutable variable
#[derive(Debug, Eq, PartialEq)]
pub struct DeclareImmutable {
    pub variable: Variable,
    pub value: Expression,
    pub location: Location,
}

impl From<DeclareImmutable> for Statement {
    fn from(statement: DeclareImmutable) -> Statement {
        Statement::DeclareImmutable(statement)
    }
}

/// A `var` statement that declares a new mutable variable
#[derive(Debug, Eq, PartialEq)]
pub struct DeclareMutable {
    pub variable: Variable,
    pub value: Expression,
    pub location: Location,
}

impl From<DeclareMutable> for Statement {
    fn from(statement: DeclareMutable) -> Statement {
        Statement::DeclareMutable(statement)
    }
}

/// A `print` statement that prints out some debugging information
#[derive(Debug, Eq, PartialEq)]
pub struct Print {
    pub values: Vec<Expression>,
    pub location: Location,
}

impl From<Print> for Statement {
    fn from(statement: Print) -> Statement {
        Statement::Print(statement)
    }
}

/// A `scan` statement that matches regular expressions against a string
#[derive(Debug, Eq, PartialEq)]
pub struct Scan {
    pub value: Expression,
    pub arms: Vec<ScanArm>,
    pub location: Location,
}

impl From<Scan> for Statement {
    fn from(statement: Scan) -> Statement {
        Statement::Scan(statement)
    }
}

/// One arm of a `scan` statement
#[derive(Debug)]
pub struct ScanArm {
    pub regex: Regex,
    pub statements: Vec<Statement>,
    pub location: Location,
}

impl Eq for ScanArm {}

impl PartialEq for ScanArm {
    fn eq(&self, other: &ScanArm) -> bool {
        self.regex.as_str() == other.regex.as_str() && self.statements == other.statements
    }
}

/// A reference to a variable
#[derive(Debug, Eq, PartialEq)]
pub enum Variable {
    Scoped(ScopedVariable),
    Unscoped(UnscopedVariable),
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
