// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

//! Defines the AST structure of a graph DSL file

use regex::Regex;
use std::collections::HashMap;
use std::fmt;
use tree_sitter::CaptureQuantifier;
use tree_sitter::Language;
use tree_sitter::Query;

use crate::Identifier;
use crate::Location;

/// A graph DSL file
#[derive(Debug)]
pub struct File {
    pub language: Language,
    /// The expected global variables used in this file
    pub globals: Vec<Global>,
    /// The combined query of all stanzas in the file
    pub query: Option<Query>,
    /// The list of stanzas in the file
    pub stanzas: Vec<Stanza>,
    /// Attribute shorthands defined in the file
    pub shorthands: AttributeShorthands,
}

impl File {
    pub fn new(language: Language) -> File {
        File {
            language,
            globals: Vec::new(),
            query: None,
            stanzas: Vec::new(),
            shorthands: AttributeShorthands::new(),
        }
    }
}

/// A global variable
#[derive(Debug, Eq, PartialEq)]
pub struct Global {
    /// The name of the global variable
    pub name: Identifier,
    /// The quantifier of the global variable
    pub quantifier: CaptureQuantifier,
    pub location: Location,
}

/// One stanza within a file
#[derive(Debug)]
pub struct Stanza {
    /// The tree-sitter query for this stanza
    pub query: Query,
    /// The list of statements in the stanza
    pub statements: Vec<Statement>,
    /// Capture index of the full match in the file query
    pub full_match_file_capture_index: usize,
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
    // If
    If(If),
    // ForIn
    ForIn(ForIn),
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::DeclareImmutable(stmt) => stmt.fmt(f),
            Self::DeclareMutable(stmt) => stmt.fmt(f),
            Self::Assign(stmt) => stmt.fmt(f),
            Self::CreateGraphNode(stmt) => stmt.fmt(f),
            Self::AddGraphNodeAttribute(stmt) => stmt.fmt(f),
            Self::CreateEdge(stmt) => stmt.fmt(f),
            Self::AddEdgeAttribute(stmt) => stmt.fmt(f),
            Self::Scan(stmt) => stmt.fmt(f),
            Self::Print(stmt) => stmt.fmt(f),
            Self::If(stmt) => stmt.fmt(f),
            Self::ForIn(stmt) => stmt.fmt(f),
        }
    }
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

impl std::fmt::Display for AddEdgeAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "attr ({} -> {})", self.source, self.sink)?;
        for attr in &self.attributes {
            write!(f, " {}", attr)?;
        }
        write!(f, " at {}", self.location)
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

impl std::fmt::Display for AddGraphNodeAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "attr ({})", self.node)?;
        for attr in &self.attributes {
            write!(f, " {}", attr)?;
        }
        write!(f, " at {}", self.location)
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

impl std::fmt::Display for Assign {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "set {} = {} at {}",
            self.variable, self.value, self.location,
        )
    }
}

/// The name and value of an attribute
#[derive(Debug, Eq, PartialEq)]
pub struct Attribute {
    pub name: Identifier,
    pub value: Expression,
}

impl std::fmt::Display for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} = {}", self.name, self.value)
    }
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

impl std::fmt::Display for CreateEdge {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "edge {} -> {} at {}",
            self.source, self.sink, self.location,
        )
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

impl std::fmt::Display for CreateGraphNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "node {} at {}", self.node, self.location)
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

impl std::fmt::Display for DeclareImmutable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "let {} = {} at {}",
            self.variable, self.value, self.location,
        )
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

impl std::fmt::Display for DeclareMutable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "var {} = {} at {}",
            self.variable, self.value, self.location,
        )
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

impl std::fmt::Display for Print {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "print")?;
        for val in &self.values {
            write!(f, " {},", val)?;
        }
        write!(f, " at {}", self.location)
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

impl std::fmt::Display for Scan {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "scan {} {{ ... }} at {}", self.value, self.location)
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

impl std::fmt::Display for ScanArm {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?} {{ ... }}", self.regex.as_str())
    }
}

/// A `cond` conditional statement that selects the first branch with a matching condition
#[derive(Debug, Eq, PartialEq)]
pub struct If {
    pub arms: Vec<IfArm>,
    pub location: Location,
}

impl From<If> for Statement {
    fn from(statement: If) -> Statement {
        Statement::If(statement)
    }
}

impl std::fmt::Display for If {
    fn fmt(&self, f: &mut fmt::Formatter) -> std::fmt::Result {
        let mut first = true;
        for arm in &self.arms {
            if first {
                first = false;
                write!(f, "if {} {{ ... }}", DisplayConditions(&arm.conditions))?;
            } else {
                if !arm.conditions.is_empty() {
                    write!(f, " elif {} {{ ... }}", DisplayConditions(&arm.conditions))?;
                } else {
                    write!(f, " else {{ ... }}")?;
                }
            }
        }
        write!(f, " at {}", self.location)
    }
}

/// One arm of a `cond` statement
#[derive(Debug, PartialEq, Eq)]
pub struct IfArm {
    pub conditions: Vec<Condition>,
    pub statements: Vec<Statement>,
    pub location: Location,
}

struct DisplayConditions<'a>(&'a Vec<Condition>);

#[derive(Debug, PartialEq, Eq)]
pub enum Condition {
    Some {
        value: Expression,
        location: Location,
    },
    None {
        value: Expression,
        location: Location,
    },
    Bool {
        value: Expression,
        location: Location,
    },
}

impl std::fmt::Display for DisplayConditions<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> std::fmt::Result {
        let mut first = true;
        for condition in self.0.iter() {
            if first {
                first = false;
                write!(f, "{}", condition)?;
            } else {
                write!(f, ", {}", condition)?;
            }
        }
        Ok(())
    }
}

impl std::fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter) -> std::fmt::Result {
        match self {
            Condition::Some { value, .. } => {
                write!(f, "some {}", value)
            }
            Condition::None { value, .. } => {
                write!(f, "none {}", value)
            }
            Condition::Bool { value, .. } => {
                write!(f, "{}", value)
            }
        }
    }
}

/// A `for in` statement
#[derive(Debug, Eq, PartialEq)]
pub struct ForIn {
    pub variable: UnscopedVariable,
    pub value: Expression,
    pub statements: Vec<Statement>,
    pub location: Location,
}

impl From<ForIn> for Statement {
    fn from(statement: ForIn) -> Statement {
        Statement::ForIn(statement)
    }
}

impl std::fmt::Display for ForIn {
    fn fmt(&self, f: &mut fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "for {} in {} {{ ... }} at {}",
            self.variable, self.value, self.location,
        )
    }
}

/// A reference to a variable
#[derive(Debug, Eq, PartialEq)]
pub enum Variable {
    Scoped(ScopedVariable),
    Unscoped(UnscopedVariable),
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Variable::Scoped(variable) => variable.fmt(f),
            Variable::Unscoped(variable) => variable.fmt(f),
        }
    }
}

/// A reference to a scoped variable
#[derive(Debug, Eq, PartialEq)]
pub struct ScopedVariable {
    pub scope: Box<Expression>,
    pub name: Identifier,
    pub location: Location,
}

impl From<ScopedVariable> for Variable {
    fn from(variable: ScopedVariable) -> Variable {
        Variable::Scoped(variable)
    }
}

impl std::fmt::Display for ScopedVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}.{}", self.scope, self.name)
    }
}

/// A reference to a global or local variable
#[derive(Debug, Eq, PartialEq)]
pub struct UnscopedVariable {
    pub name: Identifier,
    pub location: Location,
}

impl From<UnscopedVariable> for Variable {
    fn from(variable: UnscopedVariable) -> Variable {
        Variable::Unscoped(variable)
    }
}

impl std::fmt::Display for UnscopedVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
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
    // Literals
    ListLiteral(ListLiteral),
    SetLiteral(SetLiteral),
    // Syntax nodes
    Capture(Capture),
    // Variables
    Variable(Variable),
    // Functions
    Call(Call),
    // Regular expression
    RegexCapture(RegexCapture),
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expression::FalseLiteral => write!(f, "false"),
            Expression::NullLiteral => write!(f, "#null"),
            Expression::TrueLiteral => write!(f, "true"),
            Expression::IntegerConstant(expr) => expr.fmt(f),
            Expression::StringConstant(expr) => expr.fmt(f),
            Expression::ListLiteral(expr) => expr.fmt(f),
            Expression::SetLiteral(expr) => expr.fmt(f),
            Expression::Capture(expr) => expr.fmt(f),
            Expression::Variable(expr) => expr.fmt(f),
            Expression::Call(expr) => expr.fmt(f),
            Expression::RegexCapture(expr) => expr.fmt(f),
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

impl std::fmt::Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}", self.function)?;
        for arg in &self.parameters {
            write!(f, " {}", arg)?;
        }
        write!(f, ")")
    }
}

/// A capture expression that references a syntax node
#[derive(Debug, Eq, PartialEq)]
pub struct Capture {
    /// The name of the capture
    pub name: Identifier,
    /// The suffix of the capture
    pub quantifier: CaptureQuantifier,
    /// Capture index in the merged file query
    pub file_capture_index: usize,
    /// Capture index in the stanza query
    pub stanza_capture_index: usize,
    pub location: Location,
}

impl From<Capture> for Expression {
    fn from(expr: Capture) -> Expression {
        Expression::Capture(expr)
    }
}

impl std::fmt::Display for Capture {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "@{}", self.name)
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

impl std::fmt::Display for IntegerConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

/// An ordered list of values
#[derive(Debug, Eq, PartialEq)]
pub struct ListLiteral {
    pub elements: Vec<Expression>,
}

impl From<ListLiteral> for Expression {
    fn from(expr: ListLiteral) -> Expression {
        Expression::ListLiteral(expr)
    }
}

impl std::fmt::Display for ListLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[")?;
        let mut first = true;
        for elem in &self.elements {
            if first {
                write!(f, "{}", elem)?;
                first = false;
            } else {
                write!(f, ", {}", elem)?;
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

impl std::fmt::Display for RegexCapture {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "${}", self.match_index)
    }
}

/// An unordered set of values
#[derive(Debug, Eq, PartialEq)]
pub struct SetLiteral {
    pub elements: Vec<Expression>,
}

impl From<SetLiteral> for Expression {
    fn from(expr: SetLiteral) -> Expression {
        Expression::SetLiteral(expr)
    }
}

impl std::fmt::Display for SetLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{{")?;
        let mut first = true;
        for elem in &self.elements {
            if first {
                write!(f, "{}", elem)?;
                first = false;
            } else {
                write!(f, ", {}", elem)?;
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

impl std::fmt::Display for StringConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.value)
    }
}

impl From<String> for Expression {
    fn from(value: String) -> Expression {
        Expression::StringConstant(StringConstant { value }.into())
    }
}

impl From<UnscopedVariable> for Expression {
    fn from(variable: UnscopedVariable) -> Expression {
        Expression::Variable(variable.into())
    }
}

impl From<ScopedVariable> for Expression {
    fn from(variable: ScopedVariable) -> Expression {
        Expression::Variable(variable.into())
    }
}

/// Attribute shorthands
#[derive(Debug, Eq, PartialEq)]
pub struct AttributeShorthands(HashMap<Identifier, AttributeShorthand>);

impl AttributeShorthands {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn get(&self, name: &Identifier) -> Option<&AttributeShorthand> {
        self.0.get(name)
    }

    pub fn add(&mut self, shorthand: AttributeShorthand) {
        self.0.insert(shorthand.name.clone(), shorthand);
    }

    pub fn iter(&self) -> impl Iterator<Item = &AttributeShorthand> {
        self.0.values()
    }

    pub fn into_iter(self) -> impl Iterator<Item = AttributeShorthand> {
        self.0.into_values()
    }
}

/// An attribute shorthand
#[derive(Debug, Eq, PartialEq)]
pub struct AttributeShorthand {
    pub name: Identifier,
    pub variable: UnscopedVariable,
    pub attributes: Vec<Attribute>,
    pub location: Location,
}

impl std::fmt::Display for AttributeShorthand {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "attribute {} = {} =>", self.name, self.variable,)?;
        for attr in &self.attributes {
            write!(f, " {}", attr)?;
        }
        write!(f, " at {}", self.location)
    }
}
