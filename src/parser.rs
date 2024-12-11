// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

use std::fmt::Display;
use std::iter::Peekable;
use std::path::Path;
use std::str::Chars;

use regex::Regex;
use thiserror::Error;
use tree_sitter::CaptureQuantifier;
use tree_sitter::CaptureQuantifier::One;
use tree_sitter::CaptureQuantifier::OneOrMore;
use tree_sitter::CaptureQuantifier::Zero;
use tree_sitter::CaptureQuantifier::ZeroOrMore;
use tree_sitter::CaptureQuantifier::ZeroOrOne;
use tree_sitter::Language;
use tree_sitter::Query;
use tree_sitter::QueryError;

use crate::ast;
use crate::parse_error::Excerpt;
use crate::Identifier;

pub const FULL_MATCH: &str = "__tsg__full_match";

impl ast::File {
    /// Parses a graph DSL file, returning a new `File` instance.
    pub fn from_str(language: Language, source: &str) -> Result<Self, ParseError> {
        let mut file = ast::File::new(language);
        #[allow(deprecated)]
        file.parse(source)?;
        file.check()?;
        Ok(file)
    }

    /// Parses a graph DSL file, adding its content to an existing `File` instance.
    #[deprecated(
        note = "Parsing multiple times into the same `File` instance is unsound. Use `File::from_str` instead."
    )]
    pub fn parse(&mut self, content: &str) -> Result<(), ParseError> {
        Parser::new(content).parse_into_file(self)
    }
}

// ----------------------------------------------------------------------------
// Parse errors

/// An error that can occur while parsing a graph DSL file
#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Expected quantifier at {0}")]
    ExpectedQuantifier(Location),
    #[error("Expected '{0}' at {1}")]
    ExpectedToken(&'static str, Location),
    #[error("Expected variable name at {0}")]
    ExpectedVariable(Location),
    #[error("Expected unscoped variable at {0}")]
    ExpectedUnscopedVariable(Location),
    #[error("Invalid regular expression /{0}/ at {1}")]
    InvalidRegex(String, Location),
    #[error("Expected integer constant in regex capture at {0}")]
    InvalidRegexCapture(Location),
    #[error("Invalid query pattern: {}", _0.message)]
    QueryError(#[from] QueryError),
    #[error("Unexpected character '{0}' in {1} at {2}")]
    UnexpectedCharacter(char, &'static str, Location),
    #[error("Unexpected end of file at {0}")]
    UnexpectedEOF(Location),
    #[error("Unexpected keyword '{0}' at {1}")]
    UnexpectedKeyword(String, Location),
    #[error("Unexpected literal '#{0}' at {1}")]
    UnexpectedLiteral(String, Location),
    #[error("Query contains multiple patterns at {0}")]
    UnexpectedQueryPatterns(Location),
    #[error(transparent)]
    Check(#[from] crate::checker::CheckError),
}

impl ParseError {
    pub fn display_pretty<'a>(
        &'a self,
        path: &'a Path,
        source: &'a str,
    ) -> impl std::fmt::Display + 'a {
        DisplayParseErrorPretty {
            error: self,
            path,
            source,
        }
    }
}

struct DisplayParseErrorPretty<'a> {
    error: &'a ParseError,
    path: &'a Path,
    source: &'a str,
}

impl std::fmt::Display for DisplayParseErrorPretty<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let location = match self.error {
            ParseError::ExpectedQuantifier(location) => *location,
            ParseError::ExpectedToken(_, location) => *location,
            ParseError::ExpectedVariable(location) => *location,
            ParseError::ExpectedUnscopedVariable(location) => *location,
            ParseError::InvalidRegex(_, location) => *location,
            ParseError::InvalidRegexCapture(location) => *location,
            ParseError::QueryError(err) => Location {
                row: err.row,
                column: err.column,
            },
            ParseError::UnexpectedCharacter(_, _, location) => *location,
            ParseError::UnexpectedEOF(location) => *location,
            ParseError::UnexpectedKeyword(_, location) => *location,
            ParseError::UnexpectedLiteral(_, location) => *location,
            ParseError::UnexpectedQueryPatterns(location) => *location,
            ParseError::Check(err) => {
                write!(f, "{}", err.display_pretty(self.path, self.source))?;
                return Ok(());
            }
        };
        writeln!(f, "{}", self.error)?;
        write!(
            f,
            "{}",
            Excerpt::from_source(
                self.path,
                self.source,
                location.row,
                location.to_column_range(),
                0
            )
        )?;
        Ok(())
    }
}

// ----------------------------------------------------------------------------
// Location

/// The location of a graph DSL entity within its file
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct Location {
    pub row: usize,
    pub column: usize,
}

impl Location {
    fn advance(&mut self, ch: char) {
        if ch == '\n' {
            self.row += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }
    }

    pub(crate) fn to_column_range(&self) -> std::ops::Range<usize> {
        self.column..self.column + 1
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}, {})", self.row + 1, self.column + 1)
    }
}

// ----------------------------------------------------------------------------
// Range

/// The range of a graph DSL entity within its file
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct Range {
    pub start: Location,
    pub end: Location,
}

impl Display for Range {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} - {}", self.start, self.end)
    }
}

// ----------------------------------------------------------------------------
// Parser

struct Parser<'a> {
    source: &'a str,
    chars: Peekable<Chars<'a>>,
    offset: usize,
    location: Location,
    query_source: String,
}

fn is_ident_start(c: char) -> bool {
    c == '_' || c.is_alphabetic()
}

fn is_ident(c: char) -> bool {
    c == '_' || c == '-' || c.is_alphanumeric()
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Parser<'a> {
        let chars = source.chars().peekable();
        let query_source = String::with_capacity(source.len());
        Parser {
            source,
            chars,
            offset: 0,
            location: Location::default(),
            query_source,
        }
    }
}

impl<'a> Parser<'a> {
    fn peek(&mut self) -> Result<char, ParseError> {
        self.chars
            .peek()
            .copied()
            .ok_or_else(|| ParseError::UnexpectedEOF(self.location))
    }

    fn try_peek(&mut self) -> Option<char> {
        self.peek().ok()
    }

    fn next(&mut self) -> Result<char, ParseError> {
        let ch = self
            .chars
            .next()
            .ok_or_else(|| ParseError::UnexpectedEOF(self.location))?;
        self.offset += ch.len_utf8();
        self.location.advance(ch);
        Ok(ch)
    }

    fn skip(&mut self) -> Result<(), ParseError> {
        self.next().map(|_| ())
    }

    fn consume_whitespace(&mut self) {
        let mut in_comment = false;
        while let Some(ch) = self.try_peek() {
            if in_comment {
                if ch == '\n' {
                    in_comment = false;
                }
            } else {
                if ch == ';' {
                    in_comment = true;
                } else if !ch.is_whitespace() {
                    return;
                }
            }
            self.skip().unwrap();
        }
    }

    fn consume_while(&mut self, mut f: impl FnMut(char) -> bool) {
        while let Some(ch) = self.try_peek() {
            if !f(ch) {
                return;
            }
            self.skip().unwrap();
        }
    }

    fn consume_n(&mut self, count: usize) -> Result<(), ParseError> {
        for _ in 0..count {
            self.next()?;
        }
        Ok(())
    }

    fn consume_token(&mut self, token: &'static str) -> Result<(), ParseError> {
        if self.source[self.offset..].starts_with(token) {
            self.consume_n(token.len())
        } else {
            Err(ParseError::ExpectedToken(token, self.location))
        }
    }

    fn parse_into_file(&mut self, file: &mut ast::File) -> Result<(), ParseError> {
        self.consume_whitespace();
        while self.try_peek().is_some() {
            if let Ok(_) = self.consume_token("attribute") {
                self.consume_whitespace();
                let shorthand = self.parse_shorthand()?;
                file.shorthands.add(shorthand);
            } else if let Ok(_) = self.consume_token("global") {
                self.consume_whitespace();
                let global = self.parse_global()?;
                file.globals.push(global);
            } else if let Ok(_) = self.consume_token("inherit") {
                self.consume_whitespace();
                self.consume_token(".")?;
                let name = self.parse_identifier("inherit")?;
                file.inherited_variables.insert(name);
            } else {
                let stanza = self.parse_stanza(&file.language)?;
                file.stanzas.push(stanza);
            }
            self.consume_whitespace();
        }
        // we can unwrap here because all queries have already been parsed before
        file.query = Some(Query::new(&file.language, &self.query_source).unwrap());
        Ok(())
    }

    fn parse_global(&mut self) -> Result<ast::Global, ParseError> {
        let location = self.location;
        let name = self.parse_identifier("global variable")?;
        let quantifier = self.parse_quantifier()?;
        let mut default = None;
        self.consume_whitespace();
        if let Ok(_) = self.consume_token("=") {
            self.consume_whitespace();
            default = Some(self.parse_string()?);
        }
        Ok(ast::Global {
            name,
            quantifier,
            default,
            location,
        })
    }

    fn parse_shorthand(&mut self) -> Result<ast::AttributeShorthand, ParseError> {
        let location = self.location;
        let name = self.parse_identifier("shorthand name")?;
        self.consume_whitespace();
        self.consume_token("=")?;
        self.consume_whitespace();
        let variable = self.parse_unscoped_variable()?;
        self.consume_whitespace();
        self.consume_token("=>")?;
        self.consume_whitespace();
        let attributes = self.parse_attributes()?;
        Ok(ast::AttributeShorthand {
            name,
            variable,
            attributes,
            location,
        })
    }

    fn parse_quantifier(&mut self) -> Result<CaptureQuantifier, ParseError> {
        let mut quantifier = One;
        if let Some(c) = self.try_peek() {
            self.skip().unwrap();
            if c == '?' {
                quantifier = ZeroOrOne;
            } else if c == '*' {
                quantifier = ZeroOrMore;
            } else if c == '+' {
                quantifier = OneOrMore;
            } else if !c.is_whitespace() {
                return Err(ParseError::ExpectedQuantifier(self.location));
            }
        }
        Ok(quantifier)
    }

    fn parse_stanza(&mut self, language: &Language) -> Result<ast::Stanza, ParseError> {
        let start = self.location;
        let (query, full_match_stanza_capture_index) = self.parse_query(language)?;
        self.consume_whitespace();
        let statements = self.parse_statements()?;
        let end = self.location;
        let range = Range { start, end };
        Ok(ast::Stanza {
            query,
            statements,
            full_match_stanza_capture_index,
            full_match_file_capture_index: usize::MAX, // set in checker
            range,
        })
    }

    fn parse_query(&mut self, language: &Language) -> Result<(Query, usize), ParseError> {
        let location = self.location;
        let query_start = self.offset;
        self.skip_query()?;
        let query_end = self.offset;
        let query_source = self.source[query_start..query_end].to_owned() + "@" + FULL_MATCH;
        // If tree-sitter allowed us to incrementally add patterns to a query, we wouldn't need
        // the global query_source.
        self.query_source += &query_source;
        self.query_source += "\n";
        let query = Query::new(language, &query_source).map_err(|mut e| {
            // the column of the first row of a query pattern must be shifted by the whitespace
            // that was already consumed
            if e.row == 0 {
                // must come before we update e.row!
                e.column += location.column;
            }
            e.row += location.row;
            e.offset += query_start;
            e
        })?;
        if query.pattern_count() > 1 {
            return Err(ParseError::UnexpectedQueryPatterns(location));
        }
        let full_match_capture_index = query
            .capture_index_for_name(FULL_MATCH)
            .expect("missing capture index for full match")
            as usize;
        Ok((query, full_match_capture_index))
    }

    fn skip_query(&mut self) -> Result<(), ParseError> {
        let mut paren_depth = 0;
        let mut in_string = false;
        let mut in_escape = false;
        let mut in_comment = false;
        loop {
            let ch = self.peek()?;
            if in_escape {
                in_escape = false;
            } else if in_string {
                match ch {
                    '\\' => {
                        in_escape = true;
                    }
                    '"' | '\n' => {
                        in_string = false;
                    }
                    _ => {}
                }
            } else if in_comment {
                if ch == '\n' {
                    in_comment = false;
                }
            } else {
                match ch {
                    '"' => in_string = true,
                    '(' => paren_depth += 1,
                    ')' => {
                        if paren_depth > 0 {
                            paren_depth -= 1;
                        }
                    }
                    '{' => return Ok(()),
                    ';' => in_comment = true,
                    _ => {}
                }
            }
            self.skip().unwrap();
        }
    }

    fn parse_statements(&mut self) -> Result<Vec<ast::Statement>, ParseError> {
        self.consume_token("{")?;
        let mut statements = Vec::new();
        self.consume_whitespace();
        while self.peek()? != '}' {
            let statement = self.parse_statement()?;
            statements.push(statement);
            self.consume_whitespace();
        }
        self.consume_token("}")?;
        Ok(statements)
    }

    fn parse_name(&mut self, within: &'static str) -> Result<&'a str, ParseError> {
        let start = self.offset;
        let ch = self.next()?;
        if !is_ident_start(ch) {
            return Err(ParseError::UnexpectedCharacter(ch, within, self.location));
        }
        self.consume_while(is_ident);
        let end = self.offset;
        Ok(&self.source[start..end])
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, ParseError> {
        let keyword_location = self.location;
        let keyword = self.parse_name("keyword")?;
        self.consume_whitespace();
        if keyword == "let" {
            let variable = self.parse_variable()?;
            self.consume_whitespace();
            self.consume_token("=")?;
            self.consume_whitespace();
            let value = self.parse_expression()?;
            Ok(ast::DeclareImmutable {
                variable,
                value,
                location: keyword_location,
            }
            .into())
        } else if keyword == "var" {
            let variable = self.parse_variable()?;
            self.consume_whitespace();
            self.consume_token("=")?;
            self.consume_whitespace();
            let value = self.parse_expression()?;
            Ok(ast::DeclareMutable {
                variable,
                value,
                location: keyword_location,
            }
            .into())
        } else if keyword == "set" {
            let variable = self.parse_variable()?;
            self.consume_whitespace();
            self.consume_token("=")?;
            self.consume_whitespace();
            let value = self.parse_expression()?;
            Ok(ast::Assign {
                variable,
                value,
                location: keyword_location,
            }
            .into())
        } else if keyword == "node" {
            let node = self.parse_variable()?;
            Ok(ast::CreateGraphNode {
                node,
                location: keyword_location,
            }
            .into())
        } else if keyword == "edge" {
            let source = self.parse_expression()?;
            self.consume_whitespace();
            self.consume_token("->")?;
            self.consume_whitespace();
            let sink = self.parse_expression()?;
            Ok(ast::CreateEdge {
                source,
                sink,
                location: keyword_location,
            }
            .into())
        } else if keyword == "attr" {
            self.consume_token("(")?;
            self.consume_whitespace();
            let node_or_source = self.parse_expression()?;
            self.consume_whitespace();

            if self.peek()? == '-' {
                let source = node_or_source;
                self.consume_token("->")?;
                self.consume_whitespace();
                let sink = self.parse_expression()?;
                self.consume_whitespace();
                self.consume_token(")")?;
                self.consume_whitespace();
                let attributes = self.parse_attributes()?;
                Ok(ast::AddEdgeAttribute {
                    source,
                    sink,
                    attributes,
                    location: keyword_location,
                }
                .into())
            } else {
                let node = node_or_source;
                self.consume_whitespace();
                self.consume_token(")")?;
                self.consume_whitespace();
                let attributes = self.parse_attributes()?;
                Ok(ast::AddGraphNodeAttribute {
                    node,
                    attributes,
                    location: keyword_location,
                }
                .into())
            }
        } else if keyword == "print" {
            let mut values = vec![self.parse_expression()?];
            self.consume_whitespace();
            while self.try_peek() == Some(',') {
                self.consume_token(",")?;
                self.consume_whitespace();
                values.push(self.parse_expression()?);
                self.consume_whitespace();
            }
            self.consume_whitespace();
            Ok(ast::Print {
                values,
                location: keyword_location,
            }
            .into())
        } else if keyword == "scan" {
            let value = self.parse_expression()?;
            self.consume_whitespace();
            self.consume_token("{")?;
            self.consume_whitespace();
            let mut arms = Vec::new();
            while self.peek()? != '}' {
                let pattern_location = self.location;
                let pattern = self.parse_string()?;
                let regex = Regex::new(&pattern)
                    .map_err(|_| ParseError::InvalidRegex(pattern.into(), pattern_location))?;
                self.consume_whitespace();
                let statements = self.parse_statements()?;
                arms.push(ast::ScanArm {
                    regex,
                    statements,
                    location: keyword_location,
                });
                self.consume_whitespace();
            }
            self.consume_token("}")?;
            Ok(ast::Scan {
                value,
                arms,
                location: keyword_location,
            }
            .into())
        } else if keyword == "if" {
            let mut arms = Vec::new();

            // if
            let location = keyword_location;
            self.consume_whitespace();
            let conditions = self.parse_conditions()?;
            self.consume_whitespace();
            let statements = self.parse_statements()?;
            self.consume_whitespace();
            arms.push(ast::IfArm {
                conditions,
                statements,
                location,
            });

            // elif
            let mut location = self.location;
            while let Ok(_) = self.consume_token("elif") {
                self.consume_whitespace();
                let conditions = self.parse_conditions()?;
                self.consume_whitespace();
                let statements = self.parse_statements()?;
                self.consume_whitespace();
                arms.push(ast::IfArm {
                    conditions,
                    statements,
                    location,
                });
                self.consume_whitespace();
                location = self.location;
            }

            // else
            let location = self.location;
            if let Ok(_) = self.consume_token("else") {
                let conditions = vec![];
                self.consume_whitespace();
                let statements = self.parse_statements()?;
                self.consume_whitespace();
                arms.push(ast::IfArm {
                    conditions,
                    statements,
                    location,
                });
                self.consume_whitespace();
            }

            Ok(ast::If {
                arms,
                location: keyword_location,
            }
            .into())
        } else if keyword == "for" {
            self.consume_whitespace();
            let variable = self.parse_unscoped_variable()?;
            self.consume_whitespace();
            self.consume_token("in")?;
            self.consume_whitespace();
            let value = self.parse_expression()?;
            self.consume_whitespace();
            let statements = self.parse_statements()?;
            Ok(ast::ForIn {
                variable,
                value,
                statements,
                location: keyword_location,
            }
            .into())
        } else {
            Err(ParseError::UnexpectedKeyword(
                keyword.into(),
                keyword_location,
            ))
        }
    }

    fn parse_conditions(&mut self) -> Result<Vec<ast::Condition>, ParseError> {
        let mut conditions = Vec::new();
        let mut has_next = true;
        while has_next {
            conditions.push(self.parse_condition()?);
            self.consume_whitespace();
            if let Some(',') = self.try_peek() {
                self.consume_token(",")?;
                self.consume_whitespace();
                has_next = true;
            } else {
                has_next = false;
            }
        }
        Ok(conditions)
    }

    fn parse_condition(&mut self) -> Result<ast::Condition, ParseError> {
        let location = self.location;
        let condition = if let Ok(_) = self.consume_token("some") {
            self.consume_whitespace();
            let value = self.parse_expression()?;
            ast::Condition::Some { value, location }
        } else if let Ok(_) = self.consume_token("none") {
            self.consume_whitespace();
            let value = self.parse_expression()?;
            ast::Condition::None { value, location }
        } else if let Ok(value) = self.parse_expression() {
            self.consume_whitespace();
            ast::Condition::Bool { value, location }
        } else {
            return Err(ParseError::ExpectedToken(
                "(some|none)? EXPRESSION",
                location,
            ));
        };
        self.consume_whitespace();
        Ok(condition)
    }

    fn parse_identifier(&mut self, within: &'static str) -> Result<Identifier, ParseError> {
        let content = self.parse_name(within)?;
        Ok(Identifier::from(content))
    }

    fn parse_string(&mut self) -> Result<String, ParseError> {
        self.consume_token("\"")?;
        let mut escape = false;
        let mut value = String::new();
        loop {
            let ch = self.next()?;
            if escape {
                escape = false;
                value.push(match ch {
                    '0' => '\0',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    _ => ch,
                });
            } else {
                match ch {
                    '"' => return Ok(value),
                    '\\' => escape = true,
                    _ => value.push(ch),
                }
            }
        }
    }

    fn parse_expression(&mut self) -> Result<ast::Expression, ParseError> {
        let mut expression = match self.peek()? {
            '#' => self.parse_literal()?,
            '"' => self.parse_string()?.into(),
            '@' => self.parse_capture()?.into(),
            '$' => self.parse_regex_capture()?.into(),
            '(' => self.parse_call()?,
            '[' => self.parse_list()?,
            '{' => self.parse_set()?,
            ch if ch.is_ascii_digit() => self.parse_integer_constant()?,
            ch if is_ident_start(ch) => {
                let location = self.location;
                let name = self.parse_identifier("variable name")?;
                ast::UnscopedVariable { name, location }.into()
            }
            ch => {
                return Err(ParseError::UnexpectedCharacter(
                    ch,
                    "expression",
                    self.location,
                ))
            }
        };
        self.consume_whitespace();
        while self.try_peek() == Some('.') {
            self.skip().unwrap();
            self.consume_whitespace();
            let location = self.location;
            let scope = Box::new(expression);
            let name = self.parse_identifier("scoped variable name")?;
            self.consume_whitespace();
            expression = ast::ScopedVariable {
                scope,
                name,
                location,
            }
            .into();
        }
        Ok(expression)
    }

    fn parse_call(&mut self) -> Result<ast::Expression, ParseError> {
        self.consume_token("(")?;
        self.consume_whitespace();
        let function = self.parse_identifier("function name")?;
        self.consume_whitespace();
        let mut parameters = Vec::new();
        while self.peek()? != ')' {
            parameters.push(self.parse_expression()?);
            self.consume_whitespace();
        }
        self.consume_token(")")?;
        Ok(ast::Call {
            function,
            parameters,
        }
        .into())
    }

    fn parse_sequence(&mut self, end_marker: char) -> Result<Vec<ast::Expression>, ParseError> {
        let mut elements = Vec::new();
        while self.peek()? != end_marker {
            elements.push(self.parse_expression()?);
            self.consume_whitespace();
            if self.peek()? != end_marker {
                self.consume_token(",")?;
                self.consume_whitespace();
            }
        }
        Ok(elements)
    }

    fn parse_list(&mut self) -> Result<ast::Expression, ParseError> {
        let location = self.location;
        self.consume_token("[")?;
        self.consume_whitespace();
        if let Ok(_) = self.consume_token("]") {
            return Ok(ast::ListLiteral { elements: vec![] }.into());
        }
        let first_element = self.parse_expression()?;
        self.consume_whitespace();
        if let Ok(_) = self.consume_token("]") {
            let elements = vec![first_element];
            Ok(ast::ListLiteral { elements }.into())
        } else if let Ok(_) = self.consume_token(",") {
            self.consume_whitespace();
            let mut elements = self.parse_sequence(']')?;
            self.consume_whitespace();
            self.consume_token("]")?;
            elements.insert(0, first_element);
            Ok(ast::ListLiteral { elements }.into())
        } else {
            self.consume_token("for")?;
            self.consume_whitespace();
            let variable = self.parse_unscoped_variable()?;
            self.consume_whitespace();
            self.consume_token("in")?;
            self.consume_whitespace();
            let value = self.parse_expression()?;
            self.consume_whitespace();
            self.consume_token("]")?;
            Ok(ast::ListComprehension {
                element: first_element.into(),
                variable,
                value: value.into(),
                location,
            }
            .into())
        }
    }

    fn parse_set(&mut self) -> Result<ast::Expression, ParseError> {
        let location = self.location;
        self.consume_token("{")?;
        self.consume_whitespace();
        if let Ok(_) = self.consume_token("}") {
            return Ok(ast::SetLiteral { elements: vec![] }.into());
        }
        let first_element = self.parse_expression()?;
        self.consume_whitespace();
        if let Ok(_) = self.consume_token("}") {
            let elements = vec![first_element];
            Ok(ast::SetLiteral { elements }.into())
        } else if let Ok(_) = self.consume_token(",") {
            self.consume_whitespace();
            let mut elements = self.parse_sequence('}')?;
            self.consume_whitespace();
            self.consume_token("}")?;
            elements.insert(0, first_element);
            Ok(ast::SetLiteral { elements }.into())
        } else {
            self.consume_token("for")?;
            self.consume_whitespace();
            let variable = self.parse_unscoped_variable()?;
            self.consume_whitespace();
            self.consume_token("in")?;
            self.consume_whitespace();
            let value = self.parse_expression()?;
            self.consume_whitespace();
            self.consume_token("}")?;
            Ok(ast::SetComprehension {
                element: first_element.into(),
                variable,
                value: value.into(),
                location,
            }
            .into())
        }
    }

    fn parse_capture(&mut self) -> Result<ast::Capture, ParseError> {
        let location = self.location;
        let start = self.offset;
        self.consume_token("@")?;
        let ch = self.next()?;
        if !is_ident_start(ch) {
            return Err(ParseError::UnexpectedCharacter(
                ch,
                "query capture",
                self.location,
            ));
        }
        self.consume_while(is_ident);
        let end = self.offset;
        let name = Identifier::from(&self.source[start + 1..end]);
        Ok(ast::Capture {
            name,
            quantifier: Zero,                 // set in checker
            file_capture_index: usize::MAX,   // set in checker
            stanza_capture_index: usize::MAX, // set in checker
            location,
        }
        .into())
    }

    fn parse_integer_constant(&mut self) -> Result<ast::Expression, ParseError> {
        // We'll have already verified that the next digit is an integer.
        let start = self.offset;
        self.consume_while(|ch| ch.is_ascii_digit());
        let end = self.offset;
        let value = u32::from_str_radix(&self.source[start..end], 10).unwrap();
        Ok(ast::IntegerConstant { value }.into())
    }

    fn parse_literal(&mut self) -> Result<ast::Expression, ParseError> {
        let literal_location = self.location;
        self.consume_token("#")?;
        let literal = self.parse_name("literal")?;
        if literal == "false" {
            return Ok(ast::Expression::FalseLiteral);
        } else if literal == "null" {
            return Ok(ast::Expression::NullLiteral);
        } else if literal == "true" {
            return Ok(ast::Expression::TrueLiteral);
        } else {
            Err(ParseError::UnexpectedLiteral(
                literal.into(),
                literal_location,
            ))
        }
    }

    fn parse_regex_capture(&mut self) -> Result<ast::RegexCapture, ParseError> {
        let regex_capture_location = self.location;
        self.consume_token("$")?;
        let start = self.offset;
        self.consume_while(|ch| ch.is_ascii_digit());
        let end = self.offset;
        if start == end {
            return Err(ParseError::InvalidRegexCapture(regex_capture_location));
        }
        let match_index = usize::from_str_radix(&self.source[start..end], 10).unwrap();
        Ok(ast::RegexCapture { match_index }.into())
    }

    fn parse_attributes(&mut self) -> Result<Vec<ast::Attribute>, ParseError> {
        let mut attributes = vec![self.parse_attribute()?];
        self.consume_whitespace();
        while self.try_peek() == Some(',') {
            self.skip().unwrap();
            self.consume_whitespace();
            attributes.push(self.parse_attribute()?);
            self.consume_whitespace();
        }
        Ok(attributes)
    }

    fn parse_attribute(&mut self) -> Result<ast::Attribute, ParseError> {
        let name = self.parse_identifier("attribute name")?;
        self.consume_whitespace();
        let value = if self.try_peek() == Some('=') {
            self.consume_token("=")?;
            self.consume_whitespace();
            self.parse_expression()?
        } else {
            ast::Expression::TrueLiteral
        };
        Ok(ast::Attribute { name, value })
    }

    fn parse_variable(&mut self) -> Result<ast::Variable, ParseError> {
        let expression_location = self.location;
        match self.parse_expression()? {
            ast::Expression::Variable(variable) => Ok(variable),
            _ => Err(ParseError::ExpectedVariable(expression_location)),
        }
    }

    fn parse_unscoped_variable(&mut self) -> Result<ast::UnscopedVariable, ParseError> {
        match self.parse_variable()? {
            ast::Variable::Unscoped(variable) => Ok(variable),
            ast::Variable::Scoped(variable) => {
                Err(ParseError::ExpectedUnscopedVariable(variable.location))
            }
        }
    }
}
