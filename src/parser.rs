// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

use std::fmt::Display;
use std::iter::Peekable;
use std::str::Chars;

use regex::Regex;
use thiserror::Error;
use tree_sitter::Language;
use tree_sitter::Query;
use tree_sitter::QueryError;

use crate::ast;
use crate::Context;
use crate::Identifier;

impl ast::File {
    /// Parses a graph DSL file, adding its content to an existing `File` instance.
    pub fn parse(&mut self, ctx: &mut Context, content: &str) -> Result<(), ParseError> {
        Parser::new(ctx, content).parse_into_file(self)
    }
}

/// An error that can occur while parsing a graph DSL file
#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Expected '{0}' at {1}")]
    ExpectedToken(&'static str, Location),
    #[error("Expected variable name at {0}")]
    ExpectedVariable(Location),
    #[error("Invalid regular expression /{0}/ at {1}")]
    InvalidRegex(String, Location),
    #[error("Nullable regular expression /{0}/ at {1}")]
    NullableRegex(String, Location),
    #[error("Expected integer constant in regex capture at {0}")]
    InvalidRegexCapture(Location),
    // TODO: The positions in the wrapped QueryError will be incorrect, since they will count the
    // row/column from the start of the query, not from the start of the file.
    #[error("Invalid query pattern: {}", _0.message)]
    QueryError(#[from] QueryError),
    #[error("Undefined query capture '{0}' at {1}")]
    UndefinedCapture(String, Location),
    #[error("Unexpected character '{0}' in {1} at {2}")]
    UnexpectedCharacter(char, &'static str, Location),
    #[error("Unexpected end of file at {0}")]
    UnexpectedEOF(Location),
    #[error("Unexpected keyword '{0}' at {1}")]
    UnexpectedKeyword(String, Location),
    #[error("Unexpected literal '#{0}' at {1}")]
    UnexpectedLiteral(String, Location),
}

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
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}, {})", self.row + 1, self.column + 1)
    }
}

struct Parser<'a> {
    ctx: &'a mut Context,
    source: &'a str,
    chars: Peekable<Chars<'a>>,
    offset: usize,
    location: Location,
    // Keywords
    attr_keyword: Identifier,
    edge_keyword: Identifier,
    false_keyword: Identifier,
    for_keyword: Identifier,
    if_keyword: Identifier,
    let_keyword: Identifier,
    node_keyword: Identifier,
    null_keyword: Identifier,
    print_keyword: Identifier,
    scan_keyword: Identifier,
    set_keyword: Identifier,
    true_keyword: Identifier,
    var_keyword: Identifier,
}

fn is_ident_start(c: char) -> bool {
    c == '_' || c.is_alphabetic()
}

fn is_ident(c: char) -> bool {
    c == '_' || c == '-' || c.is_alphanumeric()
}

impl<'a> Parser<'a> {
    fn new(ctx: &'a mut Context, source: &'a str) -> Parser<'a> {
        let chars = source.chars().peekable();
        let attr_keyword = ctx.add_identifier("attr");
        let edge_keyword = ctx.add_identifier("edge");
        let false_keyword = ctx.add_identifier("false");
        let for_keyword = ctx.add_identifier("for");
        let if_keyword = ctx.add_identifier("if");
        let let_keyword = ctx.add_identifier("let");
        let node_keyword = ctx.add_identifier("node");
        let null_keyword = ctx.add_identifier("null");
        let print_keyword = ctx.add_identifier("print");
        let scan_keyword = ctx.add_identifier("scan");
        let set_keyword = ctx.add_identifier("set");
        let true_keyword = ctx.add_identifier("true");
        let var_keyword = ctx.add_identifier("var");
        Parser {
            ctx,
            source,
            chars,
            offset: 0,
            location: Location::default(),
            attr_keyword,
            edge_keyword,
            false_keyword,
            for_keyword,
            let_keyword,
            if_keyword,
            node_keyword,
            null_keyword,
            print_keyword,
            scan_keyword,
            set_keyword,
            true_keyword,
            var_keyword,
        }
    }
}

impl Parser<'_> {
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
            let stanza = self.parse_stanza(file.language)?;
            file.stanzas.push(stanza);
            self.consume_whitespace();
        }
        Ok(())
    }

    fn parse_stanza(&mut self, language: Language) -> Result<ast::Stanza, ParseError> {
        let location = self.location;
        let query = self.parse_query(language)?;
        self.consume_whitespace();
        let statements = self.parse_statements(&query)?;
        Ok(ast::Stanza {
            query,
            statements,
            location,
        })
    }

    fn parse_query(&mut self, language: Language) -> Result<Query, ParseError> {
        let query_start = self.offset;
        self.skip_query()?;
        let query_end = self.offset;
        let query_source = &self.source[query_start..query_end];
        Ok(Query::new(language, query_source)?)
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

    fn parse_statements(
        &mut self,
        current_query: &Query,
    ) -> Result<Vec<ast::Statement>, ParseError> {
        self.consume_token("{")?;
        let mut statements = Vec::new();
        self.consume_whitespace();
        while self.peek()? != '}' {
            let statement = self.parse_statement(current_query)?;
            statements.push(statement);
            self.consume_whitespace();
        }
        self.consume_token("}")?;
        Ok(statements)
    }

    fn parse_statement(&mut self, current_query: &Query) -> Result<ast::Statement, ParseError> {
        let keyword_location = self.location;
        let keyword = self.parse_identifier("keyword")?;
        self.consume_whitespace();
        if keyword == self.let_keyword {
            let variable = self.parse_variable(current_query)?;
            self.consume_whitespace();
            self.consume_token("=")?;
            self.consume_whitespace();
            let value = self.parse_expression(current_query)?;
            Ok(ast::DeclareImmutable {
                variable,
                value,
                location: keyword_location,
            }
            .into())
        } else if keyword == self.var_keyword {
            let variable = self.parse_variable(current_query)?;
            self.consume_whitespace();
            self.consume_token("=")?;
            self.consume_whitespace();
            let value = self.parse_expression(current_query)?;
            Ok(ast::DeclareMutable {
                variable,
                value,
                location: keyword_location,
            }
            .into())
        } else if keyword == self.set_keyword {
            let variable = self.parse_variable(current_query)?;
            self.consume_whitespace();
            self.consume_token("=")?;
            self.consume_whitespace();
            let value = self.parse_expression(current_query)?;
            Ok(ast::Assign {
                variable,
                value,
                location: keyword_location,
            }
            .into())
        } else if keyword == self.node_keyword {
            let node = self.parse_variable(current_query)?;
            Ok(ast::CreateGraphNode {
                node,
                location: keyword_location,
            }
            .into())
        } else if keyword == self.edge_keyword {
            let source = self.parse_expression(current_query)?;
            self.consume_whitespace();
            self.consume_token("->")?;
            self.consume_whitespace();
            let sink = self.parse_expression(current_query)?;
            Ok(ast::CreateEdge {
                source,
                sink,
                location: keyword_location,
            }
            .into())
        } else if keyword == self.attr_keyword {
            self.consume_token("(")?;
            self.consume_whitespace();
            let node_or_source = self.parse_expression(current_query)?;
            self.consume_whitespace();

            if self.peek()? == '-' {
                let source = node_or_source;
                self.consume_token("->")?;
                self.consume_whitespace();
                let sink = self.parse_expression(current_query)?;
                self.consume_whitespace();
                self.consume_token(")")?;
                self.consume_whitespace();
                let attributes = self.parse_attributes(current_query)?;
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
                let attributes = self.parse_attributes(current_query)?;
                Ok(ast::AddGraphNodeAttribute {
                    node,
                    attributes,
                    location: keyword_location,
                }
                .into())
            }
        } else if keyword == self.print_keyword {
            let mut values = vec![self.parse_expression(current_query)?];
            self.consume_whitespace();
            while self.try_peek() == Some(',') {
                self.consume_token(",")?;
                self.consume_whitespace();
                values.push(self.parse_expression(current_query)?);
                self.consume_whitespace();
            }
            self.consume_whitespace();
            Ok(ast::Print {
                values,
                location: keyword_location,
            }
            .into())
        } else if keyword == self.scan_keyword {
            let value = self.parse_expression(current_query)?;
            self.consume_whitespace();
            self.consume_token("{")?;
            self.consume_whitespace();
            let mut arms = Vec::new();
            while self.peek()? != '}' {
                let pattern_location = self.location;
                let pattern = self.parse_string()?;
                let regex = match Regex::new(&pattern) {
                    Ok(regex) => {
                        if let Some(_) = regex.captures("") {
                            return Err(ParseError::NullableRegex(
                                pattern.into(),
                                pattern_location,
                            ));
                        }
                        regex
                    }
                    Err(_) => {
                        return Err(ParseError::InvalidRegex(pattern.into(), pattern_location))
                    }
                };
                self.consume_whitespace();
                let statements = self.parse_statements(current_query)?;
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
        } else if keyword == self.if_keyword {
            let mut arms = Vec::new();

            // if
            let location = keyword_location;
            let condition = self.parse_expression(current_query)?;
            self.consume_whitespace();
            let statements = self.parse_statements(current_query)?;
            self.consume_whitespace();
            arms.push(ast::ConditionalArm {
                condition,
                statements,
                location,
            });

            // elif
            let mut location = self.location;
            while let Ok(_) = self.consume_token("elif") {
                self.consume_whitespace();
                let condition = self.parse_expression(current_query)?;
                self.consume_whitespace();
                let statements = self.parse_statements(current_query)?;
                self.consume_whitespace();
                arms.push(ast::ConditionalArm {
                    condition,
                    statements,
                    location,
                });
                self.consume_whitespace();
                location = self.location;
            }

            // else
            let location = self.location;
            if let Ok(_) = self.consume_token("else") {
                let condition = ast::Expression::TrueLiteral;
                self.consume_whitespace();
                let statements = self.parse_statements(current_query)?;
                self.consume_whitespace();
                arms.push(ast::ConditionalArm {
                    condition,
                    statements,
                    location,
                });
                self.consume_whitespace();
            }

            Ok(ast::Conditional {
                arms,
                location: keyword_location,
            }
            .into())
        } else if keyword == self.for_keyword {
            let name = self.parse_identifier("loop variable name")?;
            self.consume_whitespace();
            self.consume_token("in")?;
            self.consume_whitespace();
            let values = self.parse_expression(current_query)?;
            self.consume_whitespace();
            let statements = self.parse_statements(current_query)?;
            Ok(ast::ForIn {
                name,
                values,
                statements,
                location: keyword_location,
            }
            .into())
        } else {
            Err(ParseError::UnexpectedKeyword(
                self.ctx.resolve(keyword).into(),
                keyword_location,
            ))
        }
    }

    fn parse_identifier(&mut self, within: &'static str) -> Result<Identifier, ParseError> {
        let start = self.offset;
        let ch = self.next()?;
        if !is_ident_start(ch) {
            return Err(ParseError::UnexpectedCharacter(ch, within, self.location));
        }
        self.consume_while(is_ident);
        let end = self.offset;
        Ok(self.ctx.add_identifier(&self.source[start..end]))
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

    fn parse_expression(&mut self, current_query: &Query) -> Result<ast::Expression, ParseError> {
        let mut expression = match self.peek()? {
            '#' => self.parse_literal()?,
            '"' => self.parse_string()?.into(),
            '@' => self.parse_capture(current_query)?,
            '$' => self.parse_regex_capture()?,
            '(' => self.parse_call(current_query)?,
            '[' => self.parse_list(current_query)?,
            '{' => self.parse_set(current_query)?,
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
        if self.try_peek() == Some('?') {
            self.skip().unwrap();
            self.consume_whitespace();
            let value = Box::new(expression);
            expression = ast::NotNull { value }.into();
        }
        Ok(expression)
    }

    fn parse_call(&mut self, current_query: &Query) -> Result<ast::Expression, ParseError> {
        self.consume_token("(")?;
        self.consume_whitespace();
        let function = self.parse_identifier("function name")?;
        self.consume_whitespace();
        let mut parameters = Vec::new();
        while self.peek()? != ')' {
            parameters.push(self.parse_expression(current_query)?);
            self.consume_whitespace();
        }
        self.consume_token(")")?;
        Ok(ast::Call {
            function,
            parameters,
        }
        .into())
    }

    fn parse_sequence(
        &mut self,
        current_query: &Query,
        end_marker: char,
    ) -> Result<Vec<ast::Expression>, ParseError> {
        let mut elements = Vec::new();
        while self.peek()? != end_marker {
            elements.push(self.parse_expression(current_query)?);
            self.consume_whitespace();
            if self.peek()? != end_marker {
                self.consume_token(",")?;
                self.consume_whitespace();
            }
        }
        Ok(elements)
    }

    fn parse_list(&mut self, current_query: &Query) -> Result<ast::Expression, ParseError> {
        self.consume_token("[")?;
        self.consume_whitespace();
        let elements = self.parse_sequence(current_query, ']')?;
        self.consume_token("]")?;
        Ok(ast::ListComprehension { elements }.into())
    }

    fn parse_set(&mut self, current_query: &Query) -> Result<ast::Expression, ParseError> {
        self.consume_token("{")?;
        self.consume_whitespace();
        let elements = self.parse_sequence(current_query, '}')?;
        self.consume_token("}")?;
        Ok(ast::SetComprehension { elements }.into())
    }

    fn parse_capture(&mut self, current_query: &Query) -> Result<ast::Expression, ParseError> {
        let capture_location = self.location;
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
        let capture_name = &self.source[start + 1..end];
        let index = current_query
            .capture_names()
            .iter()
            .position(|c| c == capture_name);
        let index = match index {
            Some(index) => index,
            None => {
                return Err(ParseError::UndefinedCapture(
                    capture_name.into(),
                    capture_location,
                ))
            }
        };
        let quantifier = current_query.capture_quantifiers(0)[index];
        let name = self.ctx.add_identifier(&self.source[start..end]);
        Ok(ast::Capture {
            index,
            quantifier,
            name,
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
        let literal = self.parse_identifier("literal")?;
        if literal == self.false_keyword {
            return Ok(ast::Expression::FalseLiteral);
        } else if literal == self.null_keyword {
            return Ok(ast::Expression::NullLiteral);
        } else if literal == self.true_keyword {
            return Ok(ast::Expression::TrueLiteral);
        } else {
            Err(ParseError::UnexpectedLiteral(
                self.ctx.resolve(literal).into(),
                literal_location,
            ))
        }
    }

    fn parse_regex_capture(&mut self) -> Result<ast::Expression, ParseError> {
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

    fn parse_attributes(
        &mut self,
        current_query: &Query,
    ) -> Result<Vec<ast::Attribute>, ParseError> {
        let mut attributes = vec![self.parse_attribute(current_query)?];
        self.consume_whitespace();
        while self.try_peek() == Some(',') {
            self.skip().unwrap();
            self.consume_whitespace();
            attributes.push(self.parse_attribute(current_query)?);
            self.consume_whitespace();
        }
        Ok(attributes)
    }

    fn parse_attribute(&mut self, current_query: &Query) -> Result<ast::Attribute, ParseError> {
        let name = self.parse_identifier("attribute name")?;
        self.consume_whitespace();
        let value = if self.try_peek() == Some('=') {
            self.consume_token("=")?;
            self.consume_whitespace();
            self.parse_expression(current_query)?
        } else {
            ast::Expression::TrueLiteral
        };
        Ok(ast::Attribute { name, value })
    }

    fn parse_variable(&mut self, current_query: &Query) -> Result<ast::Variable, ParseError> {
        let expression_location = self.location;
        match self.parse_expression(current_query)? {
            ast::Expression::Variable(variable) => Ok(variable),
            _ => Err(ParseError::ExpectedVariable(expression_location)),
        }
    }
}
