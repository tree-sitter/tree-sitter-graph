// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2022, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

use thiserror::Error;
use tree_sitter::CaptureQuantifier;
use tree_sitter::CaptureQuantifier::One;
use tree_sitter::CaptureQuantifier::OneOrMore;
use tree_sitter::CaptureQuantifier::ZeroOrMore;
use tree_sitter::CaptureQuantifier::ZeroOrOne;
use tree_sitter::Query;

use crate::ast;
use crate::parser::FULL_MATCH;
use crate::variables::MutVariables;
use crate::variables::VariableError;
use crate::variables::VariableMap;
use crate::variables::Variables;
use crate::Location;

#[derive(Debug, Error)]
pub enum CheckError {
    #[error("Cannot hide global variable {0} at {1}")]
    CannotHideGlobalVariable(String, Location),
    #[error("Cannot set global variable {0} at {1}")]
    CannotSetGlobalVariable(String, Location),
    #[error("Duplicate global variable {0} at {1}")]
    DuplicateGlobalVariable(String, Location),
    #[error("Expected list value at {0}")]
    ExpectedListValue(Location),
    #[error("Expected local value at {0}")]
    ExpectedLocalValue(Location),
    #[error("Expected optional value at {0}")]
    ExpectedOptionalValue(Location),
    #[error("Nullable regular expression /{0}/ at {1}")]
    NullableRegex(String, Location),
    #[error("Undefined syntax capture @{0} at {1}")]
    UndefinedSyntaxCapture(String, Location),
    #[error("Undefined variable {0} at {1}")]
    UndefinedVariable(String, Location),
    #[error("{0}: {1}")]
    Variable(VariableError, String),
}

/// Checker context
struct CheckContext<'a> {
    globals: &'a dyn Variables<ExpressionResult>,
    file_query: &'a Query,
    stanza_index: usize,
    stanza_query: &'a Query,
    locals: &'a mut dyn MutVariables<ExpressionResult>,
}

//-----------------------------------------------------------------------------
// File

impl ast::File {
    pub fn check(&mut self) -> Result<(), CheckError> {
        let mut globals = VariableMap::new();
        for global in &self.globals {
            globals
                .add(
                    global.name.clone(),
                    ExpressionResult {
                        quantifier: global.quantifier,
                        is_local: true,
                    },
                    false,
                )
                .map_err(|_| {
                    CheckError::DuplicateGlobalVariable(
                        global.name.as_str().to_string(),
                        global.location,
                    )
                })?;
        }
        let file_query = self.query.as_ref().unwrap();
        for (index, stanza) in self.stanzas.iter_mut().enumerate() {
            stanza.check(&globals, file_query, index)?;
        }
        Ok(())
    }
}

//-----------------------------------------------------------------------------
// Stanza

impl ast::Stanza {
    fn check(
        &mut self,
        globals: &dyn Variables<ExpressionResult>,
        file_query: &Query,
        stanza_index: usize,
    ) -> Result<(), CheckError> {
        let mut locals = VariableMap::new();
        let mut ctx = CheckContext {
            globals,
            file_query,
            stanza_index,
            stanza_query: &self.query,
            locals: &mut locals,
        };
        self.full_match_file_capture_index =
            ctx.file_query
                .capture_index_for_name(FULL_MATCH)
                .expect("missing capture index for full match") as usize;
        for statement in &mut self.statements {
            statement.check(&mut ctx)?;
        }
        Ok(())
    }
}

//-----------------------------------------------------------------------------
// Statements

impl ast::Statement {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<(), CheckError> {
        match self {
            Self::DeclareImmutable(stmt) => stmt.check(ctx),
            Self::DeclareMutable(stmt) => stmt.check(ctx),
            Self::Assign(stmt) => stmt.check(ctx),
            Self::CreateGraphNode(stmt) => stmt.check(ctx),
            Self::AddGraphNodeAttribute(stmt) => stmt.check(ctx),
            Self::CreateEdge(stmt) => stmt.check(ctx),
            Self::AddEdgeAttribute(stmt) => stmt.check(ctx),
            Self::Scan(stmt) => stmt.check(ctx),
            Self::Print(stmt) => stmt.check(ctx),
            Self::If(stmt) => stmt.check(ctx),
            Self::ForIn(stmt) => stmt.check(ctx),
        }
    }
}

impl ast::DeclareImmutable {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<(), CheckError> {
        let value = self.value.check(ctx)?;
        self.variable.check_add(ctx, value, false)?;
        Ok(())
    }
}

impl ast::DeclareMutable {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<(), CheckError> {
        let value = self.value.check(ctx)?;
        self.variable.check_add(ctx, value, true)?;
        Ok(())
    }
}

impl ast::Assign {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<(), CheckError> {
        let value = self.value.check(ctx)?;
        self.variable.check_set(ctx, value)?;
        Ok(())
    }
}

impl ast::CreateGraphNode {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<(), CheckError> {
        self.node.check_add(
            ctx,
            ExpressionResult {
                is_local: true,
                quantifier: One,
            },
            false,
        )?;
        Ok(())
    }
}

impl ast::AddGraphNodeAttribute {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<(), CheckError> {
        self.node.check(ctx)?;
        for attribute in &mut self.attributes {
            attribute.check(ctx)?;
        }
        Ok(())
    }
}

impl ast::CreateEdge {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<(), CheckError> {
        self.source.check(ctx)?;
        self.sink.check(ctx)?;
        Ok(())
    }
}

impl ast::AddEdgeAttribute {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<(), CheckError> {
        self.source.check(ctx)?;
        self.sink.check(ctx)?;
        for attribute in &mut self.attributes {
            attribute.check(ctx)?;
        }
        Ok(())
    }
}

impl ast::Scan {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<(), CheckError> {
        let value_result = self.value.check(ctx)?;
        if !value_result.is_local {
            return Err(CheckError::ExpectedLocalValue(self.location));
        }

        for arm in &mut self.arms {
            // Be aware that this check is not complete, as it does not rule out
            // all regular expressions that admit empty matches. For example, th
            // regex "\b" matches empty strings within a larger non-empty one.
            // Therefore, there is also a runtime check that checks that a match was
            // non-empty. This is all to prevent non-termination of scan.
            if let Some(_) = arm.regex.captures("") {
                return Err(CheckError::NullableRegex(
                    arm.regex.to_string(),
                    arm.location,
                ));
            }

            let mut arm_locals = VariableMap::nested(ctx.locals);
            let mut arm_ctx = CheckContext {
                globals: ctx.globals,
                file_query: ctx.file_query,
                stanza_index: ctx.stanza_index,
                stanza_query: ctx.stanza_query,
                locals: &mut arm_locals,
            };

            for statement in &mut arm.statements {
                statement.check(&mut arm_ctx)?;
            }
        }
        Ok(())
    }
}

impl ast::Print {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<(), CheckError> {
        for value in &mut self.values {
            value.check(ctx)?;
        }
        Ok(())
    }
}

impl ast::If {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<(), CheckError> {
        for arm in &mut self.arms {
            for condition in &mut arm.conditions {
                condition.check(ctx)?;
            }

            let mut arm_locals = VariableMap::nested(ctx.locals);
            let mut arm_ctx = CheckContext {
                globals: ctx.globals,
                file_query: ctx.file_query,
                stanza_index: ctx.stanza_index,
                stanza_query: ctx.stanza_query,
                locals: &mut arm_locals,
            };

            for statement in &mut arm.statements {
                statement.check(&mut arm_ctx)?;
            }
        }
        Ok(())
    }
}

impl ast::Condition {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<(), CheckError> {
        match self {
            Self::None { value, location } | Self::Some { value, location } => {
                let value_result = value.check(ctx)?;
                if !value_result.is_local {
                    return Err(CheckError::ExpectedLocalValue(*location));
                }
                if value_result.quantifier != ZeroOrOne {
                    return Err(CheckError::ExpectedOptionalValue(*location));
                }
            }
            Self::Bool { value, location } => {
                let value_result = value.check(ctx)?;
                if !value_result.is_local {
                    return Err(CheckError::ExpectedLocalValue(*location));
                }
            }
        }
        Ok(())
    }
}

impl ast::ForIn {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<(), CheckError> {
        let value_result = self.value.check(ctx)?;
        if !value_result.is_local {
            return Err(CheckError::ExpectedLocalValue(self.location));
        }
        if value_result.quantifier != ZeroOrMore && value_result.quantifier != OneOrMore {
            return Err(CheckError::ExpectedListValue(self.location));
        }

        let mut loop_locals = VariableMap::nested(ctx.locals);
        let mut loop_ctx = CheckContext {
            globals: ctx.globals,
            file_query: ctx.file_query,
            stanza_index: ctx.stanza_index,
            stanza_query: ctx.stanza_query,
            locals: &mut loop_locals,
        };
        self.variable
            .check_add(&mut loop_ctx, value_result, false)?;
        for statement in &mut self.statements {
            statement.check(&mut loop_ctx)?;
        }
        Ok(())
    }
}

//-----------------------------------------------------------------------------
// Expressions

/// Expression checking result
#[derive(Clone, Debug)]
struct ExpressionResult {
    is_local: bool,
    quantifier: CaptureQuantifier,
}

impl ast::Expression {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        match self {
            Self::FalseLiteral => Ok(ExpressionResult {
                is_local: true,
                quantifier: One,
            }),
            Self::NullLiteral => Ok(ExpressionResult {
                is_local: true,
                quantifier: One,
            }),
            Self::TrueLiteral => Ok(ExpressionResult {
                is_local: true,
                quantifier: One,
            }),
            Self::IntegerConstant(expr) => expr.check(ctx),
            Self::StringConstant(expr) => expr.check(ctx),
            Self::ListLiteral(expr) => expr.check(ctx),
            Self::SetLiteral(expr) => expr.check(ctx),
            Self::ListComprehension(expr) => expr.check(ctx),
            Self::SetComprehension(expr) => expr.check(ctx),
            Self::Capture(expr) => expr.check(ctx),
            Self::Variable(expr) => expr.check_get(ctx),
            Self::Call(expr) => expr.check(ctx),
            Self::RegexCapture(expr) => expr.check(ctx),
        }
    }
}

impl ast::IntegerConstant {
    fn check(&mut self, _ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        Ok(ExpressionResult {
            is_local: true,
            quantifier: One,
        })
    }
}

impl ast::StringConstant {
    fn check(&mut self, _ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        Ok(ExpressionResult {
            is_local: true,
            quantifier: One,
        })
    }
}

impl ast::ListLiteral {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        let mut is_local = true;
        for element in &mut self.elements {
            let element_result = element.check(ctx)?;
            is_local &= element_result.is_local;
        }
        Ok(ExpressionResult {
            is_local,
            quantifier: ZeroOrMore,
        })
    }
}

impl ast::SetLiteral {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        let mut is_local = true;
        for element in &mut self.elements {
            let element_result = element.check(ctx)?;
            is_local &= element_result.is_local;
        }
        Ok(ExpressionResult {
            is_local,
            quantifier: ZeroOrMore,
        })
    }
}

impl ast::ListComprehension {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        let value_result = self.value.check(ctx)?;
        if !value_result.is_local {
            return Err(CheckError::ExpectedLocalValue(self.location));
        }
        if value_result.quantifier != ZeroOrMore && value_result.quantifier != OneOrMore {
            return Err(CheckError::ExpectedListValue(self.location));
        }

        let mut loop_locals = VariableMap::nested(ctx.locals);
        let mut loop_ctx = CheckContext {
            globals: ctx.globals,
            file_query: ctx.file_query,
            stanza_index: ctx.stanza_index,
            stanza_query: ctx.stanza_query,
            locals: &mut loop_locals,
        };
        self.variable
            .check_add(&mut loop_ctx, value_result, false)?;

        let element_result = self.element.check(&mut loop_ctx)?;
        Ok(ExpressionResult {
            is_local: element_result.is_local,
            quantifier: ZeroOrMore,
        })
    }
}

impl ast::SetComprehension {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        let value_result = self.value.check(ctx)?;
        if !value_result.is_local {
            return Err(CheckError::ExpectedLocalValue(self.location));
        }
        if value_result.quantifier != ZeroOrMore && value_result.quantifier != OneOrMore {
            return Err(CheckError::ExpectedListValue(self.location));
        }

        let mut loop_locals = VariableMap::nested(ctx.locals);
        let mut loop_ctx = CheckContext {
            globals: ctx.globals,
            file_query: ctx.file_query,
            stanza_index: ctx.stanza_index,
            stanza_query: ctx.stanza_query,
            locals: &mut loop_locals,
        };
        self.variable
            .check_add(&mut loop_ctx, value_result, false)?;

        let element_result = self.element.check(&mut loop_ctx)?;
        Ok(ExpressionResult {
            is_local: element_result.is_local,
            quantifier: ZeroOrMore,
        })
    }
}

impl ast::Capture {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        let name = self.name.to_string();
        self.stanza_capture_index = ctx
            .stanza_query
            .capture_index_for_name(&name)
            .ok_or_else(|| CheckError::UndefinedSyntaxCapture(name.clone(), self.location))?
            as usize;
        self.file_capture_index = ctx
            .file_query
            .capture_index_for_name(&name)
            .expect("missing capture index for name") as usize; // if the previous lookup succeeded, this one should succeed as well
        self.quantifier =
            ctx.file_query.capture_quantifiers(ctx.stanza_index)[self.file_capture_index];
        Ok(ExpressionResult {
            is_local: true,
            quantifier: self.quantifier,
        })
    }
}

impl ast::Call {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        let mut is_local = true;
        for parameter in &mut self.parameters {
            let parameter_result = parameter.check(ctx)?;
            is_local &= parameter_result.is_local;
        }
        Ok(ExpressionResult {
            is_local,
            quantifier: One, // FIXME we don't really know
        })
    }
}

impl ast::RegexCapture {
    fn check(&mut self, _ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        Ok(ExpressionResult {
            is_local: true,
            quantifier: One,
        })
    }
}

//-----------------------------------------------------------------------------
// Variables

impl ast::Variable {
    fn check_add(
        &mut self,
        ctx: &mut CheckContext,
        value: ExpressionResult,
        mutable: bool,
    ) -> Result<(), CheckError> {
        match self {
            Self::Unscoped(v) => v.check_add(ctx, value, mutable),
            Self::Scoped(v) => v.check_add(ctx, value, mutable),
        }
    }

    fn check_set(
        &mut self,
        ctx: &mut CheckContext,
        value: ExpressionResult,
    ) -> Result<(), CheckError> {
        match self {
            Self::Unscoped(v) => v.check_set(ctx, value),
            Self::Scoped(v) => v.check_set(ctx, value),
        }
    }

    fn check_get(&mut self, ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        match self {
            Self::Unscoped(v) => v.check_get(ctx),
            Self::Scoped(v) => v.check_get(ctx),
        }
    }
}

impl ast::UnscopedVariable {
    fn check_add(
        &mut self,
        ctx: &mut CheckContext,
        value: ExpressionResult,
        mutable: bool,
    ) -> Result<(), CheckError> {
        if ctx.globals.get(&self.name).is_some() {
            return Err(CheckError::CannotHideGlobalVariable(
                self.name.as_str().to_string(),
                self.location,
            ));
        }
        let mut value = value;
        // Mutable variables are not considered local, because a non-local
        // assignment in a loop could invalidate an earlier local assignment.
        // Since we process all statement in order, we don't have info on later
        // assignments, and can assume non-local to be sound.
        if mutable {
            value.is_local = false;
        }
        ctx.locals
            .add(self.name.clone(), value, mutable)
            .map_err(|e| CheckError::Variable(e, format!("{}", self.name)))
    }

    fn check_set(
        &mut self,
        ctx: &mut CheckContext,
        value: ExpressionResult,
    ) -> Result<(), CheckError> {
        if ctx.globals.get(&self.name).is_some() {
            return Err(CheckError::CannotSetGlobalVariable(
                self.name.as_str().to_string(),
                self.location,
            ));
        }
        let mut value = value;
        // Mutable variables are not considered local, because a non-local
        // assignment in a loop could invalidate an earlier local assignment.
        // Since we process all statement in order, we don't have info on later
        // assignments, and can assume non-local to be sound.
        value.is_local = false;
        ctx.locals
            .set(self.name.clone(), value)
            .map_err(|e| CheckError::Variable(e, format!("{}", self.name)))
    }

    fn check_get(&mut self, ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        if let Some(result) = ctx.globals.get(&self.name) {
            Some(result)
        } else {
            ctx.locals.get(&self.name)
        }
        .cloned()
        .ok_or_else(|| CheckError::UndefinedVariable(self.name.as_str().to_string(), self.location))
    }
}

impl ast::ScopedVariable {
    fn check_add(
        &mut self,
        ctx: &mut CheckContext,
        _value: ExpressionResult,
        _mutable: bool,
    ) -> Result<(), CheckError> {
        self.scope.check(ctx)?;
        Ok(())
    }

    fn check_set(
        &mut self,
        ctx: &mut CheckContext,
        _value: ExpressionResult,
    ) -> Result<(), CheckError> {
        self.scope.check(ctx)?;
        Ok(())
    }

    fn check_get(&mut self, ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        self.scope.check(ctx)?;
        Ok(ExpressionResult {
            is_local: false,
            quantifier: One, // FIXME we don't really know
        })
    }
}

//-----------------------------------------------------------------------------
// Attributes

impl ast::Attribute {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<(), CheckError> {
        self.value.check(ctx)?;
        Ok(())
    }
}
