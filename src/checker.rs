// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2022, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

use thiserror::Error;

use crate::ast;
use crate::variables::VariableError;
use crate::variables::VariableMap;
use crate::variables::Variables;
use crate::Context;
use crate::DisplayWithContext as _;

#[derive(Debug, Error)]
pub enum CheckError {
    #[error("{0}: {1}")]
    Variable(VariableError, String),
}

/// Checker context
struct CheckContext<'a> {
    ctx: &'a Context,
    locals: &'a mut dyn Variables<ExpressionResult>,
}

//-----------------------------------------------------------------------------
// File

impl ast::File {
    pub fn check(&mut self, ctx: &Context) -> Result<(), CheckError> {
        let locals = &mut VariableMap::new();
        let ctx = &mut CheckContext {
            ctx: ctx,
            locals: locals,
        };
        for stanza in &mut self.stanzas {
            stanza.check(ctx)?;
        }
        Ok(())
    }
}

//-----------------------------------------------------------------------------
// Stanza

impl ast::Stanza {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<(), CheckError> {
        for statement in &mut self.statements {
            statement.check(ctx)?;
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
        self.variable.add_check(ctx, value, false)?;
        Ok(())
    }
}

impl ast::DeclareMutable {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<(), CheckError> {
        let value = self.value.check(ctx)?;
        self.variable.add_check(ctx, value, true)?;
        Ok(())
    }
}

impl ast::Assign {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<(), CheckError> {
        let value = self.value.check(ctx)?;
        self.variable.set_check(ctx, value)?;
        Ok(())
    }
}

impl ast::CreateGraphNode {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<(), CheckError> {
        self.node.add_check(ctx, ExpressionResult {}, false)?;
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
        self.value.check(ctx)?;

        for arm in &mut self.arms {
            let mut arm_locals = VariableMap::new_child(ctx.locals);
            let mut arm_ctx = CheckContext {
                ctx: ctx.ctx,
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

            let mut arm_locals = VariableMap::new_child(ctx.locals);
            let mut arm_ctx = CheckContext {
                ctx: ctx.ctx,
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
        let captures = match self {
            Self::None(captures) => captures,
            Self::Some(captures) => captures,
        };
        for capture in captures {
            capture.check(ctx)?;
        }
        Ok(())
    }
}

impl ast::ForIn {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<(), CheckError> {
        let capture = self.capture.check(ctx)?;

        let mut loop_locals = VariableMap::new_child(ctx.locals);
        let mut loop_ctx = CheckContext {
            ctx: ctx.ctx,
            locals: &mut loop_locals,
        };
        self.variable.add_check(&mut loop_ctx, capture, false)?;
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
struct ExpressionResult {}

impl ast::Expression {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        match self {
            Self::FalseLiteral => Ok(ExpressionResult {}),
            Self::NullLiteral => Ok(ExpressionResult {}),
            Self::TrueLiteral => Ok(ExpressionResult {}),
            Self::IntegerConstant(expr) => expr.check(ctx),
            Self::StringConstant(expr) => expr.check(ctx),
            Self::List(expr) => expr.check(ctx),
            Self::Set(expr) => expr.check(ctx),
            Self::Capture(expr) => expr.check(ctx),
            Self::Variable(expr) => expr.get_check(ctx),
            Self::Call(expr) => expr.check(ctx),
            Self::RegexCapture(expr) => expr.check(ctx),
        }
    }
}

impl ast::ScanExpression {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        match self {
            Self::StringConstant(expr) => expr.check(ctx),
            Self::Capture(expr) => expr.check(ctx),
            Self::Variable(expr) => expr.get_check(ctx),
            Self::RegexCapture(expr) => expr.check(ctx),
        }
    }
}

impl ast::IntegerConstant {
    fn check(&mut self, _ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        Ok(ExpressionResult {})
    }
}

impl ast::StringConstant {
    fn check(&mut self, _ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        Ok(ExpressionResult {})
    }
}

impl ast::ListComprehension {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        for element in &mut self.elements {
            element.check(ctx)?;
        }
        Ok(ExpressionResult {})
    }
}

impl ast::SetComprehension {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        for element in &mut self.elements {
            element.check(ctx)?;
        }
        Ok(ExpressionResult {})
    }
}

impl ast::Capture {
    fn check(&mut self, _ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        Ok(ExpressionResult {})
    }
}

impl ast::Call {
    fn check(&mut self, ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        for parameter in &mut self.parameters {
            parameter.check(ctx)?;
        }
        Ok(ExpressionResult {})
    }
}

impl ast::RegexCapture {
    fn check(&mut self, _ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        Ok(ExpressionResult {})
    }
}

//-----------------------------------------------------------------------------
// Variables

impl ast::Variable {
    fn add_check(
        &mut self,
        ctx: &mut CheckContext,
        value: ExpressionResult,
        mutable: bool,
    ) -> Result<(), CheckError> {
        match self {
            Self::Unscoped(v) => v.add_check(ctx, value, mutable),
            Self::Scoped(v) => v.add_check(ctx, value, mutable),
        }
    }

    fn set_check(
        &mut self,
        ctx: &mut CheckContext,
        value: ExpressionResult,
    ) -> Result<(), CheckError> {
        match self {
            Self::Unscoped(v) => v.set_check(ctx, value),
            Self::Scoped(v) => v.set_check(ctx, value),
        }
    }

    fn get_check(&mut self, ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        match self {
            Self::Unscoped(v) => v.get_check(ctx),
            Self::Scoped(v) => v.get_check(ctx),
        }
    }
}

impl ast::UnscopedVariable {
    fn add_check(
        &mut self,
        ctx: &mut CheckContext,
        value: ExpressionResult,
        mutable: bool,
    ) -> Result<(), CheckError> {
        ctx.locals
            .add(self.name, value, mutable)
            .map_err(|e| CheckError::Variable(e, format!("{}", self.name.display_with(ctx.ctx))))
    }

    fn set_check(
        &mut self,
        ctx: &mut CheckContext,
        value: ExpressionResult,
    ) -> Result<(), CheckError> {
        ctx.locals
            .set(self.name, value)
            .map_err(|e| CheckError::Variable(e, format!("{}", self.name.display_with(ctx.ctx))))
    }

    fn get_check(&mut self, ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        // If the variable is not found, we return a default value for a possible global variable.
        let value = ctx
            .locals
            .get(&self.name)
            .cloned()
            .unwrap_or_else(|| ExpressionResult {});
        Ok(value)
    }
}

impl ast::ScopedVariable {
    fn add_check(
        &mut self,
        _ctx: &mut CheckContext,
        _value: ExpressionResult,
        _mutable: bool,
    ) -> Result<(), CheckError> {
        Ok(())
    }

    fn set_check(
        &mut self,
        _ctx: &mut CheckContext,
        _value: ExpressionResult,
    ) -> Result<(), CheckError> {
        Ok(())
    }

    fn get_check(&mut self, _ctx: &mut CheckContext) -> Result<ExpressionResult, CheckError> {
        Ok(ExpressionResult {})
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
