// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

use anyhow::Context as _;
use log::{debug, trace};

use tree_sitter::CaptureQuantifier::One;
use tree_sitter::Query;
use tree_sitter::QueryCursor;
use tree_sitter::QueryMatch;
use tree_sitter::Tree;

use crate::ast::AddEdgeAttribute;
use crate::ast::AddGraphNodeAttribute;
use crate::ast::Assign;
use crate::ast::Attribute;
use crate::ast::Call;
use crate::ast::Capture;
use crate::ast::Conditional;
use crate::ast::CreateEdge;
use crate::ast::CreateGraphNode;
use crate::ast::DeclareImmutable;
use crate::ast::DeclareMutable;
use crate::ast::Expression;
use crate::ast::IntegerConstant;
use crate::ast::ListComprehension;
use crate::ast::NotNull;
use crate::ast::Print;
use crate::ast::RegexCapture;
use crate::ast::Scan;
use crate::ast::ScopedVariable;
use crate::ast::SetComprehension;
use crate::ast::Stanza;
use crate::ast::Statement;
use crate::ast::StringConstant;
use crate::ast::UnscopedVariable;
use crate::ast::Variable;
use crate::execution::query_capture_value;
use crate::execution::ExecutionError;
use crate::graph::DisplayWithGraph as _;
use crate::graph::Graph;
use crate::graph::Value;
use crate::parser::FULL_MATCH;
use crate::Context;
use crate::DisplayWithContext;
use crate::Identifier;

use super::values as lazy;
use super::variables::BranchVariables;
use super::variables::LazyVariables;
use super::variables::LoopVariables;
use super::variables::Mutability;
use super::variables::VariableContext;
use super::variables::Variables;

pub fn full_match_capture_index(query: &Query) -> usize {
    query
        .capture_names()
        .iter()
        .position(|c| c == FULL_MATCH)
        .unwrap()
}

pub struct ExecutionContext<'a, 'tree> {
    ctx: &'a mut Context,
    graph: &'a mut Graph<'tree>,
    variables: &'a mut dyn Variables,
    mat: &'a QueryMatch<'a, 'tree>,
    store: &'a mut lazy::Store,
    scoped_store: &'a mut lazy::ScopedVariables,
    lazy_graph: &'a mut Vec<lazy::Statement>,
    regex_captures_identifier: Identifier,
}

impl Stanza {
    pub fn execute_lazy<'tree>(
        &self,
        ctx: &mut Context,
        tree: &'tree Tree,
        source: &'tree str,
        graph: &mut Graph<'tree>,
        variables: &mut LazyVariables,
        cursor: &mut QueryCursor,
        store: &mut lazy::Store,
        scoped_store: &mut lazy::ScopedVariables,
        lazy_graph: &mut Vec<lazy::Statement>,
        regex_captures_identifier: Identifier,
    ) -> Result<(), ExecutionError> {
        let full_match_index = full_match_capture_index(&self.query);
        let matches = cursor.matches(&self.query, tree.root_node(), source.as_bytes());
        for mat in matches {
            variables.clear();
            let mut exec = ExecutionContext {
                ctx,
                graph,
                variables,
                mat: &mat,
                store,
                scoped_store,
                lazy_graph,
                regex_captures_identifier,
            };
            let node = query_capture_value(full_match_index, One, &mat, exec.graph);
            debug!(
                "match {} at {}",
                node.display_with(exec.graph),
                self.location
            );
            trace!("{{");
            for statement in &self.statements {
                statement
                    .execute_lazy(&mut exec)
                    .with_context(|| format!("Executing {}", statement.display_with(exec.ctx)))?;
            }
            trace!("}}");
        }
        Ok(())
    }
}

impl Statement {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        debug!("exec {}", self.display_with(exec.ctx));
        match self {
            Statement::DeclareImmutable(statement) => statement.execute_lazy(exec),
            Statement::DeclareMutable(statement) => statement.execute_lazy(exec),
            Statement::Assign(statement) => statement.execute_lazy(exec),
            Statement::CreateGraphNode(statement) => statement.execute_lazy(exec),
            Statement::AddGraphNodeAttribute(statement) => statement.execute_lazy(exec),
            Statement::CreateEdge(statement) => statement.execute_lazy(exec),
            Statement::AddEdgeAttribute(statement) => statement.execute_lazy(exec),
            Statement::Scan(statement) => statement.execute_lazy(exec),
            Statement::Print(statement) => statement.execute_lazy(exec),
            Statement::Conditional(statement) => statement.execute_lazy(exec),
            Statement::ForIn(_) => Ok(()),
        }
    }
}

impl DeclareImmutable {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let value = self.value.evaluate_lazy(exec)?;
        self.variable.define(value, Mutability::Immutable, exec)
    }
}

impl DeclareMutable {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let value = self.value.evaluate_lazy(exec)?;
        self.variable.define(value, Mutability::Mutable, exec)
    }
}

impl Assign {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let value = self.value.evaluate_lazy(exec)?;
        self.variable.assign(value, exec)
    }
}

impl CreateGraphNode {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let graph_node = exec.graph.add_graph_node();
        let value = Value::GraphNode(graph_node);
        self.node.define(value.into(), Mutability::Immutable, exec)
    }
}

impl AddGraphNodeAttribute {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let node = self.node.evaluate_lazy(exec)?;
        let attributes = self
            .attributes
            .iter()
            .map(|a| a.evaluate_lazy(exec))
            .collect::<Result<_, _>>()?;
        let stmt = lazy::AddGraphNodeAttribute::new(node, attributes, self.location.into());
        exec.lazy_graph.push(stmt.into());
        Ok(())
    }
}

impl CreateEdge {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let source = self.source.evaluate_lazy(exec)?;
        let sink = self.sink.evaluate_lazy(exec)?;
        let stmt = lazy::CreateEdge::new(source, sink, self.location.into());
        exec.lazy_graph.push(stmt.into());
        Ok(())
    }
}

impl AddEdgeAttribute {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let source = self.source.evaluate_lazy(exec)?;
        let sink = self.sink.evaluate_lazy(exec)?;
        let attributes = self
            .attributes
            .iter()
            .map(|a| a.evaluate_lazy(exec))
            .collect::<Result<_, _>>()?;
        let stmt = lazy::AddEdgeAttribute::new(source, sink, attributes, self.location.into());
        exec.lazy_graph.push(stmt.into());
        Ok(())
    }
}

impl Scan {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let match_value = self.value.evaluate_lazy(exec)?;

        let regexes = self
            .arms
            .iter()
            .map(|a| a.regex.clone())
            .collect::<Vec<_>>();

        let scan_value: lazy::Value = exec
            .store
            .add(
                lazy::Scan::new(match_value, regexes).into(),
                self.location.into(),
                exec.ctx,
                exec.graph,
            )
            .into();

        let mut scan_variables =
            LoopVariables::new(exec.variables, scan_value.clone(), self.location.into());
        let arm_index: lazy::Value =
            lazy::ListIndex::new(scan_variables.iteration_value().clone(), 0.into()).into();
        let regex_captures =
            lazy::ListIndex::new(scan_variables.iteration_value().clone(), 1.into()).into();
        scan_variables.add(
            exec.regex_captures_identifier,
            regex_captures,
            Mutability::Immutable,
            &mut VariableContext {
                ctx: exec.ctx,
                graph: exec.graph,
                store: exec.store,
            },
        )?;

        let mut arm_variables = BranchVariables::new(&mut scan_variables, arm_index.clone());
        let mut lazy_graph_branches = Vec::new();
        for arm in self.arms.iter() {
            let mut lazy_graph = Vec::new();
            let mut arm_exec = ExecutionContext {
                ctx: exec.ctx,
                graph: exec.graph,
                variables: &mut arm_variables,
                mat: exec.mat,
                store: exec.store,
                scoped_store: exec.scoped_store,
                lazy_graph: &mut lazy_graph,
                regex_captures_identifier: exec.regex_captures_identifier,
            };
            for stmt in &arm.statements {
                stmt.execute_lazy(&mut arm_exec)?;
            }
            lazy_graph_branches.push(lazy_graph);

            arm_variables.next();
        }

        let arms_statement =
            lazy::BranchStatement::new(arm_index, lazy_graph_branches, self.location.into()).into();
        let scan_statement =
            lazy::LoopStatement::new(scan_value, vec![arms_statement], self.location.into()).into();
        exec.lazy_graph.push(scan_statement);

        Ok(())
    }
}

impl Print {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let mut arguments = Vec::new();
        for value in &self.values {
            let argument = if let Expression::StringConstant(expr) = value {
                lazy::PrintArgument::Text(expr.value.clone())
            } else {
                lazy::PrintArgument::Value(value.evaluate_lazy(exec)?)
            };
            arguments.push(argument);
        }
        let stmt = lazy::Print::new(arguments, self.location.into());
        exec.lazy_graph.push(stmt.into());
        Ok(())
    }
}

impl Conditional {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let conditions = self
            .arms
            .iter()
            .map(|e| e.condition.evaluate_lazy(exec))
            .collect::<Result<Vec<_>, _>>()?;

        let arm_index: lazy::Value = exec
            .store
            .add(
                lazy::FindFirst::new(conditions.into()).into(),
                self.location.into(),
                exec.ctx,
                exec.graph,
            )
            .into();

        let mut arm_variables = BranchVariables::new(exec.variables, arm_index.clone());
        let mut lazy_graph_branches = Vec::new();
        for arm in self.arms.iter() {
            let mut lazy_graph = Vec::new();
            let mut arm_exec = ExecutionContext {
                ctx: exec.ctx,
                graph: exec.graph,
                variables: &mut arm_variables,
                mat: exec.mat,
                store: exec.store,
                scoped_store: exec.scoped_store,
                lazy_graph: &mut lazy_graph,
                regex_captures_identifier: exec.regex_captures_identifier,
            };
            for stmt in &arm.statements {
                stmt.execute_lazy(&mut arm_exec)?;
            }
            lazy_graph_branches.push(lazy_graph);

            arm_variables.next();
        }

        let arms_statement =
            lazy::BranchStatement::new(arm_index, lazy_graph_branches, self.location.into()).into();
        exec.lazy_graph.push(arms_statement);

        Ok(())
    }
}

impl Expression {
    fn evaluate_lazy(&self, exec: &mut ExecutionContext) -> Result<lazy::Value, ExecutionError> {
        match self {
            Expression::FalseLiteral => Ok(false.into()),
            Expression::NullLiteral => Ok(Value::Null.into()),
            Expression::TrueLiteral => Ok(true.into()),
            Expression::IntegerConstant(expr) => expr.evaluate_lazy(exec),
            Expression::StringConstant(expr) => expr.evaluate_lazy(exec),
            Expression::List(expr) => expr.evaluate_lazy(exec),
            Expression::Set(expr) => expr.evaluate_lazy(exec),
            Expression::Capture(expr) => expr.evaluate_lazy(exec),
            Expression::Variable(expr) => expr.evaluate_lazy(exec),
            Expression::Call(expr) => expr.evaluate_lazy(exec),
            Expression::RegexCapture(expr) => expr.evaluate_lazy(exec),
            Expression::NotNull(expr) => expr.evaluate_lazy(exec),
        }
    }
}

impl IntegerConstant {
    fn evaluate_lazy(&self, _exec: &mut ExecutionContext) -> Result<lazy::Value, ExecutionError> {
        Ok(self.value.into())
    }
}

impl StringConstant {
    fn evaluate_lazy(&self, _exec: &mut ExecutionContext) -> Result<lazy::Value, ExecutionError> {
        Ok(self.value.clone().into())
    }
}

impl ListComprehension {
    fn evaluate_lazy(&self, exec: &mut ExecutionContext) -> Result<lazy::Value, ExecutionError> {
        let elements: Vec<lazy::Value> = self
            .elements
            .iter()
            .map(|e| e.evaluate_lazy(exec))
            .collect::<Result<_, _>>()?;
        Ok(elements.into())
    }
}

impl SetComprehension {
    fn evaluate_lazy(&self, exec: &mut ExecutionContext) -> Result<lazy::Value, ExecutionError> {
        let elements = self
            .elements
            .iter()
            .map(|e| e.evaluate_lazy(exec))
            .collect::<Result<_, _>>()?;
        Ok(lazy::Set::new(elements).into())
    }
}

impl Capture {
    fn evaluate_lazy(&self, exec: &mut ExecutionContext) -> Result<lazy::Value, ExecutionError> {
        let value = query_capture_value(self.index, self.quantifier, exec.mat, exec.graph);
        Ok(value.into())
    }
}

impl Call {
    fn evaluate_lazy(&self, exec: &mut ExecutionContext) -> Result<lazy::Value, ExecutionError> {
        let parameters = self
            .parameters
            .iter()
            .map(|e| e.evaluate_lazy(exec))
            .collect::<Result<_, _>>()?;
        Ok(lazy::Call::new(self.function, parameters).into())
    }
}

impl RegexCapture {
    fn evaluate_lazy(&self, exec: &mut ExecutionContext) -> Result<lazy::Value, ExecutionError> {
        let captures = match exec.variables.get(
            exec.regex_captures_identifier,
            &mut VariableContext {
                ctx: exec.ctx,
                graph: exec.graph,
                store: exec.store,
            },
        ) {
            Ok(captures) => Ok(captures),
            Err(_) => Err(ExecutionError::UndefinedRegexCapture(format!(
                "{}",
                self.display_with(exec.ctx)
            ))),
        }?;
        let value = lazy::ListIndex::new(captures.clone(), (self.match_index as u32).into());
        Ok(value.into())
    }
}

impl NotNull {
    fn evaluate_lazy(&self, exec: &mut ExecutionContext) -> Result<lazy::Value, ExecutionError> {
        let value = self.value.evaluate_lazy(exec)?;
        let result = lazy::NotNull::new(value);
        Ok(result.into())
    }
}

impl Variable {
    fn evaluate_lazy(&self, exec: &mut ExecutionContext) -> Result<lazy::Value, ExecutionError> {
        match self {
            Variable::Scoped(variable) => variable.evaluate_lazy(exec),
            Variable::Unscoped(variable) => variable.evaluate_lazy(exec).map(|v| v.clone()),
        }
    }

    fn define(
        &self,
        value: lazy::Value,
        mutability: Mutability,
        exec: &mut ExecutionContext,
    ) -> Result<(), ExecutionError> {
        match self {
            Variable::Scoped(variable) => variable.define(value, mutability, exec),
            Variable::Unscoped(variable) => variable.define(value, mutability, exec),
        }
    }

    fn assign(
        &self,
        value: lazy::Value,
        exec: &mut ExecutionContext,
    ) -> Result<(), ExecutionError> {
        match self {
            Variable::Scoped(_) => Err(ExecutionError::CannotAssignScopedVariable(format!(
                "{}",
                self.display_with(exec.ctx)
            ))),
            Variable::Unscoped(variable) => {
                let value = exec
                    .store
                    .add(value, variable.location.into(), exec.ctx, exec.graph);
                variable.assign(value.into(), exec)
            }
        }
    }
}

impl ScopedVariable {
    fn evaluate_lazy(&self, exec: &mut ExecutionContext) -> Result<lazy::Value, ExecutionError> {
        let scope = self.scope.evaluate_lazy(exec)?;
        let value = lazy::ScopedVariable::new(scope, self.name);
        Ok(value.into())
    }

    fn define(
        &self,
        value: lazy::Value,
        mutability: Mutability,
        exec: &mut ExecutionContext,
    ) -> Result<(), ExecutionError> {
        if let Mutability::Mutable = mutability {
            return Err(ExecutionError::CannotDefineMutableScopedVariable(format!(
                "{}",
                self.display_with(exec.ctx)
            )));
        }
        let scope = self.scope.evaluate_lazy(exec)?;
        let variable = exec
            .store
            .add(value, self.location.into(), exec.ctx, exec.graph);
        exec.scoped_store.add(
            scope,
            self.name,
            variable.into(),
            self.location.into(),
            exec.ctx,
        )
    }
}

impl UnscopedVariable {
    fn evaluate_lazy<'a>(
        &self,
        exec: &'a mut ExecutionContext,
    ) -> Result<&'a lazy::Value, ExecutionError> {
        exec.variables.get(
            self.name,
            &mut VariableContext {
                ctx: exec.ctx,
                graph: exec.graph,
                store: exec.store,
            },
        )
    }

    fn define(
        &self,
        value: lazy::Value,
        mutability: Mutability,
        exec: &mut ExecutionContext,
    ) -> Result<(), ExecutionError> {
        let variable = exec
            .store
            .add(value, self.location.into(), exec.ctx, exec.graph);
        exec.variables.add(
            self.name,
            variable.into(),
            mutability,
            &mut VariableContext {
                ctx: exec.ctx,
                graph: exec.graph,
                store: exec.store,
            },
        )
    }

    fn assign(
        &self,
        value: lazy::Value,
        exec: &mut ExecutionContext,
    ) -> Result<(), ExecutionError> {
        exec.variables.set(
            self.name,
            value,
            &mut VariableContext {
                ctx: exec.ctx,
                graph: exec.graph,
                store: exec.store,
            },
        )
    }
}

impl Attribute {
    fn evaluate_lazy(
        &self,
        exec: &mut ExecutionContext,
    ) -> Result<lazy::Attribute, ExecutionError> {
        let value = self.value.evaluate_lazy(exec)?;
        let attribute = lazy::Attribute::new(self.name, value);
        Ok(attribute)
    }
}
