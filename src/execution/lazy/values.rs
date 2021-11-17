// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

//! Defines lazy expression values and graph statements for DSL evaluation

use crate::execution::ExecutionError;
use crate::functions::Functions;
use crate::graph;
use crate::graph::DisplayWithGraph;
use crate::graph::Graph;
use crate::graph::GraphNodeRef;
use crate::graph::SyntaxNodeRef;
use crate::parser::Location;
use crate::Context;
use crate::DisplayWithContext;
use crate::Identifier;

use log::trace;

use regex::Regex;

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::convert::From;
use std::fmt;
use std::rc::Rc;

/**********************************************************************
 * Context
 **********************************************************************/

pub struct EvaluationContext<'a, 'tree> {
    pub ctx: &'a Context,
    pub source: &'tree str,
    pub graph: &'a mut Graph<'tree>,
    pub functions: &'a mut Functions,
    pub store: &'a Store,
    pub scoped_store: &'a ScopedVariables,
    pub function_arguments: &'a mut Vec<graph::Value>, // re-usable buffer to reduce memory allocations
    pub iteration: Iteration,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Iteration(Vec<usize>);

impl Iteration {
    pub fn new() -> Self {
        Self(Vec::default())
    }

    pub fn depth(&self) -> usize {
        self.0.len()
    }

    pub fn current_index(&self) -> Option<usize> {
        trace!("current_index {}", self);
        let index = *self.0.last().unwrap();
        if index > 0 {
            trace!("current_index {} = {}", self, index - 1);
            Some(index - 1)
        } else {
            trace!("current_index {} = none", self);
            None
        }
    }

    pub fn enter(&self, iterations: usize) -> Self {
        trace!("enter {} {}", iterations, self);
        let mut it = self.0.clone();
        it.push(iterations);
        Self(it)
    }

    pub fn prev(&self) -> Self {
        trace!("prev {}", self);
        let mut it = self.0.clone();
        let index = it.last_mut().unwrap();
        *index -= 1;
        Self(it)
    }

    pub fn exit(&self) -> Self {
        trace!("exit {}", self);
        let mut it = self.0.clone();
        it.pop();
        Self(it)
    }
}

impl fmt::Display for Iteration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<")?;
        let mut first = true;
        for i in &self.0 {
            if first {
                first = false;
                write!(f, "{}", i)?;
            } else {
                write!(f, ":{}", i)?;
            }
        }
        write!(f, ">")
    }
}

/**********************************************************************
 * Store and thunks
 **********************************************************************/

#[derive(Default)]
pub struct Store {
    elements: Vec<Thunk>,
}

impl Store {
    pub fn new() -> Store {
        Store {
            elements: Vec::new(),
        }
    }

    pub fn add(
        &mut self,
        value: Value,
        debug_info: DebugInfo,
        ctx: &Context,
        graph: &Graph,
    ) -> Variable {
        let store_location = self.elements.len();
        let variable = Variable::new(store_location);
        trace!(
            "store {} = {}",
            store_location,
            value.display_with(ctx, graph)
        );
        self.elements.push(Thunk::new(value, debug_info));
        variable
    }

    pub fn set(
        &mut self,
        variable: &Variable,
        value: Value,
        debug_info: DebugInfo,
        ctx: &Context,
        graph: &Graph,
    ) {
        let store_location = variable.store_location;
        trace!(
            "store {} = {}",
            store_location,
            value.display_with(ctx, graph)
        );
        self.elements[store_location].set(value, debug_info);
    }

    fn evaluate(
        &self,
        variable: &Variable,
        exec: &mut EvaluationContext,
    ) -> Result<graph::Value, ExecutionError> {
        self.elements[variable.store_location].evaluate(exec)
    }
}

// ScopedVariables

pub struct ScopedVariables {
    variables: HashMap<Identifier, Rc<RefCell<ScopedValues>>>,
}

impl ScopedVariables {
    pub fn new() -> ScopedVariables {
        ScopedVariables {
            variables: HashMap::new(),
        }
    }

    pub fn add(
        &mut self,
        scope: Value,
        name: Identifier,
        value: Value,
        debug_info: DebugInfo,
        ctx: &Context,
    ) -> Result<(), ExecutionError> {
        let values = self
            .variables
            .entry(name)
            .or_insert_with(|| Rc::new(RefCell::new(ScopedValues::new())));
        match values.replace(ScopedValues::Forcing) {
            ScopedValues::Unforced(mut pairs) => {
                pairs.push((scope, value, debug_info));
                values.replace(ScopedValues::Unforced(pairs));
                Ok(())
            }
            ScopedValues::Forcing => Err(ExecutionError::RecursivelyDefinedScopedVariable(
                format!("{}", name.display_with(ctx)),
            )),
            ScopedValues::Forced(map) => {
                values.replace(ScopedValues::Forced(map));
                Err(ExecutionError::VariableScopesAlreadyForced(format!(
                    "{}",
                    name.display_with(ctx)
                )))
            }
        }
    }

    fn resolve(
        &self,
        scope: SyntaxNodeRef,
        name: Identifier,
        exec: &mut EvaluationContext,
    ) -> Result<Value, ExecutionError> {
        let values = match self.variables.get(&name) {
            Some(v) => v,
            None => {
                return Err(ExecutionError::UndefinedScopedVariable(format!(
                    "{}.{}",
                    scope.display_with(exec.graph),
                    name.display_with(exec.ctx),
                )));
            }
        };
        match values.replace(ScopedValues::Forcing) {
            ScopedValues::Unforced(pairs) => {
                let mut map = HashMap::new();
                let mut debug_infos = HashMap::new();
                for (scope, value, debug_info) in pairs.into_iter() {
                    let node = scope.evaluate_as_syntax_node(exec)?;
                    let prev_debug_info = debug_infos.insert(node, debug_info.clone());
                    match map.insert(node, value.clone()) {
                        Some(_) => {
                            return Err(ExecutionError::DuplicateVariable(format!(
                                "{}.{} set at {} and {}",
                                node.display_with(exec.graph),
                                name.display_with(exec.ctx),
                                prev_debug_info.unwrap(),
                                debug_info,
                            )));
                        }
                        _ => {}
                    };
                }
                let result = map
                    .get(&scope)
                    .ok_or(ExecutionError::UndefinedScopedVariable(format!(
                        "{}.{}",
                        scope.display_with(exec.graph),
                        name.display_with(exec.ctx),
                    )))?
                    .clone();
                values.replace(ScopedValues::Forced(map));
                Ok(result)
            }
            ScopedValues::Forcing => {
                Err(ExecutionError::RecursivelyDefinedScopedVariable(format!(
                    "_.{} requested on {}",
                    name.display_with(exec.ctx),
                    scope.display_with(exec.graph)
                )))
            }
            ScopedValues::Forced(map) => {
                let result = map
                    .get(&scope)
                    .ok_or(ExecutionError::UndefinedScopedVariable(format!(
                        "{}.{}",
                        scope.display_with(exec.graph),
                        name.display_with(exec.ctx),
                    )))?
                    .clone();
                values.replace(ScopedValues::Forced(map));
                Ok(result)
            }
        }
    }
}

enum ScopedValues {
    Unforced(Vec<(Value, Value, DebugInfo)>),
    Forcing,
    Forced(HashMap<SyntaxNodeRef, Value>),
}

impl ScopedValues {
    fn new() -> ScopedValues {
        ScopedValues::Unforced(Vec::new())
    }
}

// Thunk (internal to the Store)

struct Thunk {
    value: Value,
    forced_values: Rc<RefCell<BTreeMap<Iteration, ThunkState>>>,
    debug_info: DebugInfo,
}

enum ThunkState {
    Forcing,
    Forced(graph::Value),
}

impl Thunk {
    fn new(value: Value, debug_info: DebugInfo) -> Thunk {
        Thunk {
            value,
            forced_values: Rc::new(RefCell::new(BTreeMap::new())),
            debug_info,
        }
    }

    fn set(&mut self, value: Value, debug_info: DebugInfo) {
        if !self.forced_values.borrow().is_empty() {
            panic!("Thunk set after already being forced.");
        }
        self.value = value;
        self.debug_info = debug_info;
    }

    fn evaluate(&self, exec: &mut EvaluationContext) -> Result<graph::Value, ExecutionError> {
        if self.forced_values.borrow().contains_key(&exec.iteration) {
            match self.forced_values.borrow().get(&exec.iteration).unwrap() {
                ThunkState::Forced(value) => Ok(value.clone()),
                ThunkState::Forcing => Err(ExecutionError::RecursivelyDefinedVariable(format!(
                    "{}",
                    self.debug_info
                ))),
            }
        } else {
            self.forced_values
                .borrow_mut()
                .insert(exec.iteration.clone(), ThunkState::Forcing);
            // it is important that we do not hold a borrow of self.forced_values when executing self.value.evaluate
            let value = self.value.evaluate(exec)?;
            self.forced_values
                .borrow_mut()
                .insert(exec.iteration.clone(), ThunkState::Forced(value.clone()));
            Ok(value)
        }
    }
}

/**********************************************************************
 * Values
 **********************************************************************/

// Value

#[derive(Clone, Debug)]
pub enum Value {
    Value(graph::Value),
    List(List),
    Set(Set),
    Variable(Variable),
    ScopedVariable(ScopedVariable),
    Call(Call),
    Scan(Scan),
    ListIndex(ListIndex),
    Branch(BranchValue),
    CurrentLoopElement(CurrentLoopListElementValue),
    EnterLoop(EnterLoopValue),
    PreviousLoop(PreviousLoopValue),
    Loop(LoopValue),
    NotNull(NotNull),
    AssertLoopDepth(AssertLoopDepth),
}

impl From<graph::Value> for Value {
    fn from(value: graph::Value) -> Self {
        Value::Value(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Value(value.into())
    }
}

impl From<u32> for Value {
    fn from(value: u32) -> Self {
        Value::Value(value.into())
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value::Value(value.into())
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::Value(value.into())
    }
}

impl From<GraphNodeRef> for Value {
    fn from(value: GraphNodeRef) -> Self {
        Value::Value(value.into())
    }
}

impl From<SyntaxNodeRef> for Value {
    fn from(value: SyntaxNodeRef) -> Self {
        Value::Value(value.into())
    }
}

impl From<Vec<graph::Value>> for Value {
    fn from(value: Vec<graph::Value>) -> Self {
        Value::Value(value.into())
    }
}

impl From<List> for Value {
    fn from(value: List) -> Self {
        Value::List(value)
    }
}

impl From<Vec<Value>> for Value {
    fn from(value: Vec<Value>) -> Self {
        Value::List(List::new(value))
    }
}

impl From<Set> for Value {
    fn from(value: Set) -> Self {
        Value::Set(value)
    }
}

impl From<Variable> for Value {
    fn from(value: Variable) -> Self {
        Value::Variable(value)
    }
}

impl From<ScopedVariable> for Value {
    fn from(value: ScopedVariable) -> Self {
        Value::ScopedVariable(value)
    }
}

impl From<Call> for Value {
    fn from(value: Call) -> Self {
        Value::Call(value)
    }
}

impl From<Scan> for Value {
    fn from(value: Scan) -> Self {
        Value::Scan(value)
    }
}

impl From<ListIndex> for Value {
    fn from(value: ListIndex) -> Self {
        Value::ListIndex(value)
    }
}

impl From<BranchValue> for Value {
    fn from(value: BranchValue) -> Self {
        Value::Branch(value)
    }
}

impl From<CurrentLoopListElementValue> for Value {
    fn from(value: CurrentLoopListElementValue) -> Self {
        Value::CurrentLoopElement(value)
    }
}

impl From<EnterLoopValue> for Value {
    fn from(value: EnterLoopValue) -> Self {
        Value::EnterLoop(value)
    }
}

impl From<PreviousLoopValue> for Value {
    fn from(value: PreviousLoopValue) -> Self {
        Value::PreviousLoop(value)
    }
}

impl From<LoopValue> for Value {
    fn from(value: LoopValue) -> Self {
        Value::Loop(value)
    }
}

impl From<NotNull> for Value {
    fn from(value: NotNull) -> Self {
        Value::NotNull(value)
    }
}

impl From<AssertLoopDepth> for Value {
    fn from(value: AssertLoopDepth) -> Self {
        Value::AssertLoopDepth(value)
    }
}

impl Value {
    pub fn evaluate(&self, exec: &mut EvaluationContext) -> Result<graph::Value, ExecutionError> {
        trace!("eval {} {{", self.display_with(exec.ctx, exec.graph));
        let ret = match self {
            Value::Value(value) => Ok(value.clone()),
            Value::List(expr) => expr.evaluate(exec),
            Value::Set(expr) => expr.evaluate(exec),
            Value::Variable(index) => exec.store.evaluate(index, exec),
            Value::ScopedVariable(expr) => expr.evaluate(exec),
            Value::Call(expr) => expr.evaluate(exec),
            Value::Scan(expr) => expr.evaluate(exec),
            Value::ListIndex(expr) => expr.evaluate(exec),
            Value::Branch(expr) => expr.evaluate(exec),
            Value::CurrentLoopElement(expr) => expr.evaluate(exec),
            Value::EnterLoop(expr) => expr.evaluate(exec),
            Value::PreviousLoop(expr) => expr.evaluate(exec),
            Value::Loop(expr) => expr.evaluate(exec),
            Value::NotNull(expr) => expr.evaluate(exec),
            Value::AssertLoopDepth(assert) => assert.evaluate(exec),
        }?;
        trace!("}} = {}", ret.display_with(exec.graph));
        Ok(ret)
    }

    pub fn evaluate_as_graph_node(
        &self,
        exec: &mut EvaluationContext,
    ) -> Result<GraphNodeRef, ExecutionError> {
        let node = self.evaluate(exec)?;
        match node {
            graph::Value::GraphNode(node) => Ok(node),
            _ => Err(ExecutionError::ExpectedGraphNode(format!(
                " got {}",
                node.display_with(exec.graph)
            ))),
        }
    }

    pub fn evaluate_as_syntax_node(
        &self,
        exec: &mut EvaluationContext,
    ) -> Result<SyntaxNodeRef, ExecutionError> {
        let node = self.evaluate(exec)?;
        match node {
            graph::Value::SyntaxNode(node) => Ok(node),
            _ => Err(ExecutionError::ExpectedSyntaxNode(format!(
                " got {}",
                node.display_with(exec.graph)
            ))),
        }
    }
}

impl DisplayWithContextAndGraph for Value {
    fn fmt<'tree>(
        &self,
        f: &mut fmt::Formatter,
        ctx: &Context,
        graph: &Graph<'tree>,
    ) -> fmt::Result {
        match self {
            Value::Value(value) => write!(f, "{}", value.display_with(graph)),
            Value::List(expr) => expr.fmt(f, ctx, graph),
            Value::Set(expr) => expr.fmt(f, ctx, graph),
            Value::Variable(expr) => expr.fmt(f, ctx, graph),
            Value::ScopedVariable(expr) => expr.fmt(f, ctx, graph),
            Value::Call(expr) => expr.fmt(f, ctx, graph),
            Value::Scan(expr) => expr.fmt(f, ctx, graph),
            Value::ListIndex(expr) => expr.fmt(f, ctx, graph),
            Value::Branch(expr) => expr.fmt(f, ctx, graph),
            Value::CurrentLoopElement(expr) => expr.fmt(f, ctx, graph),
            Value::EnterLoop(expr) => expr.fmt(f, ctx, graph),
            Value::PreviousLoop(expr) => expr.fmt(f, ctx, graph),
            Value::Loop(expr) => expr.fmt(f, ctx, graph),
            Value::NotNull(expr) => expr.fmt(f, ctx, graph),
            Value::AssertLoopDepth(assert) => assert.fmt(f, ctx, graph),
        }
    }
}

// Variable

#[derive(Clone, Debug)]
pub struct Variable {
    store_location: usize,
}

impl Variable {
    fn new(store_location: usize) -> Variable {
        Variable { store_location }
    }

    pub fn evaluate(&self, exec: &mut EvaluationContext) -> Result<graph::Value, ExecutionError> {
        exec.store.evaluate(self, exec)
    }
}

impl DisplayWithContextAndGraph for Variable {
    fn fmt(&self, f: &mut fmt::Formatter, _ctx: &Context, _graph: &Graph) -> fmt::Result {
        write!(f, "(load {})", self.store_location)
    }
}

// ScopedVariable

#[derive(Clone, Debug)]
pub struct ScopedVariable {
    scope: Box<Value>,
    name: Identifier,
}

impl ScopedVariable {
    pub fn new(scope: Value, name: Identifier) -> ScopedVariable {
        ScopedVariable {
            scope: scope.into(),
            name,
        }
    }

    fn resolve<'a>(&self, exec: &'a mut EvaluationContext) -> Result<Value, ExecutionError> {
        let scope = self.scope.as_ref().evaluate_as_syntax_node(exec)?;
        let scoped_store = &exec.scoped_store;
        scoped_store.resolve(scope, self.name, exec)
    }

    pub fn evaluate(&self, exec: &mut EvaluationContext) -> Result<graph::Value, ExecutionError> {
        let value = self.resolve(exec)?;
        value.evaluate(exec)
    }

    pub fn to_string(&self, ctx: &Context) -> String {
        format!("{}.{}", "?", self.name.display_with(ctx))
    }
}

impl DisplayWithContextAndGraph for ScopedVariable {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(
            f,
            "(scoped {} '{})",
            self.scope.display_with(ctx, graph),
            self.name.display_with(ctx),
        )
    }
}

// List

#[derive(Clone, Debug)]
pub struct List {
    elements: Vec<Value>,
}

impl List {
    pub fn new(elements: Vec<Value>) -> List {
        List { elements }
    }

    pub fn evaluate(&self, exec: &mut EvaluationContext) -> Result<graph::Value, ExecutionError> {
        let elements = self
            .elements
            .iter()
            .map(|e| e.evaluate(exec))
            .collect::<Result<_, _>>()?;
        Ok(graph::Value::List(elements))
    }
}

impl DisplayWithContextAndGraph for List {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(f, "(list")?;
        let mut first = true;
        for elem in &self.elements {
            if first {
                first = false;
                write!(f, "{}", elem.display_with(ctx, graph))?;
            } else {
                write!(f, " {}", elem.display_with(ctx, graph))?;
            }
        }
        write!(f, ")")
    }
}

// Set

#[derive(Clone, Debug)]
pub struct Set {
    elements: Vec<Value>,
}

impl Set {
    pub fn new(elements: Vec<Value>) -> Set {
        Set { elements }
    }

    pub fn evaluate(&self, exec: &mut EvaluationContext) -> Result<graph::Value, ExecutionError> {
        let elements = self
            .elements
            .iter()
            .map(|e| e.evaluate(exec))
            .collect::<Result<_, _>>()?;
        Ok(graph::Value::Set(elements))
    }
}

impl DisplayWithContextAndGraph for Set {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(f, "(set")?;
        let mut first = true;
        for elem in &self.elements {
            if first {
                first = false;
                write!(f, "{}", elem.display_with(ctx, graph))?;
            } else {
                write!(f, " {}", elem.display_with(ctx, graph))?;
            }
        }
        write!(f, ")")
    }
}

// Call

#[derive(Clone, Debug)]
pub struct Call {
    function: Identifier,
    arguments: Vec<Value>,
}

impl Call {
    pub fn new(function: Identifier, arguments: Vec<Value>) -> Call {
        Call {
            function,
            arguments,
        }
    }

    pub fn evaluate(&self, exec: &mut EvaluationContext) -> Result<graph::Value, ExecutionError> {
        for argument in &self.arguments {
            let argument = argument.evaluate(exec)?;
            exec.function_arguments.push(argument);
        }

        exec.functions.call(
            exec.ctx,
            self.function,
            exec.graph,
            exec.source,
            &mut exec
                .function_arguments
                .drain(exec.function_arguments.len() - self.arguments.len()..),
        )
    }
}

impl DisplayWithContextAndGraph for Call {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(f, "(call '{}", self.function.display_with(ctx))?;
        for arg in &self.arguments {
            write!(f, " {}", arg.display_with(ctx, graph))?;
        }
        write!(f, ")")
    }
}

// Scan

#[derive(Clone, Debug)]
pub struct Scan {
    value: Box<Value>,
    regexes: Vec<Regex>,
}

impl Scan {
    pub fn new(value: Value, regexes: Vec<Regex>) -> Self {
        Self {
            value: value.into(),
            regexes,
        }
    }

    pub fn evaluate(&self, exec: &mut EvaluationContext) -> Result<graph::Value, ExecutionError> {
        let match_string = self.value.evaluate(exec)?.into_string(exec.graph)?;

        let mut match_values = Vec::new();

        let mut i = 0;
        let mut candidate_matches = Vec::new();
        while i < match_string.len() {
            candidate_matches.clear();
            for (index, regex) in self.regexes.iter().enumerate() {
                if let Some(captures) = regex.captures(&match_string[i..]) {
                    if captures.get(0).unwrap().range().is_empty() {
                        return Err(ExecutionError::EmptyRegexCapture(format!(
                            "for regular expression /{}/",
                            regex
                        )));
                    }
                    candidate_matches.push((captures, index));
                }
            }

            if candidate_matches.is_empty() {
                break;
            }

            candidate_matches.sort_by_key(|(captures, index)| {
                let range = captures.get(0).unwrap().range();
                (range.start, *index)
            });
            let (captures, index) = &candidate_matches[0];

            let index_value = (*index as u32).into();
            let capture_values = captures
                .iter()
                .map(|c| {
                    let c = c.map(|c| c.as_str()).unwrap_or("");
                    c.into()
                })
                .collect::<Vec<_>>();
            let captures_value = capture_values.into();

            let match_value = vec![index_value, captures_value].into();
            match_values.push(match_value);

            i += captures.get(0).unwrap().range().end;
        }

        Ok(match_values.into())
    }
}

impl DisplayWithContextAndGraph for Scan {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(f, "(scan {} (", self.value.display_with(ctx, graph))?;
        let mut first = true;
        for regex in &self.regexes {
            if first {
                first = false;
                write!(f, "{:?}", regex.as_str())?;
            } else {
                write!(f, " {:?}", regex.as_str())?;
            }
        }
        write!(f, "))")
    }
}

// ListIndex

#[derive(Clone, Debug)]
pub struct ListIndex {
    values: Box<Value>,
    index: Box<Value>,
}

impl ListIndex {
    pub fn new(values: Value, index: Value) -> ListIndex {
        ListIndex {
            values: values.into(),
            index: index.into(),
        }
    }

    pub fn evaluate(&self, exec: &mut EvaluationContext) -> Result<graph::Value, ExecutionError> {
        let values = self.values.evaluate(exec)?;
        let values = match values {
            graph::Value::List(elements) => Ok(elements),
            _ => Err(ExecutionError::ExpectedList(format!(
                "got {} in {}",
                values.display_with(exec.graph),
                self.display_with(exec.ctx, exec.graph),
            ))),
        }?;
        let index = self.index.evaluate(exec)?.into_integer(exec.graph)?;
        Ok(values[index as usize].clone())
    }
}

impl DisplayWithContextAndGraph for ListIndex {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(
            f,
            "(index {} {})",
            self.index.display_with(ctx, graph),
            self.values.display_with(ctx, graph),
        )
    }
}

// Branch

#[derive(Clone, Debug)]
pub struct BranchValue {
    initial_value: Box<Value>,
    branch_index: Box<Value>,
    branches: BTreeMap<usize, Value>,
}

impl BranchValue {
    pub fn new(
        initial_value: Value,
        branch_index: Value,
        branches: BTreeMap<usize, Value>,
    ) -> BranchValue {
        BranchValue {
            initial_value: initial_value.into(),
            branch_index: branch_index.into(),
            branches: branches,
        }
    }

    pub fn evaluate(&self, exec: &mut EvaluationContext) -> Result<graph::Value, ExecutionError> {
        let branch_index = self.branch_index.evaluate(exec)?;
        let value = match branch_index {
            graph::Value::Null => &self.initial_value,
            graph::Value::Integer(branch_index) => self
                .branches
                .get(&(branch_index as usize))
                .unwrap_or(self.initial_value.as_ref()),
            _ => {
                return Err(ExecutionError::ExpectedInteger(format!(
                    "for branch index, got {}",
                    branch_index.display_with(exec.graph)
                )));
            }
        };
        value.evaluate(exec)
    }
}

impl DisplayWithContextAndGraph for BranchValue {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(
            f,
            "(branch {} (",
            self.branch_index.display_with(ctx, graph),
        )?;
        let mut first = true;
        for (key, value) in self.branches.iter() {
            if first {
                first = false;
                write!(f, "({} {})", key, value.display_with(ctx, graph))?;
            } else {
                write!(f, " ({} {})", key, value.display_with(ctx, graph))?;
            }
        }
        write!(f, "))")
    }
}

// Loop

/// get value of loop variable for current iteration from iteration_values
#[derive(Clone, Debug)]
pub struct CurrentLoopListElementValue {
    /// values iterated over (scoped outside the loop)
    iteration_values: Box<Value>,
}

impl CurrentLoopListElementValue {
    pub fn new(iteration_values: Value) -> Self {
        CurrentLoopListElementValue {
            iteration_values: iteration_values.into(),
        }
    }

    pub fn evaluate(&self, exec: &mut EvaluationContext) -> Result<graph::Value, ExecutionError> {
        let iteration = exec.iteration.exit();
        let iteration_values = self
            .iteration_values
            .evaluate(&mut EvaluationContext {
                ctx: exec.ctx,
                source: exec.source,
                graph: exec.graph,
                functions: exec.functions,
                store: exec.store,
                scoped_store: exec.scoped_store,
                function_arguments: exec.function_arguments,
                iteration,
            })?
            .into_list(exec.graph)?;
        let iteration_index = exec.iteration.current_index().unwrap();
        Ok(iteration_values[iteration_index].clone())
    }
}

impl DisplayWithContextAndGraph for CurrentLoopListElementValue {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(
            f,
            "(loop-index {})",
            self.iteration_values.display_with(ctx, graph)
        )
    }
}

/// get value after final iteration over the given iteration_values
#[derive(Clone, Debug)]
pub struct EnterLoopValue {
    /// values iterated over (scoped outside the loop)
    iteration_values: Box<Value>,
    /// value (scoped inside the loop)
    value: Box<Value>,
}

impl EnterLoopValue {
    pub fn new(iteration_values: Value, value: Value) -> Self {
        EnterLoopValue {
            iteration_values: iteration_values.into(),
            value: value.into(),
        }
    }

    pub fn evaluate(&self, exec: &mut EvaluationContext) -> Result<graph::Value, ExecutionError> {
        let number_of_iterations = self
            .iteration_values
            .evaluate(exec)?
            .into_list(exec.graph)?
            .len();
        let iteration = exec.iteration.enter(number_of_iterations);
        self.value.evaluate(&mut EvaluationContext {
            ctx: exec.ctx,
            source: exec.source,
            graph: exec.graph,
            functions: exec.functions,
            store: exec.store,
            scoped_store: exec.scoped_store,
            function_arguments: exec.function_arguments,
            iteration,
        })
    }
}

impl DisplayWithContextAndGraph for EnterLoopValue {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(
            f,
            "(loop-enter {} {})",
            self.iteration_values.display_with(ctx, graph),
            self.value.display_with(ctx, graph),
        )
    }
}

/// get value for previous iteration
#[derive(Clone, Debug)]
pub struct PreviousLoopValue {
    value: Box<Value>,
}

impl PreviousLoopValue {
    pub fn new(value: Value) -> Self {
        Self {
            value: value.into(),
        }
    }

    pub fn evaluate(&self, exec: &mut EvaluationContext) -> Result<graph::Value, ExecutionError> {
        let iteration = exec.iteration.prev();
        self.value.evaluate(&mut EvaluationContext {
            ctx: exec.ctx,
            source: exec.source,
            graph: exec.graph,
            functions: exec.functions,
            store: exec.store,
            scoped_store: exec.scoped_store,
            function_arguments: exec.function_arguments,
            iteration,
        })
    }
}

impl DisplayWithContextAndGraph for PreviousLoopValue {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(f, "(loop-prev {})", self.value.display_with(ctx, graph),)
    }
}

/// get most recent or initial value
#[derive(Clone, Debug)]
pub struct LoopValue {
    /// initial value before first iteration (scoped outside loop)
    initial_value: Box<Value>,
    /// value at the end  of an iteration (scoped inside loop)
    value: Box<Value>,
}

impl LoopValue {
    pub fn new(initial_value: Value, value: Value) -> Self {
        Self {
            initial_value: initial_value.into(),
            value: value.into(),
        }
    }

    pub fn evaluate(&self, exec: &mut EvaluationContext) -> Result<graph::Value, ExecutionError> {
        if exec.iteration.current_index().is_some() {
            self.value.evaluate(exec)
        } else {
            let iteration = exec.iteration.exit();
            self.initial_value.evaluate(&mut EvaluationContext {
                ctx: exec.ctx,
                source: exec.source,
                graph: exec.graph,
                functions: exec.functions,
                store: exec.store,
                scoped_store: exec.scoped_store,
                function_arguments: exec.function_arguments,
                iteration,
            })
        }
    }
}

impl DisplayWithContextAndGraph for LoopValue {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(
            f,
            "(loop {} {})",
            self.initial_value.display_with(ctx, graph),
            self.value.display_with(ctx, graph),
        )
    }
}

/// Check existence of optional value
#[derive(Debug, Clone)]
pub struct NotNull {
    value: Box<Value>,
}

impl NotNull {
    pub fn new(value: Value) -> Self {
        Self {
            value: value.into(),
        }
    }

    pub fn evaluate(&self, exec: &mut EvaluationContext) -> Result<graph::Value, ExecutionError> {
        let value = self.value.evaluate(exec)?;
        let result = match value {
            graph::Value::Null => false,
            _ => true,
        };
        Ok(result.into())
    }
}

impl DisplayWithContextAndGraph for NotNull {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(f, "{}?", self.value.display_with(ctx, graph))
    }
}

/// Assert the value is evaluated at the correct loop depth
#[derive(Debug, Clone)]
pub struct AssertLoopDepth {
    depth: usize,
    value: Box<Value>,
}

impl AssertLoopDepth {
    pub fn new(depth: usize, value: Value) -> Self {
        Self {
            depth,
            value: value.into(),
        }
    }

    pub fn evaluate(&self, exec: &mut EvaluationContext) -> Result<graph::Value, ExecutionError> {
        if exec.iteration.depth() != self.depth {
            panic!(
                "depth is {} while expecting {} for {}",
                exec.iteration.depth(),
                self.depth,
                self.value.display_with(exec.ctx, exec.graph)
            );
        }
        self.value.evaluate(exec)
    }
}

impl DisplayWithContextAndGraph for AssertLoopDepth {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        self.value.fmt(f, ctx, graph)
    }
}

/**********************************************************************
 * Statements
 **********************************************************************/

// Statement

#[derive(Debug)]
pub enum Statement {
    AddGraphNodeAttribute(AddGraphNodeAttribute),
    CreateEdge(CreateEdge),
    AddEdgeAttribute(AddEdgeAttribute),
    Print(Print),
    Branch(BranchStatement),
    Loop(LoopStatement),
}

impl Statement {
    pub fn evaluate(&self, exec: &mut EvaluationContext) -> Result<(), ExecutionError> {
        trace!("exec {} {{", self.display_with(exec.ctx, exec.graph));
        let ret = match self {
            Statement::AddGraphNodeAttribute(stmt) => stmt.evaluate(exec),
            Statement::CreateEdge(stmt) => stmt.evaluate(exec),
            Statement::AddEdgeAttribute(stmt) => stmt.evaluate(exec),
            Statement::Print(stmt) => stmt.evaluate(exec),
            Statement::Branch(stmt) => stmt.evaluate(exec),
            Statement::Loop(stmt) => stmt.evaluate(exec),
        };
        trace!("}}");
        ret
    }
}

impl From<AddEdgeAttribute> for Statement {
    fn from(stmt: AddEdgeAttribute) -> Self {
        Statement::AddEdgeAttribute(stmt)
    }
}

impl From<AddGraphNodeAttribute> for Statement {
    fn from(stmt: AddGraphNodeAttribute) -> Self {
        Statement::AddGraphNodeAttribute(stmt)
    }
}

impl From<CreateEdge> for Statement {
    fn from(stmt: CreateEdge) -> Self {
        Statement::CreateEdge(stmt)
    }
}

impl From<Print> for Statement {
    fn from(stmt: Print) -> Self {
        Statement::Print(stmt)
    }
}

impl From<BranchStatement> for Statement {
    fn from(stmt: BranchStatement) -> Self {
        Statement::Branch(stmt)
    }
}

impl From<LoopStatement> for Statement {
    fn from(stmt: LoopStatement) -> Self {
        Statement::Loop(stmt)
    }
}

impl DisplayWithContextAndGraph for Statement {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        match self {
            Statement::AddGraphNodeAttribute(stmt) => stmt.fmt(f, ctx, graph),
            Statement::CreateEdge(stmt) => stmt.fmt(f, ctx, graph),
            Statement::AddEdgeAttribute(stmt) => stmt.fmt(f, ctx, graph),
            Statement::Print(stmt) => stmt.fmt(f, ctx, graph),
            Statement::Branch(stmt) => stmt.fmt(f, ctx, graph),
            Statement::Loop(stmt) => stmt.fmt(f, ctx, graph),
        }
    }
}

// AddGraphNodeAttribute

#[derive(Debug)]
pub struct AddGraphNodeAttribute {
    node: Value,
    attributes: Vec<Attribute>,
    debug_info: DebugInfo,
}

impl AddGraphNodeAttribute {
    pub fn new(
        node: Value,
        attributes: Vec<Attribute>,
        debug_info: DebugInfo,
    ) -> AddGraphNodeAttribute {
        AddGraphNodeAttribute {
            node,
            attributes,
            debug_info,
        }
    }

    pub fn evaluate(&self, exec: &mut EvaluationContext) -> Result<(), ExecutionError> {
        let node = self.node.evaluate_as_graph_node(exec)?;
        for attribute in &self.attributes {
            let value = attribute.value.evaluate(exec)?;
            exec.graph[node]
                .attributes
                .add(attribute.name, value)
                .map_err(|_| {
                    ExecutionError::DuplicateAttribute(format!(
                        "{} on {} at {}",
                        attribute.name.display_with(exec.ctx),
                        node.display_with(exec.graph),
                        self.debug_info,
                    ))
                })?;
        }
        Ok(())
    }
}

impl DisplayWithContextAndGraph for AddGraphNodeAttribute {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(f, "attr ({})", self.node.display_with(ctx, graph))?;
        for attr in &self.attributes {
            write!(f, " {}", attr.display_with(ctx, graph))?;
        }
        write!(f, " at {}", self.debug_info)
    }
}

// CreateEdge

#[derive(Debug)]
pub struct CreateEdge {
    source: Value,
    sink: Value,
    debug_info: DebugInfo,
}

impl CreateEdge {
    pub fn new(source: Value, sink: Value, debug_info: DebugInfo) -> CreateEdge {
        CreateEdge {
            source,
            sink,
            debug_info,
        }
    }

    pub fn evaluate(&self, exec: &mut EvaluationContext) -> Result<(), ExecutionError> {
        let source = self.source.evaluate_as_graph_node(exec)?;
        let sink = self.sink.evaluate_as_graph_node(exec)?;
        if let Err(_) = exec.graph[source].add_edge(sink) {
            Err(ExecutionError::DuplicateEdge(format!(
                "({} -> {}) at {}",
                source.display_with(exec.graph),
                sink.display_with(exec.graph),
                self.debug_info,
            )))?;
        }
        Ok(())
    }
}

impl DisplayWithContextAndGraph for CreateEdge {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(
            f,
            "edge {} -> {} at {}",
            self.source.display_with(ctx, graph),
            self.sink.display_with(ctx, graph),
            self.debug_info,
        )
    }
}

// AddEdgeAttribute

#[derive(Debug)]
pub struct AddEdgeAttribute {
    source: Value,
    sink: Value,
    attributes: Vec<Attribute>,
    debug_info: DebugInfo,
}

impl AddEdgeAttribute {
    pub fn new(
        source: Value,
        sink: Value,
        attributes: Vec<Attribute>,
        debug_info: DebugInfo,
    ) -> AddEdgeAttribute {
        AddEdgeAttribute {
            source,
            sink,
            attributes,
            debug_info,
        }
    }

    pub fn evaluate(&self, exec: &mut EvaluationContext) -> Result<(), ExecutionError> {
        let source = self.source.evaluate_as_graph_node(exec)?;
        let sink = self.sink.evaluate_as_graph_node(exec)?;
        for attribute in &self.attributes {
            let value = attribute.value.evaluate(exec)?;
            let edge = match exec.graph[source].get_edge_mut(sink) {
                Some(edge) => Ok(edge),
                None => Err(ExecutionError::UndefinedEdge(format!(
                    "({} -> {}) at {}",
                    source.display_with(exec.graph),
                    sink.display_with(exec.graph),
                    self.debug_info,
                ))),
            }?;
            edge.attributes.add(attribute.name, value).map_err(|_| {
                ExecutionError::DuplicateAttribute(format!(
                    "{} on edge ({} -> {}) at {}",
                    attribute.name.display_with(exec.ctx),
                    source.display_with(exec.graph),
                    sink.display_with(exec.graph),
                    self.debug_info,
                ))
            })?;
        }
        Ok(())
    }
}

impl DisplayWithContextAndGraph for AddEdgeAttribute {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(
            f,
            "attr ({} -> {})",
            self.source.display_with(ctx, graph),
            self.sink.display_with(ctx, graph),
        )?;
        for attr in &self.attributes {
            write!(f, " {}", attr.display_with(ctx, graph),)?;
        }
        write!(f, " at {}", self.debug_info)
    }
}

// Print

#[derive(Debug)]
pub struct Print {
    arguments: Vec<PrintArgument>,
    debug_info: DebugInfo,
}

#[derive(Debug)]
pub enum PrintArgument {
    Text(String),
    Value(Value),
}

impl Print {
    pub fn new(arguments: Vec<PrintArgument>, debug_info: DebugInfo) -> Print {
        Print {
            arguments,
            debug_info,
        }
    }

    pub fn evaluate(&self, exec: &mut EvaluationContext) -> Result<(), ExecutionError> {
        for argument in &self.arguments {
            match argument {
                PrintArgument::Text(string) => eprint!("{}", string),
                PrintArgument::Value(value) => {
                    let value = value.evaluate(exec)?;
                    eprint!("{}", value.display_with(exec.graph));
                }
            }
        }
        eprintln!("");
        Ok(())
    }
}

impl DisplayWithContextAndGraph for Print {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(f, "print")?;
        let mut first = true;
        for argument in &self.arguments {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            match argument {
                PrintArgument::Text(string) => write!(f, "\"{}\"", string)?,
                PrintArgument::Value(value) => write!(f, "{}", value.display_with(ctx, graph))?,
            };
        }
        write!(f, " at {}", self.debug_info)
    }
}

// Branch

#[derive(Debug)]
pub struct BranchStatement {
    branch_index: Value,
    branches: Vec<Vec<Statement>>,
    debug_info: DebugInfo,
}

impl BranchStatement {
    pub fn new(branch_index: Value, branches: Vec<Vec<Statement>>, debug_info: DebugInfo) -> Self {
        Self {
            branch_index,
            branches,
            debug_info,
        }
    }

    pub fn evaluate(&self, exec: &mut EvaluationContext) -> Result<(), ExecutionError> {
        let branch_index = self.branch_index.evaluate(exec)?;
        match branch_index {
            graph::Value::Null => Ok(()),
            graph::Value::Integer(index) => {
                for stmt in &self.branches[index as usize] {
                    stmt.evaluate(exec)?;
                }
                Ok(())
            }
            _ => Err(ExecutionError::ExpectedInteger(format!(
                " for index, got {} at {}",
                branch_index.display_with(exec.graph),
                self.debug_info,
            ))),
        }
    }
}

impl DisplayWithContextAndGraph for BranchStatement {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(
            f,
            "branch {} in {{",
            self.branch_index.display_with(ctx, graph)
        )?;
        for (index, _) in self.branches.iter().enumerate() {
            write!(f, " {}: ...", index)?;
        }
        write!(f, " }} at {}", self.debug_info)
    }
}

// Loop

#[derive(Debug)]
pub struct LoopStatement {
    iteration_values: Value,
    statements: Vec<Statement>,
    debug_info: DebugInfo,
}

impl LoopStatement {
    pub fn new(iteration_values: Value, statements: Vec<Statement>, debug_info: DebugInfo) -> Self {
        Self {
            iteration_values,
            statements,
            debug_info,
        }
    }

    pub fn evaluate(&self, exec: &mut EvaluationContext) -> Result<(), ExecutionError> {
        let iteration_values = self
            .iteration_values
            .evaluate(exec)?
            .into_list(exec.graph)?;
        trace!("loop values {:?}", iteration_values);
        let mut iteration = exec.iteration.enter(iteration_values.len());
        trace!("loop initial {}", iteration);
        while iteration.current_index().is_some() {
            trace!("loop iteration {}", iteration);
            for statement in &self.statements {
                statement.evaluate(&mut EvaluationContext {
                    ctx: exec.ctx,
                    source: exec.source,
                    graph: exec.graph,
                    functions: exec.functions,
                    store: exec.store,
                    scoped_store: exec.scoped_store,
                    function_arguments: exec.function_arguments,
                    iteration: iteration.clone(),
                })?;
            }
            iteration = iteration.prev();
        }

        Ok(())
    }
}

impl DisplayWithContextAndGraph for LoopStatement {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(
            f,
            "loop {} {{ ... }} at {}",
            self.iteration_values.display_with(ctx, graph),
            self.debug_info,
        )
    }
}

// Attribute

#[derive(Debug)]
pub struct Attribute {
    name: Identifier,
    value: Value,
}

impl Attribute {
    pub fn new(name: Identifier, value: Value) -> Attribute {
        Attribute { name, value }
    }
}

impl DisplayWithContextAndGraph for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(
            f,
            "{} = {}",
            self.name.display_with(ctx),
            self.value.display_with(ctx, graph),
        )
    }
}

// DebugInfo
#[derive(Debug, Clone, Copy)]
pub struct DebugInfo(Location);

impl DebugInfo {
    pub fn new(location: Location) -> DebugInfo {
        DebugInfo(location)
    }
}

impl From<Location> for DebugInfo {
    fn from(value: Location) -> Self {
        Self(value)
    }
}

impl fmt::Display for DebugInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Trait to Display with a given Context and Graph
pub trait DisplayWithContextAndGraph
where
    Self: Sized,
{
    fn fmt<'tree>(
        &self,
        f: &mut fmt::Formatter,
        ctx: &Context,
        graph: &Graph<'tree>,
    ) -> fmt::Result;

    fn display_with<'a, 'tree>(
        &'a self,
        ctx: &'a Context,
        graph: &'a Graph<'tree>,
    ) -> Box<dyn fmt::Display + 'a> {
        struct Impl<'a, 'tree, T: DisplayWithContextAndGraph>(&'a T, &'a Context, &'a Graph<'tree>);

        impl<'a, 'tree, T: DisplayWithContextAndGraph> fmt::Display for Impl<'a, 'tree, T> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.0.fmt(f, self.1, self.2)
            }
        }

        Box::new(Impl(self, ctx, graph))
    }
}

impl<T: DisplayWithContextAndGraph> DisplayWithContextAndGraph for Box<T> {
    fn fmt<'tree>(
        &self,
        f: &mut fmt::Formatter,
        ctx: &Context,
        graph: &Graph<'tree>,
    ) -> fmt::Result {
        self.as_ref().fmt(f, ctx, graph)
    }
}
