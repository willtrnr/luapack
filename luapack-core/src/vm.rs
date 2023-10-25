use crate::{
    ast::IntoValue,
    token::{IntoToken, TokenReferenceExt},
};
use full_moon::{
    ast,
    tokenizer::{Symbol, Token, TokenReference, TokenType},
    visitors::{Visitor, VisitorMut},
};
use std::{borrow::Borrow, collections::BTreeMap, fmt};

#[derive(PartialEq, Clone, Debug)]
pub enum RuntimeValue {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Table(Vec<(Option<Box<RuntimeValue>>, Box<RuntimeValue>)>),
}

impl RuntimeValue {
    pub fn from_value(value: &ast::Value) -> Option<Self> {
        match value {
            ast::Value::Number(v) => Some(Self::Number(v.as_number_value()?)),
            ast::Value::String(v) => Some(Self::String(v.as_string_value()?.to_string())),
            ast::Value::Symbol(v) => Self::from_token(v),
            _ => None,
        }
    }

    pub fn from_token<T: Borrow<Token>>(value: T) -> Option<Self> {
        match value.borrow().token_type() {
            TokenType::Number { text } => text.parse().ok().map(Self::Number),
            TokenType::StringLiteral { literal, .. } => Some(Self::String(literal.to_string())),
            TokenType::Symbol {
                symbol: Symbol::False,
            } => Some(Self::Boolean(false)),
            TokenType::Symbol {
                symbol: Symbol::True,
            } => Some(Self::Boolean(true)),
            TokenType::Symbol {
                symbol: Symbol::Nil,
            } => Some(Self::Nil),
            _ => None,
        }
    }

    pub fn is_false_value(&self) -> bool {
        matches!(self, Self::Nil | Self::Boolean(false))
    }

    #[inline]
    pub fn is_true_value(&self) -> bool {
        !self.is_false_value()
    }
}

impl From<RuntimeValue> for ast::Value {
    fn from(value: RuntimeValue) -> Self {
        match value {
            RuntimeValue::Nil => Symbol::Nil.into_value(),
            RuntimeValue::Boolean(b) => b.into_value(),
            RuntimeValue::Number(n) => n.into_value(),
            RuntimeValue::String(s) => s.into_value(),
            RuntimeValue::Table(t) => {
                let brackets = ast::span::ContainedSpan::new(
                    Symbol::LeftBracket.into_token_ref(),
                    Symbol::RightBracket.into_token_ref(),
                );
                let equal = Symbol::Equal.into_token_ref();
                let comma = Symbol::Comma.into_token_ref();
                Self::TableConstructor(
                    ast::TableConstructor::new().with_fields(
                        t.into_iter()
                            .map(|(k, v)| {
                                ast::punctuated::Pair::Punctuated(
                                    if let Some(k) = k {
                                        ast::Field::ExpressionKey {
                                            brackets: brackets.clone(),
                                            key: ast::Expression::from(*k),
                                            equal: equal.clone(),
                                            value: ast::Expression::from(*v),
                                        }
                                    } else {
                                        ast::Field::NoKey(ast::Expression::from(*v))
                                    },
                                    comma.clone(),
                                )
                            })
                            .collect(),
                    ),
                )
            }
        }
    }
}

impl From<RuntimeValue> for ast::Expression {
    fn from(value: RuntimeValue) -> Self {
        Self::Value {
            value: Box::new(ast::Value::from(value)),
            type_assertion: None,
        }
    }
}

impl fmt::Display for RuntimeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RuntimeValue::Nil => f.write_str("Nil"),
            RuntimeValue::Boolean(v) => v.fmt(f),
            RuntimeValue::Number(v) => v.fmt(f),
            RuntimeValue::String(v) => v.fmt(f),
            RuntimeValue::Table(t) => {
                f.write_str("{")?;
                for (i, (k, v)) in t.iter().enumerate() {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    if let Some(k) = k {
                        f.write_fmt(format_args!("[{}] = {}", k, v))?;
                    } else {
                        v.fmt(f)?;
                    }
                }
                f.write_str("}")
            }
        }
    }
}

#[derive(Clone, Default, Debug)]
pub struct Frame {
    locals: BTreeMap<String, Option<RuntimeValue>>,
}

impl Frame {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Clone, Default, Debug)]
pub struct VmVisitor {
    globals: BTreeMap<String, Option<RuntimeValue>>,
    stack: Vec<Frame>,
}

impl VmVisitor {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn is_local<S: AsRef<str>>(&self, name: S) -> bool {
        let name = name.as_ref();
        for f in self.stack.iter().rev() {
            if f.locals.contains_key(name) {
                return true;
            }
        }
        false
    }

    /// Primitive simple expression evaluation.
    pub fn eval<'a, 'b: 'a>(&'a self, expr: &'b ast::Expression) -> Option<RuntimeValue> {
        match expr {
            ast::Expression::BinaryOperator { lhs, binop, rhs } => {
                match (self.eval(lhs)?, binop, self.eval(rhs)?) {
                    (l, ast::BinOp::TwoEqual(_), r) => Some(RuntimeValue::Boolean(l == r)),
                    (l, ast::BinOp::TildeEqual(_), r) => Some(RuntimeValue::Boolean(l != r)),
                    (l, ast::BinOp::And(_), r) => {
                        if l.is_true_value() {
                            Some(r)
                        } else {
                            Some(l)
                        }
                    }
                    (l, ast::BinOp::Or(_), r) => {
                        if l.is_false_value() {
                            Some(r)
                        } else {
                            Some(l)
                        }
                    }
                    (l, ast::BinOp::TwoDots(_), RuntimeValue::String(r)) => {
                        // .. is right associative
                        Some(RuntimeValue::String(format!("{}{}", l, r)))
                    }
                    (RuntimeValue::Number(l), _, RuntimeValue::Number(r)) => match binop {
                        ast::BinOp::Minus(_) => Some(RuntimeValue::Number(l - r)),
                        ast::BinOp::Plus(_) => Some(RuntimeValue::Number(l + r)),
                        ast::BinOp::Slash(_) => Some(RuntimeValue::Number(l / r)),
                        ast::BinOp::Star(_) => Some(RuntimeValue::Number(l * r)),
                        ast::BinOp::DoubleSlash(_) => Some(RuntimeValue::Number((l / r).floor())),
                        ast::BinOp::GreaterThan(_) => Some(RuntimeValue::Boolean(l > r)),
                        ast::BinOp::GreaterThanEqual(_) => Some(RuntimeValue::Boolean(l >= r)),
                        ast::BinOp::LessThan(_) => Some(RuntimeValue::Boolean(l < r)),
                        ast::BinOp::LessThanEqual(_) => Some(RuntimeValue::Boolean(l <= r)),
                        _ => None,
                    },
                    (RuntimeValue::String(l), _, RuntimeValue::String(r)) => match binop {
                        ast::BinOp::GreaterThan(_) => Some(RuntimeValue::Boolean(l > r)),
                        ast::BinOp::GreaterThanEqual(_) => Some(RuntimeValue::Boolean(l >= r)),
                        ast::BinOp::LessThan(_) => Some(RuntimeValue::Boolean(l < r)),
                        ast::BinOp::LessThanEqual(_) => Some(RuntimeValue::Boolean(l <= r)),
                        _ => None,
                    },
                    _ => None,
                }
            }
            ast::Expression::UnaryOperator { unop, expression } => {
                match (unop, self.eval(expression)?) {
                    (ast::UnOp::Not(_), RuntimeValue::Nil | RuntimeValue::Boolean(false)) => {
                        Some(RuntimeValue::Boolean(true))
                    }
                    (ast::UnOp::Not(_), _) => Some(RuntimeValue::Boolean(false)),
                    (ast::UnOp::Hash(_), RuntimeValue::Table(v)) => {
                        Some(RuntimeValue::Number(v.len() as f64))
                    }
                    _ => None,
                }
            }
            ast::Expression::Parentheses { expression, .. } => self.eval(expression),
            ast::Expression::Value { value, .. } => match &**value {
                ast::Value::IfExpression(e) => {
                    if self.eval(e.condition())?.is_true_value() {
                        self.eval(e.if_expression())
                    } else {
                        if let Some(elif) = e.else_if_expressions() {
                            for e in elif {
                                if self.eval(e.condition())?.is_true_value() {
                                    return self.eval(e.expression());
                                }
                            }
                        }
                        self.eval(e.else_expression())
                    }
                }
                ast::Value::InterpolatedString(_) => todo!(),
                ast::Value::TableConstructor(c) => {
                    let mut fields = Vec::new();
                    for f in c.fields() {
                        match f {
                            ast::Field::ExpressionKey { key, value, .. } => fields.push((
                                Some(Box::new(self.eval(key)?)),
                                Box::new(self.eval(value)?),
                            )),
                            ast::Field::NameKey { key, value, .. } => fields.push((
                                Some(Box::new(RuntimeValue::String(
                                    key.as_identifier()?.to_string(),
                                ))),
                                Box::new(self.eval(value)?),
                            )),
                            ast::Field::NoKey(e) => fields.push((None, Box::new(self.eval(e)?))),
                            _ => return None,
                        }
                    }
                    Some(RuntimeValue::Table(fields))
                }
                ast::Value::ParenthesesExpression(e) => self.eval(e),
                ast::Value::Var(ast::Var::Name(n)) => {
                    let ident = n.as_identifier()?;
                    for f in self.stack.iter().rev() {
                        if let Some(v) = f.locals.get(ident) {
                            return v.clone();
                        }
                    }
                    if let Some(v) = self.globals.get(ident) {
                        return v.clone();
                    }
                    None
                }
                v => RuntimeValue::from_value(v),
            },
            _ => None,
        }
    }
}

impl VmVisitor {
    fn push_frame(&mut self) {
        self.stack.push(Frame::new());
    }

    fn pop_frame(&mut self) {
        let frame = self.stack.pop().expect("popping empty stack");
        log::debug!("Popped frame: {:?}", frame);
    }

    fn frame_mut(&mut self) -> &mut Frame {
        self.stack.last_mut().expect("empty stack")
    }

    fn add_local<S: Into<String>>(&mut self, name: S, value: Option<RuntimeValue>) {
        self.frame_mut().locals.insert(name.into(), value);
    }

    fn add_locals<S: Into<String>, I: IntoIterator<Item = (S, Option<RuntimeValue>)>>(
        &mut self,
        iter: I,
    ) {
        self.frame_mut()
            .locals
            .extend(iter.into_iter().map(|(k, v)| (k.into(), v)));
    }

    fn add_global<S: Into<String>>(&mut self, name: S, value: Option<RuntimeValue>) {
        self.globals.insert(name.into(), value);
    }
}

impl Visitor for VmVisitor {
    fn visit_block(&mut self, _node: &ast::Block) {
        self.push_frame();
    }

    fn visit_block_end(&mut self, _node: &ast::Block) {
        self.pop_frame();
    }

    fn visit_assignment(&mut self, node: &ast::Assignment) {
        let values = node
            .expressions()
            .iter()
            .map(|e| Some(self.eval(e)))
            .collect::<Vec<_>>();

        for (v, e) in node
            .variables()
            .iter()
            .zip(values.into_iter().chain(std::iter::repeat(None)))
        {
            if let ast::Var::Name(n) = v {
                if let TokenType::Identifier { identifier } = n.token_type() {
                    if !self.is_local(identifier.as_str()) {
                        self.add_global(identifier.as_str(), e.unwrap_or(Some(RuntimeValue::Nil)));
                    }
                }
            }
        }
    }

    fn visit_local_assignment(&mut self, node: &ast::LocalAssignment) {
        let values = node
            .expressions()
            .iter()
            .map(|e| Some(self.eval(e)))
            .collect::<Vec<_>>();

        self.add_locals(
            node.names()
                .iter()
                .zip(values.into_iter().chain(std::iter::repeat(None)))
                .filter_map(|(n, e)| match n.token_type() {
                    TokenType::Identifier { identifier } => {
                        Some((identifier.as_str(), e.unwrap_or(Some(RuntimeValue::Nil))))
                    }
                    _ => None,
                }),
        );
    }

    fn visit_function_declaration(&mut self, node: &ast::FunctionDeclaration) {
        let fname = node.name().names();
        if fname.len() == 1 {
            if let Some(identifier) = fname.first().unwrap().value().as_identifier() {
                if !self.is_local(identifier) {
                    self.add_global(identifier, None);
                }
            }
        }
    }

    fn visit_local_function(&mut self, node: &ast::LocalFunction) {
        if let TokenType::Identifier { identifier } = node.name().token_type() {
            self.add_local(identifier.as_str(), None);
        }
    }

    fn visit_function_body(&mut self, node: &ast::FunctionBody) {
        self.push_frame();
        self.add_locals(node.parameters().iter().filter_map(|p| match p {
            ast::Parameter::Name(n) => match n.token_type() {
                TokenType::Identifier { identifier } => Some((identifier.as_str(), None)),
                _ => None,
            },
            _ => None,
        }));
    }

    fn visit_function_body_end(&mut self, _node: &ast::FunctionBody) {
        self.pop_frame();
    }

    fn visit_generic_for(&mut self, node: &ast::GenericFor) {
        self.push_frame();
        self.add_locals(node.names().iter().filter_map(|r| match r.token_type() {
            TokenType::Identifier { identifier } => Some((identifier.as_str(), None)),
            _ => None,
        }));
    }

    fn visit_generic_for_end(&mut self, _node: &ast::GenericFor) {
        self.pop_frame();
    }

    fn visit_numeric_for(&mut self, node: &ast::NumericFor) {
        self.push_frame();
        if let TokenType::Identifier { identifier } = node.index_variable().token_type() {
            self.add_local(identifier.as_str(), None);
        }
    }

    fn visit_numeric_for_end(&mut self, _node: &ast::NumericFor) {
        self.pop_frame();
    }

    fn visit_eof(&mut self, _node: &TokenReference) {
        log::debug!("Globals used by EoF: {:?}", self.globals);
        self.stack = Vec::new();
    }
}

impl VisitorMut for VmVisitor {
    fn visit_block(&mut self, node: ast::Block) -> ast::Block {
        Visitor::visit_block(self, &node);
        node
    }

    fn visit_block_end(&mut self, node: ast::Block) -> ast::Block {
        Visitor::visit_block_end(self, &node);
        node
    }

    fn visit_assignment(&mut self, node: ast::Assignment) -> ast::Assignment {
        Visitor::visit_assignment(self, &node);
        node
    }

    fn visit_local_assignment(&mut self, node: ast::LocalAssignment) -> ast::LocalAssignment {
        Visitor::visit_local_assignment(self, &node);
        node
    }

    fn visit_function_declaration(
        &mut self,
        node: ast::FunctionDeclaration,
    ) -> ast::FunctionDeclaration {
        Visitor::visit_function_declaration(self, &node);
        node
    }

    fn visit_local_function(&mut self, node: ast::LocalFunction) -> ast::LocalFunction {
        Visitor::visit_local_function(self, &node);
        node
    }

    fn visit_function_body(&mut self, node: ast::FunctionBody) -> ast::FunctionBody {
        Visitor::visit_function_body(self, &node);
        node
    }

    fn visit_function_body_end(&mut self, node: ast::FunctionBody) -> ast::FunctionBody {
        Visitor::visit_function_body_end(self, &node);
        node
    }

    fn visit_generic_for(&mut self, node: ast::GenericFor) -> ast::GenericFor {
        Visitor::visit_generic_for(self, &node);
        node
    }

    fn visit_generic_for_end(&mut self, node: ast::GenericFor) -> ast::GenericFor {
        Visitor::visit_generic_for_end(self, &node);
        node
    }

    fn visit_numeric_for(&mut self, node: ast::NumericFor) -> ast::NumericFor {
        Visitor::visit_numeric_for(self, &node);
        node
    }

    fn visit_numeric_for_end(&mut self, node: ast::NumericFor) -> ast::NumericFor {
        Visitor::visit_numeric_for_end(self, &node);
        node
    }

    fn visit_eof(&mut self, node: TokenReference) -> TokenReference {
        Visitor::visit_eof(self, &node);
        node
    }
}
