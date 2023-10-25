use crate::{
    ast::{AstExt, ExpressionExt, IntoValue, PunctuatedExt, VarExpressionExt},
    error::Result,
    token::{IntoToken, TokenReferenceExt},
    vm::{RuntimeValue, VmVisitor},
};
use full_moon::{
    ast::{self, punctuated::Punctuated, span::ContainedSpan, Ast},
    tokenizer::{Symbol, TokenReference, TokenType},
    visitors::VisitorMut,
};
use std::{
    collections::{BTreeMap, BTreeSet},
    path::{Path, PathBuf},
};

const PRELUDE: &str = include_str!("prelude.lua");

pub trait Loader {
    fn load(&self) -> Result<Ast>;
}

impl<'a> Loader for &'a Path {
    #[inline]
    fn load(&self) -> Result<Ast> {
        Ok(full_moon::parse(&std::fs::read_to_string(self)?)?)
    }
}

impl Loader for PathBuf {
    #[inline]
    fn load(&self) -> Result<Ast> {
        self.as_path().load()
    }
}

impl<'a> Loader for &'a str {
    #[inline]
    fn load(&self) -> Result<Ast> {
        Ok(full_moon::parse(self)?)
    }
}

impl<T: Loader + ?Sized> Loader for Box<T> {
    fn load(&self) -> Result<Ast> {
        (**self).load()
    }
}

pub trait Searcher {
    fn search(&self, name: &str) -> Option<(Box<dyn Loader>, PathBuf)>;
}

pub struct SearchPaths {
    paths: Vec<PathBuf>,
}

impl<P: Into<PathBuf>> FromIterator<P> for SearchPaths {
    fn from_iter<T: IntoIterator<Item = P>>(iter: T) -> Self {
        Self {
            paths: iter.into_iter().map(Into::into).collect(),
        }
    }
}

impl Searcher for SearchPaths {
    fn search(&self, name: &str) -> Option<(Box<dyn Loader>, PathBuf)> {
        if name.is_empty() {
            return None;
        }

        let subpath = name.replace('.', std::path::MAIN_SEPARATOR_STR);
        for p in &self.paths {
            let mut candidate = PathBuf::with_capacity(p.capacity());
            for c in p.components() {
                let o = c.as_os_str();
                let s = o.to_string_lossy();
                if let Some((pre, post)) = s.split_once('?') {
                    candidate.push(format!("{}{}{}", pre, subpath, post));
                } else {
                    candidate.push(o);
                }
            }

            if candidate.is_file() {
                return Some((Box::new(candidate.clone()), candidate));
            }
        }
        None
    }
}

impl<S: Searcher + ?Sized> Searcher for Box<S> {
    #[inline]
    fn search(&self, name: &str) -> Option<(Box<dyn Loader>, PathBuf)> {
        (**self).search(name)
    }
}

impl<S: Searcher> Searcher for Vec<S> {
    fn search(&self, name: &str) -> Option<(Box<dyn Loader>, PathBuf)> {
        for s in self {
            if let Some(res) = s.search(name) {
                return Some(res);
            }
        }
        None
    }
}

fn wrap_module(ast: ast::Ast) -> ast::Value {
    ast::Value::Function((
        Symbol::Function.into_token_ref(),
        ast::FunctionBody::new()
            .with_parameters_parentheses(ContainedSpan::new(
                Symbol::LeftParen.into_token_ref(),
                TokenReference::symbol(")\n").unwrap(),
            ))
            .with_parameters(Punctuated::single(ast::Parameter::Ellipse(
                Symbol::Ellipse.into_token_ref(),
            )))
            .with_block(ast.take_nodes())
            .with_end_token(TokenReference::symbol("\nend\n").unwrap()),
    ))
}

pub struct Packer {
    searchers: Vec<Box<dyn Searcher + 'static>>,
    excludes: BTreeSet<String>,
    preload: BTreeMap<String, Ast>,
    prelude: Ast,
}

impl Packer {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_searcher<S: Searcher + 'static>(mut self, searcher: S) -> Self {
        self.searchers.push(Box::new(searcher));
        self
    }

    pub fn add_exclude<S: Into<String>>(mut self, name: S) -> Self {
        self.excludes.insert(name.into());
        self
    }

    pub fn add_excludes<S: Into<String>, I: IntoIterator<Item = S>>(mut self, names: I) -> Self {
        self.excludes.extend(names.into_iter().map(Into::into));
        self
    }

    pub fn add_preload<S: Into<String>, L: Loader>(mut self, name: S, loader: L) -> Result<Self> {
        self.preload.insert(name.into(), loader.load()?);
        Ok(self)
    }

    pub fn add_preloads<S: Into<String>, L: Loader, I: IntoIterator<Item = (S, L)>>(
        mut self,
        modules: I,
    ) -> Result<Self> {
        for (n, l) in modules {
            self = self.add_preload(n, l)?;
        }
        Ok(self)
    }

    pub fn pack<L: Loader>(&self, entry: L) -> Result<Ast> {
        let mut unresolved = BTreeSet::new();
        let mut resolved = BTreeMap::new();

        let mut packer = PackerVisitor::new(|args: Vec<_>| {
            if let Some(Some(RuntimeValue::String(n))) = args.first() {
                unresolved.insert(n.clone());
            }
        });

        let ast = packer.visit_ast(entry.load()?);

        'upper: while let Some(modname) = unresolved.pop_first() {
            for k in self.excludes.iter() {
                if k == &modname || (k.ends_with('.') && modname.starts_with(k)) {
                    log::info!("Ignoring excluded module: {}", modname);
                    continue 'upper;
                } else if k > &modname {
                    break;
                }
            }

            let mut packer = PackerVisitor::new(|args: Vec<_>| {
                if let Some(Some(RuntimeValue::String(n))) = args.first() {
                    if !resolved.contains_key(n) {
                        unresolved.insert(n.clone());
                    }
                }
            });

            for (k, p) in self.preload.iter() {
                if k == &modname || (k.ends_with('.') && modname.starts_with(k)) {
                    log::info!("Using preloaded module: {}", modname);
                    resolved.insert(modname, packer.visit_ast(p.clone()));
                    continue 'upper;
                } else if k > &modname {
                    break;
                }
            }

            if let Some((loader, path)) = self.searchers.search(&modname) {
                log::info!("Resolved module: {} at {}", modname, path.display());
                resolved.insert(modname, packer.visit_ast(loader.load()?));
            } else {
                log::warn!("Unresolved module: {}", modname);
            }
        }

        let bundles = resolved.into_iter().map(|(modname, ast)| {
            let vars = Punctuated::single(ast::Var::Expression(
                ast::VarExpression::name("__luapack").with_suffixes(vec![
                    ast::Suffix::Index(ast::Index::Dot {
                        dot: Symbol::Dot.into_token_ref(),
                        name: TokenReference::identifier("preload"),
                    }),
                    ast::Suffix::Index(ast::Index::Brackets {
                        brackets: ContainedSpan::new(
                            Symbol::LeftBracket.into_token_ref(),
                            Symbol::RightBracket.into_token_ref(),
                        ),
                        expression: ast::Expression::value(modname.into_value()),
                    }),
                ]),
            ));

            let exprs = Punctuated::single(ast::Expression::value(wrap_module(ast)));

            ast::Stmt::Assignment(
                ast::Assignment::new(vars, exprs)
                    .with_equal_token(TokenReference::symbol(" = ").unwrap()),
            )
        });

        let last_stmt = ast.nodes().last_stmt_with_semicolon().cloned();

        let stmts = self
            .prelude
            .nodes()
            .stmts_with_semicolon()
            .cloned()
            .chain(bundles.map(|s| (s, None)))
            .chain(ast.nodes().stmts_with_semicolon().cloned())
            .collect();

        Ok(ast.with_nodes(
            ast::Block::new()
                .with_stmts(stmts)
                .with_last_stmt(last_stmt),
        ))
    }

    pub fn pack_to_string<L: Loader>(&mut self, entry: L) -> Result<String> {
        Ok(full_moon::print(&self.pack(entry)?))
    }
}

impl Default for Packer {
    fn default() -> Self {
        Self {
            searchers: Default::default(),
            excludes: Default::default(),
            preload: Default::default(),
            prelude: full_moon::parse(PRELUDE).unwrap(),
        }
    }
}

#[derive(Clone, Debug)]
struct PackerVisitor<F> {
    vm: VmVisitor,
    register_require: F,
    in_require_call: bool,
}

impl<F> PackerVisitor<F> {
    pub fn new(register_require: F) -> Self {
        Self {
            vm: VmVisitor::new(),
            register_require,
            in_require_call: false,
        }
    }
}

impl<F> VisitorMut for PackerVisitor<F>
where
    F: FnMut(Vec<Option<RuntimeValue>>),
{
    fn visit_block(&mut self, node: ast::Block) -> ast::Block {
        self.vm.visit_block(node)
    }

    fn visit_block_end(&mut self, node: ast::Block) -> ast::Block {
        self.vm.visit_block_end(node)
    }

    fn visit_assignment(&mut self, node: ast::Assignment) -> ast::Assignment {
        self.vm.visit_assignment(node)
    }

    fn visit_local_assignment(&mut self, node: ast::LocalAssignment) -> ast::LocalAssignment {
        self.vm.visit_local_assignment(node)
    }

    fn visit_function_declaration(
        &mut self,
        node: ast::FunctionDeclaration,
    ) -> ast::FunctionDeclaration {
        self.vm.visit_function_declaration(node)
    }

    fn visit_local_function(&mut self, node: ast::LocalFunction) -> ast::LocalFunction {
        self.vm.visit_local_function(node)
    }

    fn visit_function_body(&mut self, node: ast::FunctionBody) -> ast::FunctionBody {
        self.vm.visit_function_body(node)
    }

    fn visit_function_body_end(&mut self, node: ast::FunctionBody) -> ast::FunctionBody {
        self.vm.visit_function_body_end(node)
    }

    fn visit_generic_for(&mut self, node: ast::GenericFor) -> ast::GenericFor {
        self.vm.visit_generic_for(node)
    }

    fn visit_generic_for_end(&mut self, node: ast::GenericFor) -> ast::GenericFor {
        self.vm.visit_generic_for_end(node)
    }

    fn visit_numeric_for(&mut self, node: ast::NumericFor) -> ast::NumericFor {
        self.vm.visit_numeric_for(node)
    }

    fn visit_numeric_for_end(&mut self, node: ast::NumericFor) -> ast::NumericFor {
        self.vm.visit_numeric_for_end(node)
    }

    fn visit_eof(&mut self, node: TokenReference) -> TokenReference {
        self.vm.visit_eof(node)
    }

    fn visit_function_call(&mut self, node: ast::FunctionCall) -> ast::FunctionCall {
        let node = self.vm.visit_function_call(node);
        if let ast::Prefix::Name(n) = node.prefix() {
            if let TokenType::Identifier { identifier } = n.token_type() {
                if identifier.as_str() == "require" {
                    self.in_require_call = true;
                }
            }
        }
        node
    }

    fn visit_function_call_end(&mut self, node: ast::FunctionCall) -> ast::FunctionCall {
        let node = self.vm.visit_function_call_end(node);
        self.in_require_call = false;
        node
    }

    fn visit_var_expression(&mut self, node: ast::VarExpression) -> ast::VarExpression {
        let node = self.vm.visit_var_expression(node);
        if let ast::Prefix::Name(n) = node.prefix() {
            if let TokenType::Identifier { identifier } = n.token_type() {
                if identifier.as_str() == "require" {
                    if let Some(ast::Suffix::Call(_)) = node.suffixes().next() {
                        self.in_require_call = true;
                    }
                }
            }
        }
        node
    }

    fn visit_call_end(&mut self, node: ast::Call) -> ast::Call {
        let node = self.vm.visit_call_end(node);
        self.in_require_call = false;
        node
    }

    fn visit_function_args(&mut self, node: ast::FunctionArgs) -> ast::FunctionArgs {
        let node = self.vm.visit_function_args(node);
        if self.in_require_call {
            let args = match &node {
                ast::FunctionArgs::Parentheses { arguments, .. } => {
                    arguments.iter().map(|e| self.vm.eval(e)).collect()
                }
                ast::FunctionArgs::String(v) => vec![RuntimeValue::from_token(v)],
                ast::FunctionArgs::TableConstructor(c) => {
                    vec![self
                        .vm
                        .eval(&ast::Expression::value(ast::Value::TableConstructor(
                            c.clone(),
                        )))]
                }
                _ => Vec::new(),
            };
            log::debug!("Registering require call with args: {:?}", args);
            (self.register_require)(args);
        }
        node
    }
}
