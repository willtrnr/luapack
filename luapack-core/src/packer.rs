use crate::error::Result;
use full_moon::{
    ast::{self, Ast},
    node::Node,
    tokenizer::{Position, TokenType},
    visitors::VisitorMut,
};
use std::{
    collections::{BTreeMap, BTreeSet},
    path::{Path, PathBuf},
};

pub struct Module {
    pub path: PathBuf,
    pub ast: Ast,
}

pub trait Loader {
    fn load(self) -> Result<Ast>;
}

impl<'a> Loader for &'a Path {
    #[inline]
    fn load(self) -> Result<Ast> {
        Ok(full_moon::parse(&std::fs::read_to_string(self)?)?)
    }
}

impl Loader for PathBuf {
    #[inline]
    fn load(self) -> Result<Ast> {
        self.as_path().load()
    }
}

impl<'a> Loader for &'a str {
    #[inline]
    fn load(self) -> Result<Ast> {
        Ok(full_moon::parse(self)?)
    }
}

impl Loader for Ast {
    fn load(self) -> Result<Ast> {
        Ok(self)
    }
}

pub trait Searcher {
    fn search(&self, name: &str) -> Result<Option<(Box<dyn Loader>, PathBuf)>>;
}

pub struct SearchPaths {
    paths: Vec<PathBuf>,
}

impl<P> FromIterator<P> for SearchPaths
where
    P: Into<PathBuf>,
{
    fn from_iter<T: IntoIterator<Item = P>>(iter: T) -> Self {
        Self {
            paths: iter.into_iter().map(Into::into).collect(),
        }
    }
}

impl Searcher for SearchPaths {
    fn search(&self, name: &str) -> Result<Option<(Box<dyn Loader>, PathBuf)>> {
        let subpath = PathBuf::from_iter(name.split('.'));
        for p in &self.paths {
            let candidate = p.join(&subpath);
            if p.is_file() {
                return Ok(Some((Box::new(candidate.clone()), candidate)));
            }
        }
        Ok(None)
    }
}

impl<S> Searcher for Box<S>
where
    S: Searcher,
{
    #[inline]
    fn search(&self, name: &str) -> Result<Option<(Box<dyn Loader>, PathBuf)>> {
        (**self).search(name)
    }
}

impl<S> Searcher for Vec<S>
where
    S: Searcher,
{
    fn search(&self, name: &str) -> Result<Option<(Box<dyn Loader>, PathBuf)>> {
        for s in self {
            if let Some(res) = s.search(name)? {
                return Ok(Some(res));
            }
        }
        Ok(None)
    }
}

#[derive(Default)]
pub struct Packer {
    searchers: Vec<Box<dyn Searcher>>,
    preload: BTreeMap<String, Ast>,
}

impl Packer {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_searcher<S: Searcher + 'static>(mut self, searcher: S) -> Self {
        self.searchers.push(Box::new(searcher));
        self
    }

    pub fn add_preload<S: Into<String>, L: Loader>(mut self, name: S, loader: L) -> Result<Self> {
        self.preload.insert(name.into(), loader.load()?);
        Ok(self)
    }

    pub fn pack<L: Loader>(&self, entry: L) -> Result<Ast> {
        Ok(PackerVisitor::new(self).visit_ast(entry.load()?))
    }
}

#[derive(Clone, Default, Debug)]
struct Frame {
    range: Option<(Position, Position)>,
    locals: BTreeSet<String>,
}

impl Frame {
    pub fn new(range: Option<(Position, Position)>) -> Self {
        Self {
            range,
            locals: BTreeSet::new(),
        }
    }
}

struct PackerVisitor<'a> {
    packer: &'a Packer,
    loaded: BTreeMap<String, Ast>,
    globals: BTreeSet<String>,
    stack: Vec<Frame>,
}

impl<'a> PackerVisitor<'a> {
    pub fn new(packer: &'a Packer) -> Self {
        Self {
            packer,
            loaded: BTreeMap::new(),
            globals: BTreeSet::new(),
            stack: Vec::new(),
        }
    }

    fn push_frame(&mut self, range: Option<(Position, Position)>) {
        log::debug!("Pushing new frame for position: {:?}", range);
        self.stack.push(Frame::new(range));
    }

    fn pop_frame(&mut self) {
        let frame = self.stack.pop().expect("popping empty stack");
        log::debug!("Popped frame: {:?}", frame);
    }

    fn add_local<S: Into<String>>(&mut self, name: S) -> bool {
        self.stack
            .last_mut()
            .expect("empty stack")
            .locals
            .insert(name.into())
    }

    fn add_locals<S: Into<String>, I: IntoIterator<Item = S>>(&mut self, iter: I) {
        self.stack
            .last_mut()
            .expect("empty stack")
            .locals
            .extend(iter.into_iter().map(Into::into));
    }

    fn add_global<S: Into<String>>(&mut self, name: S) -> bool {
        self.globals.insert(name.into())
    }

    fn is_local<S: AsRef<str>>(&self, name: S) -> bool {
        let name = name.as_ref();
        for f in self.stack.iter().rev() {
            if f.locals.contains(name) {
                return true;
            }
        }
        false
    }
}

impl<'a> VisitorMut for PackerVisitor<'a> {
    fn visit_block(&mut self, node: ast::Block) -> ast::Block {
        self.push_frame(node.range());
        node
    }

    fn visit_block_end(&mut self, node: ast::Block) -> ast::Block {
        self.pop_frame();
        node
    }

    fn visit_assignment(&mut self, node: ast::Assignment) -> ast::Assignment {
        for v in node.variables() {
            if let ast::Var::Name(n) = v {
                if let TokenType::Identifier { identifier } = n.token_type() {
                    if !self.is_local(identifier.as_str()) {
                        self.add_global(identifier.as_str());
                    }
                }
            }
        }
        node
    }

    fn visit_local_assignment(&mut self, node: ast::LocalAssignment) -> ast::LocalAssignment {
        self.add_locals(node.names().iter().filter_map(|n| match n.token_type() {
            TokenType::Identifier { identifier } => Some(identifier.as_str()),
            _ => None,
        }));
        node
    }

    fn visit_function_declaration(
        &mut self,
        node: ast::FunctionDeclaration,
    ) -> ast::FunctionDeclaration {
        let fname = node.name().names();
        if fname.len() == 1 {
            if let TokenType::Identifier { identifier } =
                fname.first().unwrap().value().token_type()
            {
                if !self.is_local(identifier.as_str()) {
                    self.add_global(identifier.as_str());
                }
            }
        }
        node
    }

    fn visit_local_function(&mut self, node: ast::LocalFunction) -> ast::LocalFunction {
        if let TokenType::Identifier { identifier } = node.name().token_type() {
            self.add_local(identifier.as_str());
        }
        node
    }

    fn visit_function_body(&mut self, node: ast::FunctionBody) -> ast::FunctionBody {
        self.push_frame(node.range());
        self.add_locals(node.parameters().iter().filter_map(|p| match p {
            ast::Parameter::Name(n) => match n.token_type() {
                TokenType::Identifier { identifier } => Some(identifier.as_str()),
                _ => None,
            },
            _ => None,
        }));
        node
    }

    fn visit_function_body_end(&mut self, node: ast::FunctionBody) -> ast::FunctionBody {
        self.pop_frame();
        node
    }

    fn visit_generic_for(&mut self, node: ast::GenericFor) -> ast::GenericFor {
        self.push_frame(node.range());
        self.add_locals(node.names().iter().filter_map(|r| match r.token_type() {
            TokenType::Identifier { identifier } => Some(identifier.as_str()),
            _ => None,
        }));
        node
    }

    fn visit_generic_for_end(&mut self, node: ast::GenericFor) -> ast::GenericFor {
        self.pop_frame();
        node
    }

    fn visit_numeric_for(&mut self, node: ast::NumericFor) -> ast::NumericFor {
        self.push_frame(node.range());
        if let TokenType::Identifier { identifier } = node.index_variable().token_type() {
            self.add_local(identifier.as_str());
        }
        node
    }

    fn visit_numeric_for_end(&mut self, node: ast::NumericFor) -> ast::NumericFor {
        self.pop_frame();
        node
    }
}
