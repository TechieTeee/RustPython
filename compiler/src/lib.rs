use rustpython_codegen::{compile, symboltable};
use rustpython_parser::{
    ast::{self as ast, fold::Fold, ConstantOptimizer, Expr, Suite},
    source_code::LinearLocator,
    Parse,
};
use rustpython_compiler_core::{bytecode::CodeObject, Mode};
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum CompileErrorType {
    Codegen(rustpython_codegen::error::CodegenErrorType),
    Parse(rustpython_parser::ParseErrorType),
}

impl Error for CompileErrorType {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            CompileErrorType::Codegen(e) => e.source(),
            CompileErrorType::Parse(e) => e.source(),
        }
    }
}
impl fmt::Display for CompileErrorType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompileErrorType::Codegen(e) => e.fmt(f),
            CompileErrorType::Parse(e) => e.fmt(f),
        }
    }
}
impl From<rustpython_codegen::error::CodegenErrorType> for CompileErrorType {
    fn from(source: rustpython_codegen::error::CodegenErrorType) -> Self {
        CompileErrorType::Codegen(source)
    }
}
impl From<rustpython_parser::ParseErrorType> for CompileErrorType {
    fn from(source: rustpython_parser::ParseErrorType) -> Self {
        CompileErrorType::Parse(source)
    }
}

pub type CompileError = rustpython_parser::source_code::LocatedError<CompileErrorType>;

/// Compile a given source code into a bytecode object.
pub fn compile(
    source: &str,
    mode: Mode,
    source_path: &str,
    opts: CompileOpts,
) -> Result<CodeObject, CompileError> {
    let mut locator = LinearLocator::new(source);
    let ast = match parser::parse(source, mode.into(), source_path) {
        Ok(x) => x,
        Err(e) => return Err(locator.locate_error(e)),
    };
    
    let ast = if opts.optimize > 0 {
        ConstantOptimizer::new().fold_mod(ast)?
    } else {
        ast
    };
    
    compile::compile_top(&ast, source_path, mode, opts).map_err(Into::into)
}

pub fn compile_symtable(
    source: &str,
    mode: Mode,
    source_path: &str,
) -> Result<symboltable::SymbolTable, CompileError> {
    let mut locator = LinearLocator::new(source);
    let res = match mode {
        Mode::Exec | Mode::Single | Mode::BlockExpr => {
            let ast = Suite::parse(source, source_path).map_err(|e| locator.locate_error(e))?;
            symboltable::SymbolTable::scan_program(&ast)
        }
        Mode::Eval => {
            let expr = Expr::parse(source, source_path).map_err(|e| locator.locate_error(e))?;
            symboltable::SymbolTable::scan_expr(&expr)
        }
    };
    res.map_err(|e| e.into_codegen_error(source_path.to_owned()).into())
}
