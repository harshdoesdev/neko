use neko_lang::error::Error;
use neko_lang::interpreter::{Interpreter, Value};
use neko_lang::parser::Parser;
use neko_lang::tokenizer::Tokenizer;

pub fn run_source(source: &str) -> Result<Value, Error> {
    let tokens = Tokenizer::new(source);
    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;
    let interpreter = Interpreter::new();
    interpreter.interpret(&ast).map_err(Error::RuntimeError)
}
