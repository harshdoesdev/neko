use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use neko::parser::AstNode;
use neko::tokenizer::Operator;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
    String(String),
    List(Vec<Value>),
    Map(HashMap<String, Value>),
    Function(Function),
    NativeFunction(NativeFunction),
}

#[derive(Debug, Clone)]
pub struct Function {
    name: String,
    params: Vec<String>,
    body: Box<AstNode>,
    closure: Option<Rc<RefCell<Environment>>>,
}

/// A function implemented in Rust that can be called from the script
#[derive(Clone)]
pub struct NativeFunction {
    name: String,
    function: Rc<dyn Fn(&Interpreter, Vec<Value>) -> Result<Value, RuntimeError>>,
}

impl std::fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeFunction")
            .field("name", &self.name)
            .field("function", &"<function>")
            .finish()
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.params == other.params
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

/// Environment for storing variables and functions
#[derive(Debug, Clone)]
pub struct Environment {
    values: HashMap<String, Value>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    /// Create a new empty environment
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            parent: None,
        }
    }

    /// Create a new environment with a parent
    pub fn with_parent(parent: Rc<RefCell<Environment>>) -> Self {
        Self {
            values: HashMap::new(),
            parent: Some(parent),
        }
    }

    /// Define a variable in the current environment
    pub fn define(&mut self, name: &str, value: Value) {
        self.values.insert(name.to_string(), value);
    }

    /// Get a variable value, looking up in parent environments if necessary
    pub fn get(&self, name: &str) -> Option<Value> {
        match self.values.get(name) {
            Some(value) => Some(value.clone()),
            None => {
                // Look in parent environment
                match &self.parent {
                    Some(parent) => parent.borrow().get(name),
                    None => None,
                }
            }
        }
    }

    /// Assign a value to an existing variable
    pub fn assign(&mut self, name: &str, value: Value) -> Result<(), RuntimeError> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value);
            Ok(())
        } else {
            // Try to assign in parent environment
            match &self.parent {
                Some(parent) => parent.borrow_mut().assign(name, value),
                None => Err(RuntimeError::UndefinedVariable(name.to_string())),
            }
        }
    }

    /// Register a native function
    pub fn register_native_fn<F>(&mut self, name: &str, f: F)
    where
        F: Fn(&Interpreter, Vec<Value>) -> Result<Value, RuntimeError> + 'static,
    {
        let native_fn = NativeFunction {
            name: name.to_string(),
            function: Rc::new(f),
        };
        self.define(name, Value::NativeFunction(native_fn));
    }
}

#[derive(Debug, Error, PartialEq)]
pub enum RuntimeError {
    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),

    #[error("Undefined function: {0}")]
    UndefinedFunction(String),

    #[error("Type error: {0}")]
    TypeError(String),

    #[error("Invalid operation: {0}")]
    InvalidOperation(String),

    #[error("Index error: {0}")]
    IndexError(String),

    #[error("Function call error: {0}")]
    FunctionCallError(String),

    #[error("Return value: {0:?}")]
    ReturnValue(Box<Value>),
}

/// The interpreter for evaluating AST nodes
pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
}

impl Interpreter {
    /// Create a new interpreter with a global environment
    pub fn new() -> Self {
        let env = Rc::new(RefCell::new(Environment::new()));

        // Register built-in functions
        Self::register_standard_library(&env);

        Self { environment: env }
    }

    /// Register built-in functions and values
    fn register_standard_library(env: &Rc<RefCell<Environment>>) {
        let mut env = env.borrow_mut();

        // Register print function
        env.register_native_fn("print", |_, args| {
            for arg in args {
                print!("{}", Interpreter::stringify_value(&arg));
            }
            Ok(Value::Nil)
        });

        // Register println function
        env.register_native_fn("println", |_, args| {
            for arg in args {
                println!("{}", Interpreter::stringify_value(&arg));
            }
            Ok(Value::Nil)
        });

        // Register len function
        env.register_native_fn("len", |_, args| {
            if args.is_empty() {
                return Err(RuntimeError::FunctionCallError(
                    "len requires at least one argument".to_string(),
                ));
            }

            // Get the value/reference from the first argument
            let value = &args[0];

            // Determine the length based on its type
            match value {
                Value::List(list) => Ok(Value::Number(list.len() as f64)),
                v => Err(RuntimeError::TypeError(
                    format!("cannot determine length of type {:#?}", v), // TODO: impl display for types
                )),
            }
        });

        // Register range function
        env.register_native_fn("range", |_, args| match args.as_slice() {
            [Value::Number(start), Value::Number(end)] => {
                let range: Vec<Value> = (*start as i64..*end as i64)
                    .map(|i| Value::Number(i as f64))
                    .collect();
                Ok(Value::List(range))
            }
            [Value::Number(end)] => {
                let range: Vec<Value> = (0..*end as i64).map(|i| Value::Number(i as f64)).collect();
                Ok(Value::List(range))
            }
            _ => Err(RuntimeError::TypeError(
                "range requires 1 or 2 numeric arguments".to_string(),
            )),
        });

        env.register_native_fn("invoke", |interpreter, args| {
            if args.is_empty() {
                return Err(RuntimeError::FunctionCallError(
                    "invoke requires at least one argument (function reference)".to_string(),
                ));
            }

            // Get the function reference from the first argument
            let function_value = &args[0];
            let remaining_args = &args[1..];

            // Call the function based on its type
            match function_value {
                Value::Function(func) => interpreter.call_function(func, remaining_args.to_vec()),
                Value::NativeFunction(native_fn) => {
                    (native_fn.function)(interpreter, remaining_args.to_vec())
                }
                _ => Err(RuntimeError::TypeError(
                    "First argument to invoke must be a function".to_string(),
                )),
            }
        });
    }

    /// Helper to convert a Value to a string representation
    fn stringify_value(value: &Value) -> String {
        match value {
            Value::Nil => "nil".to_string(),
            Value::Number(n) => n.to_string(),
            Value::Boolean(b) => b.to_string(),
            Value::String(s) => s.clone(),
            Value::List(items) => {
                let items_str: Vec<String> = items
                    .iter()
                    .map(|item| Self::stringify_value(item))
                    .collect();
                format!("[{}]", items_str.join(", "))
            }
            Value::Map(entries) => {
                let entries_str: Vec<String> = entries
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, Self::stringify_value(v)))
                    .collect();
                format!("{{{}}}", entries_str.join(", "))
            }
            Value::Function(f) => format!("<function {}>", f.name),
            Value::NativeFunction(f) => format!("<native function {}>", f.name),
        }
    }

    /// Interpret an AST node, evaluating to a runtime value
    pub fn interpret(&self, node: &AstNode) -> Result<Value, RuntimeError> {
        self.evaluate(node, &self.environment)
    }

    // Helper function to get a nested value (for reading)
    fn get_nested_value(
        &self,
        container: &Value,
        indexes: &[AstNode],
        env: &Rc<RefCell<Environment>>,
    ) -> Result<Value, RuntimeError> {
        let mut current = container.clone();

        for index_expr in indexes {
            let index = self.evaluate(index_expr, env)?;

            current = match (&current, &index) {
                (Value::List(list), Value::Number(n)) => {
                    let i = *n as usize;
                    if i < list.len() {
                        list[i].clone()
                    } else {
                        return Err(RuntimeError::IndexError(format!(
                            "Index {} out of bounds for list of length {}",
                            i,
                            list.len()
                        )));
                    }
                }
                (Value::Map(map), Value::String(key)) => {
                    map.get(key).cloned().unwrap_or(Value::Nil)
                }
                _ => {
                    return Err(RuntimeError::TypeError(
                        "Invalid subscript operation".to_string(),
                    ))
                }
            };
        }

        Ok(current)
    }

    // Helper function to assign a nested value (for writing)
    fn assign_nested(
        &self,
        container: &mut Value,
        indexes: &[AstNode],
        new_value: Value,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<(), RuntimeError> {
        if indexes.is_empty() {
            return Err(RuntimeError::InvalidOperation(
                "Subscript assignment requires at least one index".to_string(),
            ));
        }

        if indexes.len() == 1 {
            let index = self.evaluate(&indexes[0], env)?;
            match (container, &index) {
                (Value::List(list), Value::Number(n)) => {
                    let i = *n as usize;
                    if i < list.len() {
                        list[i] = new_value;
                    } else {
                        return Err(RuntimeError::IndexError(format!(
                            "Index {} out of bounds for list of length {}",
                            i,
                            list.len()
                        )));
                    }
                }
                (Value::Map(map), Value::String(key)) => {
                    map.insert(key.clone(), new_value);
                }
                _ => {
                    return Err(RuntimeError::TypeError(
                        "Invalid subscript assignment".to_string(),
                    ))
                }
            }
            Ok(())
        } else {
            let index = self.evaluate(&indexes[0], env)?;
            match container {
                Value::List(list) => {
                    let i = match index {
                        Value::Number(n) => n as usize,
                        _ => {
                            return Err(RuntimeError::TypeError(
                                "List index must be a number".to_string(),
                            ))
                        }
                    };
                    if i >= list.len() {
                        return Err(RuntimeError::IndexError(format!(
                            "Index {} out of bounds for list of length {}",
                            i,
                            list.len()
                        )));
                    }
                    self.assign_nested(&mut list[i], &indexes[1..], new_value, env)
                }
                Value::Map(map) => {
                    let key = match index {
                        Value::String(k) => k,
                        _ => {
                            return Err(RuntimeError::TypeError(
                                "Map key must be a string".to_string(),
                            ))
                        }
                    };
                    let nested = map.entry(key.clone()).or_insert(Value::Map(HashMap::new()));
                    self.assign_nested(nested, &indexes[1..], new_value, env)
                }
                _ => Err(RuntimeError::TypeError(
                    "Invalid container for nested assignment".to_string(),
                )),
            }
        }
    }

    /// Evaluate an AST node in a specific environment
    fn evaluate(
        &self,
        node: &AstNode,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<Value, RuntimeError> {
        match node {
            AstNode::Nil => Ok(Value::Nil),
            AstNode::Number(n) => Ok(Value::Number(*n)),
            AstNode::Boolean(b) => Ok(Value::Boolean(*b)),
            AstNode::StringLiteral(s) => Ok(Value::String(s.clone())),

            AstNode::List(items) => {
                let mut values = Vec::new();
                for item in items {
                    values.push(self.evaluate(item, env)?);
                }
                Ok(Value::List(values))
            }

            AstNode::Map(pairs) => {
                let mut map = HashMap::new();
                for (key, value) in pairs {
                    let key_value = self.evaluate(key, env)?;
                    // Only string keys are supported for now
                    let key_string = match key_value {
                        Value::String(s) => s,
                        Value::Function(f) => f.name,
                        Value::NativeFunction(f) => f.name,
                        _ => {
                            return Err(RuntimeError::TypeError(
                                "Map keys must be strings".to_string(),
                            ))
                        }
                    };

                    map.insert(key_string, self.evaluate(value, env)?);
                }
                Ok(Value::Map(map))
            }

            AstNode::Identifier(name) => match env.borrow().get(name) {
                Some(value) => Ok(value),
                None => Err(RuntimeError::UndefinedVariable(name.clone())),
            },

            AstNode::Symbol(name) => {
                // Symbols are treated as string literals with special syntax
                Ok(Value::String(name.clone()))
            }

            AstNode::BinaryExpr { left, op, right } => {
                let left_val = self.evaluate(left, env)?;
                let right_val = self.evaluate(right, env)?;

                self.eval_binary_op(op, &left_val, &right_val)
            }

            AstNode::UnaryExpr { op, expr } => {
                let value = self.evaluate(expr, env)?;

                match op {
                    Operator::Not => match value {
                        Value::Boolean(b) => Ok(Value::Boolean(!b)),
                        _ => Err(RuntimeError::TypeError(
                            "Cannot apply 'not' to non-boolean value".to_string(),
                        )),
                    },
                    _ => Err(RuntimeError::InvalidOperation(format!(
                        "Unsupported unary operator: {:?}",
                        op
                    ))),
                }
            }

            AstNode::Block(statements) => {
                let mut result = Value::Nil;

                // Create a new environment for the block
                let block_env = Rc::new(RefCell::new(Environment::with_parent(env.clone())));

                for stmt in statements {
                    result = match self.evaluate(stmt, &block_env) {
                        Ok(val) => val,
                        Err(RuntimeError::ReturnValue(val)) => {
                            return Err(RuntimeError::ReturnValue(val))
                        }
                        Err(e) => return Err(e),
                    };
                }

                Ok(result)
            }

            AstNode::Assignment { name, value } => {
                let evaluated_value = self.evaluate(value, env)?;

                // Check if variable exists
                if env.borrow().get(name).is_some() {
                    env.borrow_mut().assign(name, evaluated_value.clone())?;
                } else {
                    // Define new variable
                    env.borrow_mut().define(name, evaluated_value.clone());
                }

                Ok(evaluated_value)
            }

            AstNode::FunctionDef { name, params, body } => {
                let function = Function {
                    name: name.clone(),
                    params: params.clone(),
                    body: body.clone(),
                    closure: Some(env.clone()),
                };

                env.borrow_mut()
                    .define(name, Value::Function(function.clone()));

                Ok(Value::Function(function))
            }

            AstNode::AnonFunctionDef { params, body } => {
                let function = Function {
                    name: "<anonymous>".to_string(),
                    params: params.clone(),
                    body: body.clone(),
                    closure: Some(env.clone()),
                };

                Ok(Value::Function(function))
            }

            AstNode::FunctionCall { name, args } => {
                let mut evaluated_args = Vec::new();
                for arg in args {
                    evaluated_args.push(self.evaluate(arg, env)?);
                }

                let function_value = match env.borrow().get(name) {
                    Some(val) => val,
                    None => return Err(RuntimeError::UndefinedFunction(name.clone())),
                };

                match function_value {
                    Value::Function(function) => self.call_function(&function, evaluated_args),
                    Value::NativeFunction(native_fn) => (native_fn.function)(self, evaluated_args),
                    _ => Err(RuntimeError::TypeError(format!(
                        "{} is not a function",
                        name
                    ))),
                }
            }

            AstNode::Return(expr) => {
                let value = self.evaluate(expr, env)?;
                Err(RuntimeError::ReturnValue(Box::new(value)))
            }

            AstNode::IfStmt {
                condition,
                then_branch,
                elsif_branches,
                else_branch,
            } => {
                let condition_value = self.evaluate(condition, env)?;

                if Self::is_truthy(&condition_value) {
                    self.evaluate(then_branch, env)
                } else {
                    // Check elsif branches
                    for (elsif_condition, elsif_body) in elsif_branches {
                        let elsif_condition_value = self.evaluate(elsif_condition, env)?;
                        if Self::is_truthy(&elsif_condition_value) {
                            return self.evaluate(elsif_body, env);
                        }
                    }

                    // If no branches matched, evaluate else branch if it exists
                    match else_branch {
                        Some(else_body) => self.evaluate(else_body, env),
                        None => Ok(Value::Nil),
                    }
                }
            }

            AstNode::ForStmt {
                item,
                iterator,
                body,
            } => {
                let iterator_value = self.evaluate(iterator, env)?;

                // Get the items to iterate over
                let items = match iterator_value {
                    Value::List(list) => list,
                    _ => {
                        return Err(RuntimeError::TypeError(
                            "Cannot iterate over non-list value".to_string(),
                        ))
                    }
                };

                let mut result = Value::Nil;

                // Create a new environment for the loop
                let loop_env = Rc::new(RefCell::new(Environment::with_parent(env.clone())));

                for item_value in items {
                    loop_env.borrow_mut().define(item, item_value);

                    match self.evaluate(body, &loop_env) {
                        Ok(val) => result = val,
                        Err(RuntimeError::ReturnValue(val)) => {
                            return Err(RuntimeError::ReturnValue(val))
                        }
                        Err(e) => return Err(e),
                    }
                }

                Ok(result)
            }

            AstNode::Subscript { name, indexes } => {
                let container = match env.borrow().get(name) {
                    Some(val) => val,
                    None => return Err(RuntimeError::UndefinedVariable(name.clone())),
                };

                self.get_nested_value(&container, indexes, env)
            }

            AstNode::SubscriptAssignment {
                name,
                indexes,
                value,
            } => {
                let evaluated_value = self.evaluate(value, env)?;

                let mut container = match env.borrow().get(name) {
                    Some(val) => val.clone(),
                    None => return Err(RuntimeError::UndefinedVariable(name.clone())),
                };

                self.assign_nested(&mut container, indexes, evaluated_value.clone(), env)?;
                env.borrow_mut().assign(name, container)?;

                Ok(evaluated_value)
            }

            AstNode::InterpolatedString { parts, vars } => {
                let mut result = String::new();

                if !parts.is_empty() {
                    result.push_str(&parts[0]);
                }

                // Iterate over vars and append corresponding parts if they exist
                for i in 0..vars.len() {
                    let var_value = self.evaluate(&vars[i], env)?;
                    result.push_str(&Self::stringify_value(&var_value));
                    // Append the next part if it exists (i + 1 < parts.len())
                    if i + 1 < parts.len() {
                        result.push_str(&parts[i + 1]);
                    }
                }

                Ok(Value::String(result))
            }
        }
    }

    fn call_function(&self, function: &Function, args: Vec<Value>) -> Result<Value, RuntimeError> {
        // Check if the number of arguments matches the function parameters
        if args.len() != function.params.len() {
            return Err(RuntimeError::FunctionCallError(format!(
                "Function {} expected {} arguments but got {}",
                function.name,
                function.params.len(),
                args.len()
            )));
        }

        // Create a new environment for the function call
        let closure_env = match &function.closure {
            Some(env) => env.clone(),
            None => self.environment.clone(),
        };

        let func_env = Rc::new(RefCell::new(Environment::with_parent(closure_env)));

        // Bind arguments to parameters
        for (param, arg) in function.params.iter().zip(args) {
            func_env.borrow_mut().define(param, arg);
        }

        // Execute the function body
        match self.evaluate(&function.body, &func_env) {
            Ok(val) => Ok(val),
            Err(RuntimeError::ReturnValue(val)) => Ok(*val),
            Err(e) => Err(e),
        }
    }

    /// Helper to determine if a value is "truthy"
    fn is_truthy(value: &Value) -> bool {
        match value {
            Value::Nil => false,
            Value::Boolean(b) => *b,
            Value::Number(n) => *n != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::List(items) => !items.is_empty(),
            Value::Map(entries) => !entries.is_empty(),
            Value::Function(_) | Value::NativeFunction(_) => true,
        }
    }

    /// Evaluate a binary operation
    fn eval_binary_op(
        &self,
        op: &Operator,
        left: &Value,
        right: &Value,
    ) -> Result<Value, RuntimeError> {
        match (op, left, right) {
            // Numeric operations
            (Operator::Plus, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            (Operator::Minus, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
            (Operator::Asterisk, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
            (Operator::Slash, Value::Number(a), Value::Number(b)) => {
                if *b == 0.0 {
                    Err(RuntimeError::InvalidOperation(
                        "Division by zero".to_string(),
                    ))
                } else {
                    Ok(Value::Number(a / b))
                }
            }
            (Operator::Percent, Value::Number(a), Value::Number(b)) => {
                if *b == 0.0 {
                    Err(RuntimeError::InvalidOperation("Modulo by zero".to_string()))
                } else {
                    Ok(Value::Number(a % b))
                }
            }

            // String concatenation
            (Operator::Plus, Value::String(a), Value::String(b)) => {
                Ok(Value::String(format!("{}{}", a, b)))
            }

            // Comparison operations
            (Operator::DoubleEqual, a, b) => Ok(Value::Boolean(a == b)),
            (Operator::NotEqual, a, b) => Ok(Value::Boolean(a != b)),

            // Numeric comparisons
            (Operator::Greater, Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a > b)),
            (Operator::GreaterEqual, Value::Number(a), Value::Number(b)) => {
                Ok(Value::Boolean(a >= b))
            }
            (Operator::Less, Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a < b)),
            (Operator::LessEqual, Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a <= b)),

            // Logical operations
            (Operator::And, _, _) => {
                if !Self::is_truthy(left) {
                    Ok(left.clone())
                } else {
                    Ok(right.clone())
                }
            }
            (Operator::Or, _, _) => {
                if Self::is_truthy(left) {
                    Ok(left.clone())
                } else {
                    Ok(right.clone())
                }
            }

            // Type error for incompatible operations
            _ => Err(RuntimeError::TypeError(format!(
                "Cannot apply {:?} to {:?} and {:?}",
                op, left, right
            ))),
        }
    }
}
