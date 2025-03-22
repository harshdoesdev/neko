use neko::interpreter::{Interpreter, Value};
use neko::parser::Parser;
use neko::tokenizer::Tokenizer;

const DEMO_SOURCE_CODE: &str = r#"
# NEKO EXAMPLE #0

fn create_person(name, age, password)
    return {
        "name" => name,
        "age" => age,
        "password" => password,
        # Anonymous function
        "greet" => fn(self)
            name = self["name"]
            println("Hello, #{name}!") # prints the formatted string
        end,
        "auth" => fn(self, password)
            if self["password"] == password do
                println("Access granted!")
            else
                println("Wrong password!")
            end
        end
    }
end

# create a new person
person = create_person("Harsh Singh", 21, "hunter42")

# invoke the greeter method
invoke(person["greet"], person)

# assign new value to a property
person["name"] = "Deepak Singh"

# invoking this will print "Hello, Deepak Singh!"
invoke(person["greet"], person)

invoke(person["auth"], person, "hunter42")

fruits = ["apple", "mango", "banana",]

for i in range(len(fruits))
    item = fruits[i]
    println("Item: #{item}")
end
"#;

pub fn run_source(source: &str) -> Result<Value, String> {
    let tokens = Tokenizer::new(source);
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().map_err(|e| format!("Parse error: {}", e))?;
    let interpreter = Interpreter::new();
    interpreter
        .interpret(&ast)
        .map_err(|e| format!("Runtime error: {}", e))
}

fn main() {
    if let Err(err) = run_source(DEMO_SOURCE_CODE) {
        eprintln!("{:#?}", err);
        std::process::exit(1);
    }
}
