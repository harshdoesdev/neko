# neko
a smol general purpose scripting language

## example

```neko
# variables
name = "supercat1"
num = 1

# formatted string
message = "Hello, #{name}!"

# list
fruits = ["apple", "mango", "banana"]

# for loop
for index in range(0, len(fruits))
    item = fruits[index]
    println("Item: #{item}")
end

# map
person = {
    "name" => "Harsh Singh",
    "company" => "Shuru Technologies",
    "role" => "Product Engineer",
    "print_about" => fn(self)
        name = self["name"]
        company = self["company"]
        role = self["role"]
        println("#{name} is a #{role} at #{company}")
    end
}

# function

fn greet(person)
    name = person["name"]
    println("Hello, #{name}!")
end

greet(person)

invoke(person["print_about"], person)
```

## todos
- [x] basic langugage setup
- [x] basic example
- [ ] human-friendly error messages
- [ ] neko standard library

## known issues
```neko
# neko assumes it is a single statement
var =
print("Hello, World!")

# a workaround for now
var = nil
print("Hello, World!")
```
