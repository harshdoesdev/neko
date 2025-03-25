# Project neko
a smol general purpose scripting language

## Usage

1. Clone this project, `cd` to it and install the `neko` command via `cargo install`

2. Create a file with a `.neko` extension

```neko
fn greet(name)
    println("Hello, #{name}!")
end

names = ["momo", "luna", "niko", "suki", "taro"]

for i in range(len(names))
    name = names[i]
    greet(name)
end
```

3. Run the script with the following command

```neko
neko run ./hello.neko
```

## Todos
- [x] basic langugage setup
- [x] basic example
- [x] basic cli
- [ ] human-friendly error messages
- [ ] neko standard library
