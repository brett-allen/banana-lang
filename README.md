# Banana Lang

A programming language interpreter written in Zig, implementing a lexer, parser, and AST representation. This project is inspired by the [Monkey programming language](https://monkeylang.org/) and serves as an educational implementation of language parsing techniques.

## Features

### Currently Implemented

- **Lexer**: Tokenizes source code into tokens
- **Parser**: Pratt parser implementation for parsing expressions and statements
- **AST**: Abstract Syntax Tree representation of parsed code
- **Language Features**:
  - Variable declarations (`let`)
  - Identifiers
  - Integer literals
  - Prefix expressions (`-`, `!`)
  - Infix expressions (`+`, `-`, `*`, `/`, `==`, `!=`, `<`, `>`)
  - Operator precedence

### Planned Features

- Expression evaluation
- Environment/variable storage
- Function definitions and calls
- Control flow (`if`/`else`)
- Return statements
- Built-in functions
- String literals
- Error handling and reporting

## Requirements

- [Zig](https://ziglang.org/) 0.15.2 or later

## Building

```bash
# Build the project
zig build

# Run with a source file
./zig-out/bin/banana-lang <file.nana>

# Or use the build system
zig build run -- <file.nana>
```

## Usage

Create a `.nana` file with Banana Lang code:

```nana
let a = 2;
let b = 3;
let c = a + b;
```

Then run it:

```bash
./zig-out/bin/banana-lang example.nana
```

The interpreter will parse the code and print the AST representation.

## Example

Given `test.nana`:

```nana
let a = 2;
let b = 3;
let c = a + b;
```

The parser will output the AST representation of these statements.

## Project Structure

```
banana-lang/
├── src/
│   ├── main.zig      # Entry point, file I/O, and program orchestration
│   ├── lex.zig       # Lexer - tokenizes source code
│   ├── token.zig      # Token definitions and types
│   ├── parser.zig     # Pratt parser - parses tokens into AST
│   └── ast.zig        # Abstract Syntax Tree definitions
├── build.zig          # Zig build configuration
├── test.nana          # Example source file
└── README.md          # This file
```

## Architecture

### Lexer (`lex.zig`)
- Reads source code character by character
- Produces tokens (identifiers, literals, operators, keywords)
- Handles whitespace and comments

### Parser (`parser.zig`)
- Implements Pratt parsing algorithm for operator precedence
- Parses expressions with proper associativity
- Builds AST nodes for statements and expressions

### AST (`ast.zig`)
- Defines the Abstract Syntax Tree structure
- Includes nodes for:
  - Programs
  - Statements (let statements, expression statements)
  - Expressions (identifiers, literals, prefix/infix expressions)

## Language Syntax

### Variable Declaration
```nana
let x = 5;
let name = "value";
```

### Expressions
```nana
let sum = 1 + 2;
let product = 3 * 4;
let negative = -5;
let not = !true;
```

### Operator Precedence
Operators are parsed with the following precedence (lowest to highest):
1. `==`, `!=` (equality)
2. `<`, `>` (comparison)
3. `+`, `-` (addition/subtraction)
4. `*`, `/` (multiplication/division)
5. `-`, `!` (prefix operators)

## Development

### Running Tests
Tests are planned but not yet implemented. The project structure supports adding tests using Zig's built-in testing framework.

### Memory Management
The project uses Zig's `ArenaAllocator` for memory management, which automatically frees all allocated memory when the arena is deinitialized. This simplifies memory management for the AST.

## License

[Add your license here]

## Contributing

[Add contribution guidelines here]

## Acknowledgments

- Inspired by [Writing An Interpreter In Go](https://interpreterbook.com/) by Thorsten Ball
- Built with [Zig](https://ziglang.org/)
