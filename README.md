# Lowland Programming Language

Lowland is a statically-typed, interpreted programming language designed for performance and ease of use. 

## Features

- Clean, readable syntax
- Static typing
- High performance interpreter written in Rust
- Garbage collection for memory management
- Modern CLI tool

## Supported Types

- `string`: Text values
- `int`: Integer numbers
- `bool`: Boolean values (true/false)
- `obj`: Object values with properties
- `list`: List of values

## Getting Started

### Prerequisites

You don't need to have Rust installed to run Lowland. The interpreter is designed to be distributed as a standalone binary.

### Installation

Download the binary for your platform from the releases page or build from source:

```bash
cargo build --release
```

The binary will be located at `target/release/low`.

## Usage

Lowland supports files with the extensions `.lln` or `.lowl`.

### CLI Commands

- `low run <file>`: Run a Lowland script
- `low start`: Start an interactive REPL
- `low build <file>`: Build a Lowland program (coming soon)

### Examples

Create a file named `hello.lln`:

```
println("Hello, Lowland!");
```

Run it:

```bash
low run hello.lln
```

### Using the REPL

Start the REPL:

```bash
low start
```

Try some expressions:

```
>> println("Hello, World!");
>> println(42);
>> println(true);
>> println(2 + 2 * 2);
```

## Language Features

### Literals

```
// String
"This is a string"

// Integer
42

// Boolean
true
false

// Object
{}

// List
[]
```

### Operations

```
// Arithmetic
1 + 2   // Addition
3 - 1   // Subtraction
2 * 3   // Multiplication
6 / 2   // Division

// Comparison
1 == 1  // Equal
1 != 2  // Not equal
1 < 2   // Less than
1 <= 2  // Less than or equal
2 > 1   // Greater than
2 >= 1  // Greater than or equal

// Logical
!true   // Logical NOT
```

### Built-in Functions

```
// Print to console
println("Hello");
```

## License

This project is licensed under the MIT License - see the LICENSE file for details. 