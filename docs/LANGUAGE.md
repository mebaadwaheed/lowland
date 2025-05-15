# Lowland Programming Language Documentation

## Table of Contents
1. [Language Overview](#language-overview)
2. [Basic Syntax](#basic-syntax)
3. [Data Types](#data-types)
4. [Variables and Mutability](#variables-and-mutability)
5. [Control Flow](#control-flow)
6. [Functions](#functions)
7. [Modules and Imports](#modules-and-imports)
8. [Standard Library](#standard-library)
9. [Error Codes](#error-codes)
10. [Language Features](#language-features)

## Language Overview

Lowland is a statically-typed programming language designed for simplicity and clarity. It features a clean syntax, strong type checking, and a comprehensive standard library.

## Basic Syntax

### Comments
```lowland
// Single-line comment
/* Multi-line
   comment */
```

### Basic Statements
```lowland
// Variable declaration
let x: int = 10;
let& y: string = "Hello"; // Mutable variable

// Print statements
println("Hello, World!");
print("No newline");

// Expression statements
x = x + 1;
```

## Data Types

### Primitive Types
- `int`: 64-bit integer
- `float`: 64-bit floating-point
- `bool`: Boolean (true/false)
- `string`: String of characters
- `null`: Null value

### Complex Types
- `list`: Dynamic array of values
- `object`: Key-value pairs

## Variables and Mutability

### Variable Declaration
```lowland
// Immutable variable
let x: int = 10;

// Mutable variable
let& y: string = "Hello";
```

### Type Inference
```lowland
let x = 10;        // Inferred as int
let y = 3.14;      // Inferred as float
let z = "Hello";   // Inferred as string
```

## Control Flow

### If Statements
```lowland
if (condition) {
    // code
} elif (other_condition) {
    // code
} else {
    // code
}
```

### Loops
```lowland
// While loop
while (condition) {
    // code
}

// For loop
for (let& i: int = 0; i < 10; i = i + 1) {
    // code
}

// Break and continue
while (true) {
    if (condition) {
        break;
    }
    if (other_condition) {
        continue;
    }
}
```

## Functions

### Function Declaration
```lowland
func add(a: int, b: int): int {
    return a + b;
}

// Exported function
export func multiply(a: int, b: int): int {
    return a * b;
}
```

### Function Calls
```lowland
let result = add(5, 3);
```

## Modules and Imports

### Importing Standard Library
```lowland
// Import specific functions
including [std::math::sqrt, std::math::abs] from "standard";

// Import entire module
including [std::math] from "standard";
```

### Importing Local Files
```lowland
// Import specific functions
including [func1, func2] from "localfile.lln";

// Import all exported functions
including "localfile.lln";
```

## Standard Library

### Math Module (std::math)
```lowland
// Available functions
math.sqrt(x)      // Square root
math.abs(x)       // Absolute value
math.pow(x, y)    // Power function
math.floor(x)     // Floor function

// Usage examples
let x = math.sqrt(16);    // 4.0
let y = math.abs(-10);    // 10
let z = math.pow(2, 3);   // 8
let w = math.floor(3.7);  // 3.0
```

### Terminal Module (std::terminal)
```lowland
// Input/Output
inputln()           // Read a line from stdin
println(x, y, z)    // Print values with newline
print(x, y, z)      // Print values without newline
```

### Type Conversion
```lowland
toInt(x)      // Convert to integer
toFloat(x)    // Convert to float
toString(x)   // Convert to string
toBool(x)     // Convert to boolean
```

### List Methods
```lowland
list.push(x)    // Add element to end
list.pop()      // Remove and return last element
list.len()      // Get length
list.get(i)     // Get element at index
list.set(i, x)  // Set element at index
```

## Error Codes

### Syntax Errors (P0000-P9999)
- `P0000`: Generic syntax error
- `P0001`: Invalid operator
- `P0002`: Missing semicolon
- `P0003`: Invalid expression

### Type Errors (T0000-T9999)
- `T0001`: Type mismatch
- `T0002`: Invalid operation for type
- `T0003`: Invalid binary operation
- `T0004`: Variable not mutable
- `T0005`: Expected boolean condition
- `T0006`: Function return type mismatch
- `T0007`: Invalid argument type
- `T0008`: Property not found
- `T0009`: Cannot set property
- `T0010`: Invalid method call

### Runtime Errors (R0000-R9999)
- `R0000`: Generic runtime error
- `R0001`: Undefined variable
- `R0002`: Division by zero
- `R0003`: Property not found
- `R0004`: Function not defined
- `R0005`: Invalid argument count
- `R0006`: Invalid increment/decrement
- `R0007`: Modulo by zero
- `R0008`: Method not found

### I/O Errors (I0000-I9999)
- `I0001`: File not found
- `I0002`: Failed to flush stdout
- `I0003`: Failed to read file/input

## Best Practices

1. **Type Safety**
   - Always declare variable types explicitly
   - Use type conversion functions when needed
   - Check for null values before operations

2. **Error Handling**
   - Validate input before operations
   - Check for division by zero
   - Verify array indices before access

3. **Code Organization**
   - Use meaningful variable and function names
   - Export only necessary functions
   - Group related functionality in modules

4. **Performance**
   - Use appropriate data types
   - Avoid unnecessary type conversions
   - Reuse existing functions when possible

## Language Features

### Print Statements: println() vs println!()

#### println()
- Standard print function that adds a newline at the end
- Takes multiple arguments separated by commas
- Automatically converts values to strings
- Adds spaces between arguments
- Example:
```lowland
println("Hello", 42, true);  // Output: Hello 42 true
```

#### println!()
- Raw print function that adds a newline at the end
- Takes multiple arguments separated by commas
- No automatic spacing between arguments
- Preserves exact formatting
- Example:
```lowland
println!("Hello", 42, true);  // Output: Hello42true
```

### Variable Declaration: let vs let&

#### let
- Creates an immutable variable
- Value cannot be changed after initialization
- Better for constants and values that shouldn't change
- More memory efficient
- Example:
```lowland
let x: int = 10;
// x = 20;  // Error: Cannot assign to immutable variable
```

#### let&
- Creates a mutable variable
- Value can be changed after initialization
- Used when the value needs to be modified
- Required for loop counters and accumulators
- Example:
```lowland
let& y: int = 10;
y = 20;  // OK: Can modify mutable variable
```

### When to Use Each

#### Use let when:
- The value is a constant
- The value should not change
- Working with function parameters
- Storing configuration values
- Improving code safety

#### Use let& when:
- The value needs to be modified
- Working with loop counters
- Building accumulators
- Implementing state machines
- Handling user input

#### Use println() when:
- Printing multiple values with spacing
- Debugging with readable output
- Displaying formatted data
- General purpose printing

#### Use println!() when:
- Need exact formatting without spaces
- Printing concatenated values
- Working with specific output formats
- Performance is critical 