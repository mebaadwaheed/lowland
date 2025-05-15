# Lowland Language Examples

## Basic Examples

### Hello World
```lowland
println("Hello, World!");
```

### Variables and Types
```lowland
// Variable declarations
let x: int = 10;
let& y: float = 3.14;
let name: string = "Lowland";
let is_valid: bool = true;

// Type inference
let a = 42;        // int
let b = 3.14;      // float
let c = "Hello";   // string
let d = true;      // bool
```

### Control Flow
```lowland
// If statement
let& age: int = 18;
if (age >= 18) {
    println("Adult");
} elif (age >= 13) {
    println("Teenager");
} else {
    println("Child");
}

// While loop
let& count: int = 0;
while (count < 5) {
    println("Count:", count);
    count = count + 1;
}

// For loop
for (let& i: int = 0; i < 5; i = i + 1) {
    println("Iteration:", i);
}
```

### Print Statement Differences
```lowland
// println() adds spaces between arguments
println("Name:", "John", "Age:", 25);  // Output: Name: John Age: 25

// println!() preserves exact formatting
println!("Name:", "John", "Age:", 25);  // Output: Name:JohnAge:25

// Formatting numbers
println("Price:", 10.5, "USD");  // Output: Price: 10.5 USD
println!("Price:", 10.5, "USD");  // Output: Price:10.5USD

// Concatenating strings
println("Hello", "World");  // Output: Hello World
println!("Hello", "World");  // Output: HelloWorld
```

### Variable Mutability
```lowland
// Immutable variable (let)
let max_score: int = 100;
// max_score = 90;  // Error: Cannot assign to immutable variable

// Mutable variable (let&)
let& current_score: int = 0;
current_score = 50;  // OK: Can modify mutable variable

// Loop counter must be mutable
for (let& i: int = 0; i < 5; i = i + 1) {
    println("Count:", i);
}

// Accumulator must be mutable
let& sum: int = 0;
for (let& i: int = 1; i <= 5; i = i + 1) {
    sum = sum + i;
}
println("Sum:", sum);

// Constants should be immutable
let PI: float = 3.14159;
let MAX_RETRIES: int = 3;
```

## Functions

### Basic Function
```lowland
func add(a: int, b: int): int {
    return a + b;
}

let result = add(5, 3);
println("Sum:", result);
```

### Exported Function
```lowland
// In math_ops.lln
export func multiply(a: int, b: int): int {
    return a * b;
}

// In main.lln
including [multiply] from "math_ops.lln";
let product = multiply(4, 6);
println("Product:", product);
```

## Standard Library Usage

### Math Module
```lowland
including [std::math] from "standard";

// Using math functions
let x = math.sqrt(16);    // 4.0
let y = math.abs(-10);    // 10
let z = math.pow(2, 3);   // 8
let w = math.floor(3.7);  // 3.0

println("Square root:", x);
println("Absolute value:", y);
println("Power:", z);
println("Floor:", w);
```

### List Operations
```lowland
// Create and manipulate lists
let& numbers: list = [1, 2, 3, 4, 5];

// List methods
numbers.push(6);
println("Length:", numbers.len());
println("First element:", numbers.get(0));
numbers.set(0, 10);
println("Modified first element:", numbers.get(0));

// List iteration
for (let& i: int = 0; i < numbers.len(); i = i + 1) {
    println("Element", i, ":", numbers.get(i));
}
```

### Type Conversion
```lowland
// Type conversion examples
let str_num = "42";
let num = toInt(str_num);
let float_num = toFloat(num);
let bool_val = toBool(1);
let str_val = toString(3.14);

println("Integer:", num);
println("Float:", float_num);
println("Boolean:", bool_val);
println("String:", str_val);
```

## Error Handling

### Division by Zero
```lowland
func safe_divide(a: int, b: int): int {
    if (b == 0) {
        println("Error: Division by zero!");
        return 0;
    }
    return a / b;
}

let result = safe_divide(10, 0);
```

### List Bounds Checking
```lowland
func safe_get(list: list, index: int): int {
    if (index < 0 || index >= list.len()) {
        println("Error: Index out of bounds!");
        return 0;
    }
    return list.get(index);
}

let& numbers = [1, 2, 3];
let value = safe_get(numbers, 5);
```

## Complete Program Example

### Calculator
```lowland
// Simple calculator program
func get_number(prompt: string): int {
    println(prompt);
    let& input_str: string = inputln();
    return toInt(input_str);
}

func main() {
    println("Simple Calculator");
    println("1. Add");
    println("2. Subtract");
    println("3. Multiply");
    println("4. Divide");
    
    let choice = get_number("Enter your choice (1-4):");
    let num1 = get_number("Enter first number:");
    let num2 = get_number("Enter second number:");
    
    if (choice == 1) {
        println("Result:", num1 + num2);
    } elif (choice == 2) {
        println("Result:", num1 - num2);
    } elif (choice == 3) {
        println("Result:", num1 * num2);
    } elif (choice == 4) {
        if (num2 == 0) {
            println("Error: Division by zero!");
        } else {
            println("Result:", num1 / num2);
        }
    } else {
        println("Invalid choice!");
    }
}

main();
``` 