Goburin -> goblin in japanese
Soshi -> data, element
Shazou -> mapping, map (mathematics)
Kansuu -> function (mathematics / programming)
### Relevant Quotes
(https://www.youtube.com/watch?v=oa0qq75i9oc)
(https://craftinginterpreters.com/the-lox-language.html)
"A program is a description of a mapping from inputs to outputs." 
"Program correctness defines a set of constraints over the input to output mapping."
"In a program there is no order, only constraints."
"Data flowing through a system from inputs to outputs allows to reason about its correctness, and from outputs to inputs allows to reason about debugging."
# Key Features
1. Simple regular syntax
2. Base meta-language
3. Preserve horizontal screen space
# Brainstorm Examples
## Taken from JAI
```c
// Comment
alpha := "a"

/* Block Comment */
/* Nested Multi-
	/* Line-
		/* Block */ */ */

beta : string = "b"

array : float[8] = [0.0;8]

// functions with 0 arguments and return value
five : () -> int = 5
// functions with arguments but no return value
print : (a:float) -> () = println "{a}"
// functions with unnamed arguments and return value
square : (float) -> float = $0 * $0
// function with nammed arguments
multiply : (x: float, y: float) -> float = x * y
(3.0,5.0).multiply               // calling with a tuple (unnamed members)
(a:=3.0,b:=5.0).multiply         // calling with a struct (nammed members)

// this is how you chain multiple functions
(3.0,5.0).multiply.print         // the payload is in front, similar to koka

// tuples do not assign names to their members
tuple : (int, float)
age_and_height : (
	int,
	float,
)
// you can declare and give a default for all collections
another_tuple : (int, float) = (5, 3.14)     // <-- this is the default
another_tuple : (int, float) = (_, 3.14)     // <-- partial defaults (0, 3.14)
another_tuple : (int, float) = ($1:=3.14,..) // <-- same as ^

// struct are named tuples
node : (
	owned_a : &node,
	owned_b : &node,
	value : int,
)
// we can also one-line it and omit the type
another_node : (a : &another_node, b : &another_node, value : int,)
								 // the last coma is optional ---^

value : (
	| ZERO,                           // untypped 
	| ONE : (i32),                    // typed
	| TWO : (string, string),         // typed w/ multiple members
	| THREE : (a: string, b: f32),    // typed w/ struct
)

test_value := value.ZERO // this will infer type "value" 

// enums can be inlined like so
smol_enum:(|ZERO,|ONE:i32,|TWO:(string,string),|THREE:(a:string,b:f32))

// generics works with all collections and functions
generic_value<A> : (
	| ZERO, 
	| ONE : (A),
	| TWO : (string, string),
)

test_gv := generic_value.ONE("one")             // infered the type of A
test_gv : generic_value<string> = ONE("one")    // explicit type of A
test_gv := generic_value.ONE("one")      // infered the type of A

generic_func<A> : (A, string) -> string = "{$1}"

// advanced function

```
## Hello World
Rust
```rust
fn main() {
	println!("Hello World!");
}
```
Goburin
```c
let main = println "Hello World!"
```
## Print Numbers
Rust
```rust
fn main() {
	let x = 5 + 5;
	println!("Is 'x' 10 or 100? x = {x}");
}
```
Goburin
```rust
#main := { // special syntax for main function
	x = 5 + 5
	println "Is 'x' 10 ir 100? x = {x}"
}
```
## Fibonacci
Rust
```rust
fn fib(i: i32) -> i32 {
	if i <= 1 {
		1
	} else {
		fib(i-1) + fib(i-2)
	}
}
```
Goburin
```c
fib : |i: i32| -> i32 = {
	1 if i <= 1 else fib i-1 + fib i-2
}
---
fib : i32 i -> i32 1 if i <= 1 else fib i-1 + fib i-2
---
if : |bool| =  

```

## Data Types
- 8-bit
- 16-bit
- 32-bit
- 64-bit
- 128-bit

### Symbols
```
;
:   type anotation
,   separating values inside collections (tuples, structs, enums, arrays, ...)
.   to link function calls and access members
<>
`
~
!
@
#
$   select reserved words
%
*
()  collections (unnamed members like tuples, or nammed like structs and enums)
-
_
=   asignment w/ lazy computation (x : || -> string = "hello" ) is not precomp.
+
{}  totally optional for multi line code belonging to the same block
[]  array (probably fixed size?)
'
\
"
/
?
|   pattern matching / branching  
->  function return types (e.g. a no args and returns an integer || -> i32 )
```
