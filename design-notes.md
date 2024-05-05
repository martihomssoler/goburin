Goburin -> goblin in japanese
Soshi -> data, element
Shazou -> mapping, map (mathematics)
Kansuu -> function (mathematics / programming)
### Relevant Info
- Koka
- JAI
- Rust
(https://www.youtube.com/watch?v=oa0qq75i9oc)
(https://craftinginterpreters.com/the-lox-language.html)
(https://github.com/BSVino/JaiPrimer/blob/master/JaiPrimer.md)
"A program is a description of a mapping from inputs to outputs." 
"Program correctness defines a set of constraints over the input to output mapping."
"In a program there is no order, only constraints."
"Data flowing through a system from inputs to outputs allows to reason about its correctness, and from outputs to inputs allows to reason about debugging."
# Key Features
1. Simple regular syntax
2. Base meta-language
3. Preserve horizontal screen space
# Brainstorm Examples
## Symbols
```cpp
// --- Comments
//   single line comments
/*  multi-line comments followed by */ to close it

// --- Type Notations
!   fallible type -> like Result<T,E> in rust, where E is the ERROR/EXCEPTION
?   nullable type -> like Option<T> in rust, where None is NULL
& 
@   
#   compiler directives
$   select reserved words
%
^   pointer type, we assume ownership by default, also used for dereferencing
~
_
=   asignment

// -- Math Symbols
we use keywords for the exponential and the modulo operator which are "pow" and "mod" respectively
+   add
*   multiply
-   substract
/   divide

//others
()  collections (unnamed members like tuples, or nammed like structs and enums)
{}  code block
[]  array (probably fixed size?)
<>  variable capturing for a code block
'   char delimiter
"   string delimiter
\   escape character
|>  pattern matching and branching  
->  function return types (e.g. a no args and returns an integer || -> i32 )
:   type anotation or other notations like array length, range step sizes, ...
,   separating values inside collections and code blocks
.   to call functions and access "members"
`

// type aliases
float : f64            // f32, f64 
int : i32              // un/signed i8, u8, i16, u16, i32, u32, i64, u64, u128
string : char[]        // <-- TBD: to be decided
char :                 // <-- TBD: to be decided
```
## Taken from JAI
```cpp
// --- Comments
/* Block Comment */
/* Nested Multi-
	/* Line-
		/* Block */ */ */

// --- Variables
// defined as "name: type = value"
// where ":" is the type definition, and "=" is the value assignment.
counter: int = 0
name: string = "Goblin"
average: float = 3.14
// if the type is omitted then the compiler infers it based on the value
counter := 0               // an int
name := "Goblin"           // a string
average := 3.14            // a float
// if the value is omitted then you have a declaration without an initialization.
counter: int
name: string
average: float

// --- Pointers
// pointers are "full Pascal"
pointer : ^int = ^5            // creating a pointer to an int
value := pointer^              // dereferencing the pointer to its value (int)
zero_ptr : ^int                // the default value for pointers is 0.

// --- Arrays and Ranges
// arrays are a homogeneous list and are declared as follows
// we can declare arrays statically, they have a length at compile-time
array : [8] float     // "8-long array of floats"
array : [8] ^float    // "8-long array of pointers to floats"
array : ^[8] float    // "pointer to an 8-long array floats"
// or dynamically, same as Rust Vec (pointer, len, capacity)
array : [..] float      // "dynamically-long array of floats"
array : [..] ^float     // "dynamically-long array of pointers to floats"
array : ^[..] float     // "pointer to a dynamically-long array floats"
// if you do not want a u64 as the default index size, you could do
array : [8:u8] float    // "dynamically-long array of floats w/ u8 index-size"
array : [..:u8] float   // "8-long array of floats w/ u8 index-size"
// ranges are sorted values with a start and end values
a := 1..10             // range from 1 to 9
a := 1..=10            // range from 1 to 10
a := 1..=10:3          // range from 1 to 10 with steps of size 3
// you can also do partial ranges
a := ..=10             // range from 0 to 10
a := 0..               // range from 0 to max int (max i32)
// we can conbine both and declare an array of length 5 with content 1,2,3,4,5
array := [..=5]

// --- Tuples & Structs
// tuples do not assign names to their members
tuple : (int, float)
age_and_height : (
	int,
	float,
)
// you can declare and give a default value
another_tuple : (int, float) = (5, 3.14)     // <-- this is the default
another_tuple : (int, float) = (_, 3.14)     // <-- partial defaults (0, 3.14)
another_tuple : (int, float) = ($1:=3.14,..) // <-- same as ---^
// struct do assign names to their members. We could call them "named tuples"
node : (
	owned_a : ^node,
	owned_b : ^node,
	value : int,
)
// we can also one-line it
node : (a : ^node, b : ^node, value : int,)
//          the last coma is optional ---^
// you can also give defaults, even partials, for structs
another_node : node = (_, _, 0)              // <-- partial defaults
another_node : node = (value := 0, ..)       // <-- same as ---^

// --- Enums
value : (
	|> ZERO,                           // untypped 
	|> ONE : (i32),                    // typed
	|> TWO : (string, string),         // typed w/ multiple members
	|> THREE : (a: string, b: f32),    // typed w/ struct
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

// --- Functions & Lambdas
// there is no distiction between them, a "function" is a named lambda
// types do not need to be explicitly defined because they can be infered
// the format to define a function is as follows
// "name := (parameters) -> return <capture> { code }"
// this format gives us the following four possibilities
// 						 		    { code } <-- Anonymous code block
//					      <capture> { code } <-- Captured code block
//      (i: int) -> float <capture> { code } <-- Anonymous function aka a lambda
// f := (i: int) -> float <capture> { code } <-- Named function
// for example:
// w/ 0 arguments and return value
five := () -> int { 5 }
// w/ unnamed arguments and return value
square := (float) -> float { $0 * $0 }
// w/ arguments but no return, which is optional to specify 
print := (a:float) { "{a}".print }
/* w/ untyped arguments which get mono-morphization at compile-time
// we can make explicit that we are using the global print function
//                            v----- explicit global print function
untyped_print := (a) -> () <print> { print "{a}" }
//                     ^---- explicit empty return value 

```
## Others Ideas
```
// you can use the double "<>" for captures to make all callees subject to 
// the same capture restriction
// <<capture>> { code } <-- Captured code block
```

```cpp
// function calls have a similar "dot notation" as koka
// function with nammed arguments
multiply : (x: float, y: float) -> float = x * y
(3.0,5.0).multiply               // calling with a tuple (unnamed members)
(a:=3.0,b:=5.0).multiply         // calling with a struct (nammed members)

// this is how you chain multiple functions
(3.0,5.0).multiply.print         // the payload is in front, similar to koka



generic_func<A> : (A, string) -> string = "{$1}"

// loops 
// for-loops can be done with the "each" function
0..10.each( i -> "{i}".print)

(0..10, i -> "{i}".print).each

each(0..10, i -> print "{i}")
// 

// advanced function

```


