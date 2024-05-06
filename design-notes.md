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
2. Base meta-language with only ...
	- Data definitions
	- Function definitions
	- Reserved words
	- Compiler directives
	- Minor data flow (like the function arrow and pipe/match)
1. Preserve horizontal screen space
# Brainstorm Examples
## Symbols
```cpp
// --- Comments
//   single line comments
/*  multi-line comments followed by */ to close it

// --- Type Notations
// to the LEFT of the type mean...
!   fallible type -> like Result<T,E> in rust, where E is the ERROR/EXCEPTION
?   nullable type -> like Option<T> in rust, where None is NULL
&   pointer type, we assume ownership by default, also used for dereferencing
// to the RIGHT of the type mean...
!   unwrap fallible value and propagate error upwards -> like Result? in rust
?   unwrap nullable value or panic -> like Option? in rust
&   dereference pointer value
// --- Unused so far
@   
%
^   
~   
_
;

// -- Math Symbols
we use keywords for the exponential and the modulo operator which are "pow" and "mod" respectively
+   add
*   multiply
-   substract
/   divide

//others
=   asignment
#   compiler directives
$   reserved words
()  collections (unnamed members like tuples, or nammed like structs and enums)
{}  code block
[]  arrays
<>  variable capturing for a code block
'   Char delimiter
"   String delimiter
\   escape character
|>  pattern matching and branching  
->  function return types (e.g. a no args and returns an integer || -> I32 )
:   type anotation or other notations like array length, range step sizes, ...
::  type definitions
,   separating values inside collections and code blocks
.   to call functions and access "members"
..  used to specify ranges
`

// type aliases
Float : F64            // f32, F64 
Int: I32               // un/signed I8, U8, I16, U16, I32, U32, I64, u64, U128
String : Char[]        // <-- TBD: to be decided
Char :                 // <-- TBD: to be decided
```
## Taken from JAI
```cpp
// --- Comments
/* Block Comment */
/* Nested Multi-
	/* Line-
		/* Block */ */ */

// --- Naming Convention
// - Varibles and functions are "snake_case"
// - Types, also EnumVariants, are "PascalCase"
// - Constrants and Generics, are "SCREAMING_SNAKE_CASE"

// --- Variables
// defined as "name : type = value"
// where ":" is the type definition, and "=" is the value assignment.
counter : Int = 0
name : String = "Goblin"
average : Float = 3.14
// if the type is omitted then the compiler infers it based on the value
counter := 0               // an Int
name := "Goblin"           // a String
average := 3.14            // a Float
// if the value is omitted then you have a declaration without an initialization.
counter : Int
name : String
average : Float

// --- Pointers
// pointers have the same idea as in Pascal, but changed the symbol to "&" since in too many languages the "^" is a "dead-key" which makes it hard to type
pointer : &Int= &5            // creating a pointer to an int
value := pointer&             // dereferencing the pointer to its value (Int)
zero_ptr : &Int               // the default value for pointers is 0.

// --- Arrays and Ranges
// arrays are a homogeneous list and are declared as follows
// we can declare arrays statically, they have a length at compile-time
array : [8] Float     // "8-long array of Floats"
array : [8] &Float    // "8-long array of pointers to Floats"
array : &[8] Float    // "pointer to an 8-long array Floats"
// or dynamically, same as Rust Vec (pointer, len, capacity)
array : [..] Float      // "dynamically-long array of Floats"
array : [..] &Float     // "dynamically-long array of pointers to Floats"
array : &[..] Float     // "pointer to a dynamically-long array Floats"
// if you do not want a u64 as the default index size, you could do
array : [8:U8] Float    // "dynamically-long array of Floats w/ U8 index-size"
array : [..:U8] Float   // "8-long array of Floats w/ U8 index-size"
// ranges are sorted values with a start and end values
a := 1..10             // range from 1 to 9
a := 1..=10            // range from 1 to 10
a := 1..=10:3          // range from 1 to 10 with steps of size 3
// you can also do partial ranges
a := ..=10             // range from 0 to 10
a := 0..               // range from 0 to max Int(max I32)
// we can conbine both and declare an array of length 5 with content 1,2,3,4,5
array := [..=5]

// --- Tuples & Structs
// tuples do not assign names to their members. In order to access them we use the "$" sign followed by their position, like $0, $1, ...
tuple : (Int, Float)                         // <-- single line
age_and_height : (                           // <-- multi line
	Int,
	Float,
)
// you can declare and give a default value
another_tuple : (Int, Float) = (5, 3.14)     // <-- this is the default
another_tuple : (Int, Float) = (_, 3.14)     // <-- partial defaults (0, 3.14)
another_tuple : (Int, Float) = ($1:=3.14,..) // <-- same as ---^
// struct have named members, they are "named tuples". 
node : (
	owned_a : &node,
	owned_b : &node,
	value : Int,
)
node : (a : &node, b : &node, value : Int,)
//          the last coma is optional ---^
// you can also give defaults, even partials, for structs
another_node : node = (_, _, 0)              // <-- partial defaults
another_node : node = (value := 0, ..)       // <-- same as ---^

// --- Enums
// they let you group related values together, but unlike structs, they are a way to define that something is one of a set of other values.
// To differenciate their one-of-a-set behaviour, we declare them using the "|>" pipe/matching symbol and with all caps.
my_enum : (
	|> Zero := 0,                      // untypped but infered
	|> Num : (I32),                    // typed
	|> Str : (String, String),         // typed w/ tuple
	|> Other : (a: String, b: f32),    // typed w/ struct
)
// if you do not want a u64 as the default index size you can do as follows
another_enum : (|> Zero, |> One, |> Two) : U8
//             enum with "U8" as index ----^

// --- Types
// every piece of data has a data type. By default, there are only a small set of data types defined, but custom types can be defined as "name :: type"
// this is how "int" and "Float" are defined
Int:: F32
Float :: F64
// you can define new type for a struct with the following syntax
Node :: (
	owned_a : &Node,
	owned_b : &Node,
	value : Int,
) = (value := 0, ..)                                  // <-- with defaults!
// and enums like so
AnotherEnum :: (|> Zero, |> One, |> Two) : U8 = One   // <-- with defaults
// referencing an already defined value-type ----^

// --- Functions & Lambdas
// there is no distiction between them, a "function" is a named lambda
// types do not need to be explicitly defined because they can be infered
// the format to define a function is as follows
// "name := (parameters) -> return <capture> { code }"
// this format gives us the following four possibilities
// 						 		    { code } <-- Anonymous code block
//					      <capture> { code } <-- Captured code block
//      (i: int) -> Float <capture> { code } <-- Anonymous function aka a lambda
// f := (i: int) -> Float <capture> { code } <-- Named function
// for example:
// w/ 0 arguments and return value
five := () -> Int { 5 }
// w/ unnamed arguments and return value
square := (Float) -> Float { $0 * $0 }
// w/ arguments but no return. 
// empty returns are optional, but code bodies are mandatory even if empty
nothing := (a:Float) { }
// w/ untyped arguments they get mono-morphizied at compile-time
// we must make explicit that we are using the global print function
//                    v----- explicit capture of global print function
print := (a) -> () <print> { print "{a}" }
//              ^---- explicit empty return value 
// to enforce a possible callee to the same capture restrictions as the defined code block you can use the double "<<>>" like so "<<capture>> { code }"
print := (a) -> () <<print>> { print "{a}" }
//                     ^--- redundant since print is available to all scopes

// --- Generics
// the type is determined by the GENERIC_TYPE preceded by the "$" symbol
sum := (a: $T, b: T) -> T { a + b }
// e.g. calling the code above with "sum(Int, Int)", results in  "T = Int".
// failing to provide a GENERIC_TYPE preceeded by a "$" is an error

// --- Results & Options
// the type Result signals that a value could produce an error instead of the desired output. It is defined as 
Result :: (
	|> Ok : $T,
	|> Err : $E,
)
// the type Option signals that a value could be NULL instead of the desired output. It is defined as 
Option :: (
	|> None,
	|> Some : $T,
)

```
## Others Ideas

```cpp
// function calls have a similar "dot notation" as koka
// function with nammed arguments
multiply : (x: Float, y: Float) -> Float = x * y
(3.0,5.0).multiply               // calling with a tuple (unnamed members)
(a:=3.0,b:=5.0).multiply         // calling with a struct (nammed members)

// this is how you chain multiple functions
(3.0,5.0).multiply.print         // the payload is in front, similar to koka

// loops 
// for-loops can be done with the "each" function
0..10.each( i -> "{i}".print)

(0..10, i -> "{i}".print).each

each(0..10, i -> print "{i}")
// 

// advanced function

```


