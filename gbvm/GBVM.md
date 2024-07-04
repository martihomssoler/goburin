# GBVM (Goburin Virtual Machine)

> [!WARNING]
> **WORK IN PROGRESS**

Here you can find what I call "tiny goburin", a sub-set of the final language that has enough to be able to code
*real software* and get to be self-hosted. 

## Instruction and Data Set 

This compiler instruction and data set is split in two: the Core set, and the Platform set. The Core variant is 
guaranteed to compile to all target platforms, and the Platform variant is platform-specify set of instructions 
(which are implemented for most targets) but some might not support them (e.g. floats and memory management).

### Core Set

#### Data Types and Sizes

There are **3** data sizes: 
  * byte -> smallest chunk of workable memory, equals to 8 bits (I'm aware of 1/2/4 bit platforms, but 8 is the minimum).
  * word -> largest chunk of workable memory, equals to the platform-specific register size (usually 32 or 64).
  * addr -> largest addressable memory, equals to platform-specific pointer/address size (usually a word but it could be bigger).

For platforms where the word size is 8 bits, there is no real disctinction between word and byte.

There are **6** basic data types:
  * bool -> byte-sized boolean, either 0 (false) or 1 (true).
  * char -> byte-sized unsigned integer.
  * int -> word-sized signed integer.
  * ptr-bool -> addr-sized pointer to an integer.
  * ptr-char -> addr-sized pointer to a char.
  * ptr-int -> addr-sized pointer to an integer.

> [!NOTE]
> The idea to separate bools from chars is to make the instructions simple, allow for minimal "type-checking"
> and to make changing the size of a character from 8bits (ASCII compliant) to UNICODE compliant, dunno if even possible.

And there is only **1** composed data type: 
  * array T -> under the hood, the array is a (length + ptr) structure, where the array type T is the type pointed by the pointer.

Strings are just "array char". This way we have a "standarized" interface for them and implementations can change to
acomodate for C-strings.

#### Instructions

Here's how the instruction set might look in a simplified table format:

| Instruction  |  Operation         |  Description              |
|  ADD         |  v1 = ADD  v2, v3  |  Integer addition         |
|  SUB         |  v1 = SUB  v2, v3  |  Integer subtraction      |
|  MUL         |  v1 = MUL  v2, v3  |  Integer multiplication   |
|  DIV         |  v1 = DIV  v2, v3  |  Integer division         |
|  REM         |  v1 = REM  v2, v3  |  Integer remainder        |
|  AND         |  v1 = AND  v2, v3  |  Bitwise AND              |
|  OR          |  v1 = OR   v2, v3  |  Bitwise OR               |
|  XOR         |  v1 = XOR  v2, v3  |  Bitwise XOR              |
|  NOT         |  v1 = NOT  v2      |  Bitwise NOT              |
|  NEG         |  v1 = NEG  v2      |  Check if it is negative  |
|  LOAD        |  LOAD v1,  v2      |  Load data from memory    |
|  SAVE        |  SAVE v1,  v2      |  Store data to memory     |
|  JMP         |  JMP  v1           |  Unconditional jump       |
|  JMPZ        |  JPMZ v1           |  Conditional jump if zero |
|  CALL        |  CALL v1           |  Function call            |
|  RET         |  RET               |  Function return          |

There are **X** core instructions: 
 * 

### Platform Set
