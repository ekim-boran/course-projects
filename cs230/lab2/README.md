# [Lab 2] Data Lab

## Introduction

The purpose of this assignment is to become more familiar with bit-level representations of integers and floating point numbers. You’ll do this by solving a series of programming “puzzles.” Many of these puzzles are quite artificial, but you’ll find yourself thinking much more about bits in working your way through them.

## Handout Instructions

Fork this repository to your private namespace; make it private; and then clone it. See [this](https://cp-git.kaist.ac.kr/cs230/cs230#tools) for more details.

## Files you need to modify

The only file you will be modifying and turning in is `bits.c`.

The `bits.c` file contains a skeleton for each of the **14** programming puzzles. Your assignment is to complete each function skeleton using only straightline code for the integer puzzles (i.e., no loops or conditionals) and a limited number of C arithmetic and logical operators. Specifically, you are only allowed to use the following eight operators:

`!`  `̃` `&` `ˆ` `|` `+` `<<` `>>`

A few of the functions further restrict this list. Also, you are not allowed to use any constants longer than 8 bits. See the comments in `bits.c` for detailed rules and a discussion of the desired coding style.

## The Puzzles

This section describes the puzzles that you will be solving in `bits.c`.

### Bit Manipulations

Table below lists the normal bit manipulation puzzles. The “Rating” field gives the difficulty rating (the number of points) for the puzzle, and the “Max ops” field gives the maximum number of operators you are allowed to use to implement each function. See the comments in `bits.c` for more details on the desired behavior of the functions. You may also refer to the test functions in `tests.c`. These are used as reference functions to express the correct behavior of your functions, although they don’t satisfy the coding rules for your functions.

| Name | Description | Rating | Max Ops |
|--|--|--|--|
| `bitOr(x,y)` | `x \| y` using only `~` and `&` | 1 | 8 |
| `upperBits(n)` | pads `n` upper bits with 1's | 1 | 12 |
| `allEvenBits(x)` | returns 1 if all even-numbered bits in word set to 1 | 2 | 12 |
| `dividePower2(x,n)` | computes `x/(2^n)`, for 0 <= `n` <= 30 | 2 | 15 |
| `isPositive(x)` | returns 1 if `x` > 0, returns 0 otherwise | 2 | 8 |
| `conditional(x,y,z)` | same as `x ? y : z` | 3 | 16 |
| `isAsciiDigit(x)` | returns 1 if `0x30 <= x <= 0x39` | 3 | 15 |
| `replaceByte(x,n,c)` | replaces byte `n` in `x` with `c` | 3 | 10 |
| `increment(x)` | computes `x+1` without using `+` and `~` | 4 | 30 |
| `logicalNeg(x)` | implements the `!` operator without using `!` | 4 | 12 |

### Unsigned Operations

The following puzzles involve unsigned integers, and for these puzzles you are allowed to use `unsigned` data types.

| Name | Description | Rating | Max Ops |
|--|--|--|--|
| `isUmax(x)` | returns 1 if `x` is the maximum unsigned integer | 1 | 4 |
| `unsignedSatAdd(x,y)` | adds two numbers but when overflow occurs, returns maximum possible value | 4 | 25 |

### Floating-Point Operations

For the floating point puzzles, you will implement some common single-precision floating-point operations. You are allowed to use standard control structures (conditionals, loops), and you may use both `int` and `unsigned` data types, including arbitrary unsigned and integer constants. You may not use any unions, structs, or arrays. Most significantly, you may not use any floating point data types, operations, or constants. Instead, any floating-point operand will be passed to the function as having type `unsigned`, and any returned floating-point value will be of type `unsigned`. Your code should perform the bit manipulations that implement the specified floating point operations.

| Name | Description | Rating | Max Ops |
|--|--|--|--|
| `floatisEqual(uf,ug)` | computes `f == g` for floating point arguments `f` and `g` | 2 | 25 |
| `floatUnsigned2Float(u)` | returns bit-level equivalent of expression `(float) u` | 4 | 30 |

value `f` is the floating-point number having the same bit representation as the unsigned integer `uf`.

## Evaluation

Your score will be computed out of a maximum of 64 points based on the following distribution:

* **36** Correctness points.
* **28** Performance points.
* **Style**. Submitted code must be formatted according to the llvm style. Otherwise, you will get 0 points. **Don't forget to format your code using `make format` before submission.**

### Correctness points

The 14 puzzles you must solve have been given a difficulty rating between 1 and 4, such that their weighted sum totals to 36. We will evaluate your functions using the `btest` program, which is described in the next section. You will get full credit for a puzzle if it passes all of the tests performed by btest, and no credit otherwise.

#### Undefined behavior

If the code involves undefined behaviors, there is no guarantee about what happens after running the code. For example, the return value may depend on the compiler and the operating environment. Therefore, it is best to assume that **any C code with undefined behaviors is incorrect** and avoid them as much as possible.

Two undefined behvaiors that can usually happen in this lab are [signed integer overflow](https://wiki.sei.cmu.edu/confluence/display/c/INT32-C.+Ensure+that+operations+on+signed+integers+do+not+result+in+overflow) and [shifting by the amount that is not permitted by the operation](https://wiki.sei.cmu.edu/confluence/display/c/INT34-C.+Do+not+shift+an+expression+by+a+negative+number+of+bits+or+by+greater+than+or+equal+to+the+number+of+bits+that+exist+in+the+operand).

### Performance points

Our main concern at this point in the course is that you can get the right answer. However, we want to instill in you a sense of keeping things as short and simple as you can. Furthermore, some of the puzzles can be solved by brute force, but we want you to be more clever. Thus, for each function we’ve established a maximum number of operators that you are allowed to use for each function. This limit is very generous and is designed only to catch egregiously inefficient solutions. You will receive two points for each correct function that satisfies the operator limit.

### Autograding

We have included some autograding tools in the handout directory — `btest`, `dlc` to help you check the correctness of your work.

Your solution will be tested on a same Linux machine that you were provided with, with `make grade`:

```bash
unix> make grade
```

This grades your solution using `btest` and `dlc` to compute the correctness and performance points for your solution.
Also make sure your `bits.c` works with the original files.

#### `btest`

This program checks the functional correctness of the functions in `bits.c`. To build and use it, type the following two commands:

```bash
unix> make
unix> ./btest
```

Notice that you must rebuild `btest` each time you modify your `bits.c` file. You’ll find it helpful to work through the functions one at a time, testing each one as you go. You can use the -f flag to instruct btest to test only a single function:

```bash
unix> ./btest -f bitOr
```

You can feed it specific function arguments using the option flags -1, -2, and -3:

```bash
unix> ./btest -f bitOr -1 7 -2 0xf
```

Type `./btest -h` for a list of command line options.

#### `dlc`

This is a modified version of an ANSI C compiler from the MIT CILK group that you can use to check for compliance with the coding rules for each puzzle. The typical usage is:

```bash
unix> ./dlc bits.c
```

The program runs silently unless it detects a problem, such as an illegal operator, too many operators, or non-straightline code in the integer puzzles. Running with the -e switch:

```bash
unix> ./dlc -e bits.c
```

causes dlc to print counts of the number of operators used by each function. Type `./dlc -help` for a list of command line options.

## Helper Programs

We have included the `ishow` and `fshow` programs to help you decipher integer and floating point representations respectively. Each takes a single decimal or hex number as an argument. To build them, type

```bash
unix> make
```

Example usages:

```bash
unix> ./ishow 0x27

Hex = 0x00000027, Signed = 39, Unsigned = 39



unix> ./ishow 27

Hex = 0x0000001b, Signed = 27, Unsigned = 27



unix> ./fshow 0x15213243

Floating point value 3.255334057e-26

Bit Representation 0x15213243, sign = 0, exponent = 0x2a, fraction = 0x213243

Normalized. +1.2593463659 X 2^(-85)



unix> ./fshow 15213243

Floating point value 2.131829405e-38

Bit Representation 0x00e822bb, sign = 0, exponent = 0x01, fraction = 0x6822bb

Normalized. +1.8135598898 X 2^(-126)
```

## Handin Instructions

Go to [the submission website](https://gg.kaist.ac.kr/assignment/27/) and submit your `bits.c`.
See [this](https://cp-git.kaist.ac.kr/cs230/cs230#handin-instructions) for more details.

## Advice

* Don’t include the `<stdio.h>` header file in your `bits.c` file, as it confuses dlc and results in some non-intuitive error messages. You will still be able to use printf in your bits.c file for debugging without including the `<stdio.h>` header, although gcc will print a warning that you can ignore.
* The `dlc` program enforces a stricter form of C declarations than is the case for C++ or that is enforced by gcc. In particular, any declaration must appear in a block (what you enclose in curly braces) before any statement that is not a declaration. For example, it will complain about the following code:

```c
int foo(int x)
{
  int a = x;
  a *= 3; /* Statement that is not a declaration */
  int b = a;   /* ERROR: Declaration not allowed here */
}
```
