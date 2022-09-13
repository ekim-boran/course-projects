# Assignment 2: Random Art (160 points)

## Due by Wednesday, February 10th, 2021, 23:59:59

## Overview

The objective of this assignment is for you to have fun learning
about recursion, recursive datatypes, and make some pretty
cool pictures. All the problems require relatively little
code: somewhere between 2 to 10 lines. If any function requires
more than that, you should rethink your solution.


The assignment is in the files:

1. [src/TailRecursion.hs](/src/TailRecursion.hs) and
   [src/RandomArt.hs](/src/RandomArt.hs) has skeleton
   functions with missing bodies that you will fill in,

2. [tests/Test.hs](/tests/Test.hs) has some sample tests
   and testing code that you will use to check your
   assignments before submitting.

You should only need to modify the parts of the files which say:

```haskell
error "TBD:..."
```

with suitable Haskell implementations.

## Assignment Testing and Evaluation

Your functions/programs **must** compile and run on `ieng6.ucsd.edu`.

Most of the points will be awarded automatically by
**evaluating your functions against a given test suite**.

[Tests.hs](/tests/Test.hs) contains a very small suite
of tests which should give you a flavor of the sorts of tests
your assignment will be graded against.
When you run

```shell
$ make test
```

Your last lines should have

```
All N tests passed (...)
OVERALL SCORE = ... / ...
```

**or**

```
K out of N tests failed
OVERALL SCORE = ... / ...
```

**If your output does not have one of the above your code will receive a zero**

If, for some problem, you cannot get the code to compile,
leave it as in the starter code (with `error ...`), with your partial
solution enclosed below as a comment.

The other lines will give you a readout for each test.
You are encouraged to try to understand the testing code,
but you will not be graded on this.

## Submission Instructions

To submit your code, just do:

```bash
$ make turnin
```

## Problem #1: Tail Recursion

We say that a function is *tail recursive*
if every recursive call is a [tail call](https://wiki.haskell.org/Tail_recursion)
whose value is immediately returned by the procedure.

### (a) 15 points

Without using any built-in functions, write a
*tail-recursive* function

```haskell
assoc :: Int -> String -> [(String, Int)] -> Int
```

such that

```haskell
assoc def key [(k1,v1), (k2,v2), (k3,v3);...])
```

searches the list for the first i such that `ki` = `key`.
If such a ki is found, then vi is returned.
Otherwise, if no such ki exists in the list,
the default value `def` is returned.

Once you have implemented the function, you
should get the following behavior:

```haskell
ghci> assoc 0 "william" [("ranjit", 85), ("william",23), ("moose",44)])
23

ghci> assoc 0 "bob" [("ranjit",85), ("william",23), ("moose",44)]
0
```

### (b) 15 points

Use the library function `elem` to **modify the skeleton** for
`removeDuplicates` to obtain a function of type

```haskell
removeDuplicates :: [Int] -> [Int]
```

such that `removeDuplicates xs` returns the list
of elements of `xs` with the duplicates, i.e.
second, third, etc. occurrences, removed, and
where the remaining elements appear in the same
order as in `xs`.

Once you have implemented the function, you
should get the following behavior:

```haskell
ghci> removeDuplicates [1,6,2,4,12,2,13,12,6,9,13]
[1,6,2,4,12,13,9]
```

### (c) 20 points

Without using any built-in functions, write a
tail-recursive function:

```haskell
wwhile :: (a -> (Bool, a)) -> a -> a
```

such that `wwhile f x` returns `x'` where there exist values
`v_0`,...,`v_n` such that

- `x` is equal to `v_0`
- `x'` is equal to `v_n`
- for each `i` between `0` and `n-2`, we have `f v_i` equals `(true, v_i+1)`
- `f v_n-1` equals `(false, v_n)`.

Your function should be tail recursive.

Once you have implemented the function,
you should get the following behavior:

```haskell
ghci> let f x = let xx = x * x * x in (xx < 100, xx) in wwhile f 2
512
```

### (d) 20 points

Fill in the implementation of the function

```haskell
fixpointL :: (Int -> Int) -> Int -> [Int]
```

The expression  `fixpointL f x0` should return the list  
`[x_0, x_1, x_2, x_3, ... , x_n]` where

* `x = x_0`
* `f x_0 = x_1, f x_1 = x_2, f x_2 = x_3, ... f x_n = x_{n+1}`
* `x_n = x_{n+1}`

When you are done, you should see the following behavior:

```haskell
>>> fixpointL collatz 1
[1]
>>> fixpointL collatz 2
[2,1]
>>> fixpointL collatz 3
[3,10,5,16,8,4,2,1]
>>> fixpointL collatz 4
[4,2,1]
>>> fixpointL collatz 5
[5,16,8,4,2,1]

>>> fixpointL g 0
[0, 1000000, 540302, 857553, 654289,
793480,701369,763959,722102,750418,
731403,744238,735604,741425,737506,
740147,738369,739567,738760,739304,
738937,739184,739018,739130,739054,
739106,739071,739094,739079,739089,
739082,739087,739083,739086,739084,
739085]
```

The last one is because `cos 0.739085` is approximately `0.739085`.

### (e) 20 points

Without using any built-in functions,
**modify the skeleton** for `fixpointW`
to obtain a function

```haskell
fixpointW :: (Int -> Int) -> Int -> Int
```

such that `fixpointW f x` returns
**the last element** of the list
returned by `fixpointL f x`.

Once you have implemented the
function, you should get the
following behavior:

```haskell
ghci> fixpointW collatz 1
1

ghci> fixpointW collatz 2
1

ghci> fixpointW collatz 3
1

ghci> fixpointW collatz 4
1

ghci> fixpointW collatz 5
1

ghci> fixpointW g 0
739085
```


## Problem #2: Random Art

At the end of this assignment, you should be able to produce
pictures like those shown below. To do so, we shall:
1) devise a grammar for a certain class of expressions,
2) design a Haskell datatype whose values correspond to these expressions,
3) write code to evaluate the expressions, and then
4) write a function that randomly generates such expressions and plots them -- thereby
producing random psychedelic art.

**Color Images**

![](/img/color1.jpg)\ ![](/img/color2.jpg)\ ![](/img/color3.jpg)

**Gray Scale Images**

![](/img/art_g_sample.jpg)\ ![](/img/gray2.jpg)\ ![](/img/gray3.jpg)


The expressions are described by the grammar:

```
e ::= x
    | y
    | sin (pi*e)
    | cos (pi*e)
    | ((e + e)/2)
    | e * e
    | (e<e ? e : e)
```

where pi is the constant we all learned in grade school, rounded so that it
fits in a Double. All functions are over the variables
x,y, which are guaranteed to produce a value in the range [-1,1] when x and
y are in that range. We can represent expressions of this grammar
using values of the following datatype:

```haskell
data Expr
  = VarX                            -- ^ VarX ~ x
  | VarY                            -- ^ VarY ~ y
  | Sine    Expr                    -- ^ Sine e ~ sin (pi * e)  
  | Cosine  Expr                    -- ^ Cosine e ~ cos (pi * e)
  | Average Expr Expr               -- ^ Average e1 e2 ~ (e1 + e2) / 2
  | Times   Expr Expr               -- ^ Times e1 e2 ~ e1 * e2 
  | Thresh  Expr Expr Expr Expr     -- ^ Thresh e1 e2 e2 e4 ~ (e1 < e2 ? e3 : e4)
```

### (a) 15 points

First, write a function

```haskell
exprToString :: Expr -> String
```

to enable the printing of expressions. Once you have implemented
the function, you should get the following behavior:

```haskell
ghci> exprToString sampleExpr0
"sin(pi*((x+y)/2))"

ghci> exprToString sampleExpr1
"(x<y?x:sin(pi*x)*cos(pi*((x+y)/2)))"

ghci> exprToString sampleExpr2
"(x<y?sin(pi*x):cos(pi*y))"
```

### (b) 15 points

Next, write a function

```haskell
eval :: Double -> Double -> Expr -> Double
```

such that `eval x y e` returns the result of evaluating
the expression `e` at the point `(x, y)`. That is, evaluating
the result of `e` when `VarX` has the value `x` and `VarY` has
the value `y`.

* You should use functions from Prelude like
 `sin`, `cos`, and `pi` to build your evaluator.

* Recall that `Sine VarX` corresponds to
  the expression `sin(pi*x)`

Once you have implemented the function,
you should get the following behavior:

```haskell
ghci> eval  0.5 (-0.5) sampleExpr0
0.0

ghci> eval  0.3 0.3    sampleExpr0
0.8090169943749475

ghci> eval  0.5 0.2    sampleExpr2
0.8090169943749475
```

If you execute

```haskell
ghci> emitGray "sample.png" 150 sampleExpr3
```
(or just run `make` if your `ghci` is not working)
you should get a file `img/sample.png` in
your working directory. To receive full
credit, this image must look like the
leftmost grayscale image displayed above.
Note that this requires your `eval` to
work correctly. A message `Assert failure...` is an
indication that your `eval` is returning a value
outside the valid range `[-1.0,1.0]`.

### (c) 20 points

Next, consider the skeleton function:

```haskell
build :: Int -> Expr
build 0
  | r < 5     = VarX
  | otherwise = VarY
  where
    r         = rand 10
build d       = error "TBD:build"
```

Change and extend the function to generate interesting expressions `Expr`.

- A call to `rand n` will return a random number between (0..n-1)
  Use this function to randomly select operators when
  composing subexpressions to build up larger expressions.
  For example, in the above, at depth `0` we generate the expressions  
  `VarX` and `VarY` with equal probability.

- `depth` is a a maximum nesting dept; a random expression of depth `d` is
  built by randomly composing  sub-expressions of depth `d-1` and the
  only expressions of depth `0` are `VarX` and `VarY`.

With this in place you can generate random art using the functions

```haskell
emitRandomGray  :: Int -> (Int, Int) -> IO ()
emitRandomColor :: Int -> (Int, Int) -> IO ()
```

For example, running

```haskell
ghci> emitRandomGray 150 (3, 12)
```

will generate a gray image `img/grag_150_3_12.png` by:
randomly generating an `Expr`  

1. Whose  `depth` is equal to `3`,
2. Using the **seed** value of `12`.

Re-running the code with the same parameters will yield
the same image. (But different `seed` values will yield
different images).

The gray scale image, is built by mapping out a single
randomly generated expression over the plane.
The color image (generated by `emitRandomColor`) is built
by generating three `Expr` for the Red, Green and Blue
intensities of each pixel.

Play around with how you generate the expressions, using the
tips below.

- Depths of 8-12 produce interesting pictures, but play around!
- Make sure your expressions don't get *cut-off* early with
  `VarX`, `VarY` as small expressions give simple pictures.
- Play around to bias the generation towards more interesting operators.

Save the parameters (i.e. the depth and the seeds)
for your favorite three color images in the bodies of
`c1`, `c2`, `c3` respectively, and favorite three gray
images in `g1`, `g2` , `g3`.

### (d) 20 points

Finally, add **two** new operators to the grammar, i.e. to the
datatype, by introducing two new datatype constructors, and adding the
corresponding cases to `exprToString`, `eval`, and `build`.
The only requirements are that the operators must return
values in the range `[-1.0,1.0]` if their arguments (i.e. `VarX` and `VarY`)
are in that range, and that one of the operators take **three**
arguments, i.e. one of the datatype constructors is of the form:
`Ctor Expr Expr Expr`

You can include images generated with these new operators
when choosing your favorite images for part (c).
