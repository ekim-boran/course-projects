# Assignment 5: Type Classes (255 points)

## Due by Sunday 2020-03-15, 23:59:59PM

## Overview

The overall objective of this assignment is to get some experience 
with different *type-classes*, in particular to learn about:

* Property-based testing, and
* Monads.

by modifying and extending your nano-interpreter.

The assignment is in the files:

- [BST.hs](/src/Data/BST.hs)
- [Eval.hs](/src/Language/Nano/Eval.hs)
- [Repl.hs](/src/Language/Nano/Repl.hs)
- [Main.hs](/src/Main.hs)
- [tests/Test.hs](/tests/Test.hs)

As before `Test.hs` has some sample tests, and testing code that
you will use to check your assignments before submitting.

You should only need to modify the parts of the files which say:

```haskell
error "TBD: ..."
```

with suitable Haskell implementations.

## Assignment Testing and Evaluation

Your functions/programs **must** compile and run on `ieng6.ucsd.edu`.

Most of the points, will be awarded automatically, by
**evaluating your functions against a given test suite**.

[Tests.hs](/tests/Test.hs) contains a very small suite
of tests which gives you a flavor of of these tests.
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

If for some problem, you cannot get the code to compile,
leave it as is with the `error ...` with your partial
solution enclosed below as a comment.

The other lines will give you a readout for each test.
You are encouraged to try to understand the testing code,
but you will not be graded on this.

## Submission Instructions

To submit your code, just do:

```bash
$ make turnin
```

## Problem 1: Sets via Binary Search Trees

For this problem, you will use Haskell's data types to
implement an _Abstract Set_ datatype that implements 
the following API:

```haskell
-- | The Set data type
data Set a

-- | `contains x ys` returns `True` if and only if `x` is a member of the set `ys`
contains :: (Ord a) => a -> Set a -> Bool

-- | `add x xs` returns the (new) set obtained by inserting `x` into the set `xs`
add :: (Ord a) => a -> Set a -> Set a

-- | `remove x xs` returns the (new) set obtained by deleting `x` from the set `xs`
remove :: (Ord a) => a -> Set a -> Set a
```

### Data Type

The sets will be represented as *Binary Search Trees* that support
efficient addition and removal of elements. We represent the 
datatype as:

```haskell
data BST a
  = Leaf                      -- ^ empty tree
  | Node a (BST a) (BST a)    -- ^ node with left and right subtrees
  deriving (Show)
```

Thus, values of type `BST a` are of the form

- `Leaf` or
- `Node e l r` where e is an element of type `a` and `l` and `r` are left and right subtrees of type `BST a`.

### The `isOrdered` Invariant

We will only use trees that are **Binary-Search Ordered**,
which is to say, trees `t` such that `isOrdered t` evaluates
to `True`. Make sure you understand what the `isOrdered`
function is doing!

### (10 points): Build

Fill in the definition of `build` that converts the supplied `[a]`
into a `BST a` by recursively splitting the list up into sub-lists,
converting the sub-lists to trees. Make sure that the resulting
`BST a` is binary-search-ordered. When you are done you should
get the following behavior:

```haskell
λ> quickCheck prop_build
+++ OK, passed 100 tests.
```

### (10 points): Contains

Next, fill in the definition of the function

```haskell
contains :: (Ord a) => a -> BST a -> Bool
```

so that `contains x t` evaluates to `True` iff the element `x`
is in the tree `t`. When you are done, you should see the
following behavior:

```haskell
λ> t2
Node 5 Leaf (Node 20 (Node 10 Leaf Leaf) (Node 30 Leaf Leaf))

λ> [ contains x t2 | x <- [5,6,10,11,20,21,30,31] ]
[True,False,True,False,True,False,True,False]
```

Writing these tests can be tedious, instead we will check properties!
The property `prop_contains_elt` states that trees created by `build xs`
`contain` those elements `x` that are are in the input list `xs`:

```haskell
λ> quickCheck prop_contains_elt
+++ OK, passed 100 tests.
```

### (15 points): Fold

Now, write a `fold` function that performs an *in-order traversal*
of the tree, accumulating the results as it goes. 

```haskell
fold :: (b -> a -> b) -> b -> BST a -> b
```

Why is there no `Ord` in the type for `fold`?

When you are done, various bonus properties get unlocked thanks
to the `toList` function that we have supplied that uses `fold`.
In particular, you should get the following behavior

```haskell
λ> toList t2
[5,10,20,30]

λ> toString t2
"build [5,10,20,30]"

λ> build [5,10,20,30]
Node 5 Leaf (Node 10 Leaf (Node 20 Leaf (Node 30 Leaf Leaf)))
```

You should also check the property that the trees you `build`
actually contain *all* the elements from the source list.

```haskell
λ> quickCheck prop_contains_elts
+++ OK, passed 100 tests.
```


### (15 points): Add

Next, fill in the definition for

```haskell
add :: (Ord a) => a -> BST a -> BST a
``` 

such that `add x t` returns a (new) BST which
has the same elements as `t` *plus* the new
element `x`. Of course, the new tree should
satisfy the binary-search-ordering invariant.

When you are done, you should see the following behavior

```haskell
λ> t2
Node 5 Leaf (Node 20 (Node 10 Leaf Leaf) (Node 30 Leaf Leaf))

λ> add 12 t2
Node 5 Leaf (Node 20 (Node 10 Leaf (Node 12 Leaf Leaf)) (Node 30 Leaf Leaf))

λ> let t2_new = add 12 t2

λ> isOrdered t2_new
True
```

Once again, we will check the *property* that the added elements are indeed in
the new set.
(Make sure you understand the properties!)

```haskell
λ> quickCheck prop_add_elt
+++ OK, passed 100 tests.

λ> quickCheck prop_add_elts_old
+++ OK, passed 100 tests.

λ> quickCheck prop_add_isOrd
+++ OK, passed 100 tests.
```

### (5 points): Debug the Property

We've seen a bunch of properties go sailing through. But consider this:

```haskell
λ> quickCheck prop_multiset
*** Failed! Falsifiable (after 6 tests):
[1,1]
```

Uh oh. Lets try to debug the code and property to see why the test fails.

You might see something different for ARG_0, i.e. some other list, but
nevertheless you will see some kind of list. Well, lets see why the
property failed, by running the test on the failing input that QuickCheck
has automatically found for us!

Let's run the property on the _failing input_. How? Well the property is
just a function

```haskell
prop_multiset :: [Int] -> Bool
prop_multiset xs = toList (build xs) == L.sort xs
```

so you can run it, by calling that function with the failing input:

```haskell
λ> prop_multiset [1,1]
False
```

(Of course, your failing input may be slightly different,
because QuickCheck finds them by _random sampling_.)

The property says for any list `xs` the result of building
a `BST` from xs and converting it to a list, should be
identical to just `sort`ing the list `xs`.

Lets see if that equality really is true, for the particular 
`xs` that we have above. The left hand side is:

```haskell
λ> toList (build [1,1])
[1]
```

and the right hand side is:

```haskell
λ> :m +Data.List
λ> sort [1,1]
[1,1]
```

whoops! The `BST` does not keep duplicates (no duplicates on the LHS),
but vanilla sorting does keep duplicates! So the property is broken,
we want to check only that the `BST` has all the elements from `xs`
but _excluding any duplicates_.

Fix the property so that it passes. Do this in a meaningful way --
don't just replace it with `True`.

**Note:** Use the above strategy of running the test on the failing input to
fix your code when there are _other_ failing tests too.

### (10 points): Remove Minimum

We also want to remove elements from the set. As a first step,
write a function

```haskell
removeMin :: (Ord a) => BST a -> (a, BST a)
```

that returns a tuple of the _minimum element_ in the tree,
and the tree containing all elements _except_ the minimum.
When you are done you should see this behavior

```haskell
λ> removeMin t2
(5,Node 20 (Node 10 Leaf Leaf) (Node 30 Leaf Leaf))

λ> quickCheck prop_remove_min
+++ OK, passed 100 tests.
```

`removeMin` should throw an error if given an empty tree.

### (20 points): Remove

Finally, use `removeMin` to fill in the definition of

```haskell
remove :: (Ord a) => a -> BST a -> BST a
```

such that `remove x t` returns the tree containing all
elements _except_ x. Of course, the new set should
satisfy the binary-search-ordering property.

`remove x t` should return `t` unchanged if `x` is not
in `t`.

When you are done, you should see the following behavior.

```haskell
λ> remove 12 t2
Node 5 Leaf (Node 20 (Node 10 Leaf Leaf) (Node 30 Leaf Leaf))

λ> remove 20 t2
Node 5 Leaf (Node 30 (Node 10 Leaf Leaf) Leaf)

λ> quickCheck prop_remove
+++ OK, passed 100 tests.

λ> quickCheck prop_remove_old
+++ OK, passed 100 tests.

λ> quickCheck prop_remove_isOrd
+++ OK, passed 100 tests.
```

## Problem 2: Exceptions

Next, we will add _exceptions_ to the `nano` language, in several steps.


### Exceptional Expressions

We have extended the representation of _Expressions_ by adding two new constructs:

```haskell
data Expr
  = ...
  | EThr Expr           -- ^ throw e
  | ETry Expr Id Expr   -- ^ try e1 catch z => e2
```

### Exceptional Values

We will represent the _result_ of evaluation via the type:

```haskell
data Either a b = Left a | Right b
```

The key idea is that when we evaluate an expression `e` we get:

1. `Left exn` when `e` "throws" an (uncaught) exception;
2. `Right val` when `e` "finishes" normally, without an exception.


To do so, note that the _top-level_ `eval` function is defined as:

```haskell
eval :: Env -> Expr -> Value
eval env e = case evalE env e of
  Left exn  -> exn
  Right val -> val
```

The helper function `evalE` has the type:

```haskell
evalE :: Env -> Expr -> Either Value Value
```

### (55 points): Copy and Update

First, copy and update your old code from

- `Lexer.x`
- `Parser.y`
- `Eval.hs`

`Lexer.x` can be copied entirely. For `Parser.y`, only copy the
parts you wrote - some of the other starter code has changed.
For `Eval.hs`, copy `evalOp`, `lookupId`, and `prelude`, and
adapt your `eval` implementation to become the _first 9_ cases
of `evalE`.

```haskell
evalE :: Env -> Expr -> Either Value Value
evalE env (EInt i)       = error "TBD"
evalE env (EBool b)      = error "TBD"
evalE env (EVar x)       = error "TBD"
evalE env (EBin o e1 e2) = error "TBD"
evalE env (EIf c t e)    = error "TBD"
evalE env (ELet x e1 e2) = error "TBD"
evalE env (EApp e1 e2)   = error "TBD"
evalE env (ELam x e)     = error "TBD"
evalE env ENil           = error "TBD"
```

When you are done, your old tests should pass, that is the command


```
$ stack test --test-arguments "-p Eval" --allow-different-user
...
OVERALL SCORE = 55 / 55
```

### (30 points): Throw

Next, complete the implementation of

```haskell
evalE env (EThr e)       = error "TBD"
```

When you are done, you should see the following behavior:

```haskell
λ> eval [] (EBin Plus (EInt 1) (EInt 2))
3
λ> eval [] (EBin Plus (EThr (EInt 1)) (EInt 2))
1
λ> eval [] (EBin Plus (EInt 1) (EThr (EInt 2)))
2
λ> eval [] (EBin Plus (EThr (EInt 1)) (EThr (EInt 2)))
1
λ> eval [] (EThr (EBin Plus (EInt 1) (EInt 2)))
3
λ> eval [] (EThr (EBin Plus (EInt 1) (EThr (EInt 2))))
2
```

(You will have to modify the other evalE cases too to get
this to work.)

### (30 points): Catch

Finally, complete the implementation of

```haskell
evalE env (ETry e1 x e2) = error "TBD"
```

when you are done, you should see the following behavior:

```haskell
λ> let tryZ e = ETry e "z" (EBin Plus "z" (EInt 10))

λ> eval [] (tryZ (EBin Plus (EInt 1) (EInt 2)))
3
λ> eval [] (tryZ (EBin Plus (EThr (EInt 1)) (EInt 2)))
11
λ> eval [] (tryZ (EBin Plus (EInt 1) (EThr (EInt 2))))
12
λ> eval [] (tryZ (EBin Plus (EThr (EInt 1)) (EThr (EInt 2))))
11
λ> eval [] (tryZ (EThr (EBin Plus (EInt 1) (EInt 2))))
13
λ> eval [] (tryZ (EThr (EBin Plus (EInt 1) (EThr (EInt 2)))))
12
```

## Problem 3: Nano REPL

For this problem, you will get some experience building
a _standalone "app"_ in Haskell, by implementing a "shell" 
for `nano`, that will let you:

- `quit` :-)
- `run` a file,
- `eval` an expression passed in as a `String`
- `load` a file and then evaluate expressions 
   that can refer to the top-level definitions of the file.

**This time, we're giving you almost _no_ scaffolding.**
Your task is simply to implement the code in `src/Main.hs`,
specifically, to fill in the implementation of the function

```haskell
main :: IO ()
main = error "TBD:main"
```

which is the top-level `IO` _recipe_ that Haskell runs as an "app".

However, there are lots of _very useful_ functions in

- [Repl.hs](/src/Language/Nano/Repl.hs)

that we suggest you understand (and complete the implementation of where necessary.)

### (5 points): Quit

First, hook up your shell so that it starts up thus:

```sh
$ make repl
...
------------------------------------------------------------
-------- The NANO Interpreter v.0.0.0.0 --------------------
------------------------------------------------------------

λ [0]
```

The `[0]` is a "prompt" at which the user can type a command.

Specifically, if the user types `:quit` then the shell should exit!

```sh
------------------------------------------------------------
-------- The NANO Interpreter v.0.0.0.0 --------------------
------------------------------------------------------------

λ [0] :quit
Goodbye.
```

**HINT:** The function `doQuit` may be useful.

### (15 points): Evaluating Expressions

Next, extend your REPL so that the user can (repeatedly)
enter expressions that then get parsed and evaluated, with
the results printed back.

**Don't worry about parsing `handle` and `try-catch` -- just the older constructs.**

When you are done you should see the following behavior:

```sh
rjhala@borscht ~/t/1/a/05-classes (master)> make repl
...
------------------------------------------------------------
-------- The NANO Interpreter v.0.0.0.0 --------------------
------------------------------------------------------------

λ [0] 2 + 3
5
λ [1] let x = 2 in x + 3
5
λ [2] let add x y = x + y in ((add 10) 20)
30
λ [3] quit
unbound variable: quit
λ [4] :quit
Goodbye.
```

**HINT:** The function `doEval` may be useful.

### (10 points): Running Files

Next, extend your REPL so that you can `run` a particular file, that is,

- read the file,
- parse its contents into an `Expr`
- evaluate the expr to get a `Value`.

When you are done, you should see the following behavior

```sh
$ make repl
...
------------------------------------------------------------
-------- The NANO Interpreter v.0.0.0.0 --------------------
------------------------------------------------------------

λ [0] :run tests/input/t1.hs
45
λ [1] :run tests/input/t2.hs
0
λ [2] :run tests/input/t3.hs
2
λ [3] :quit
Goodbye.
```

**HINT:** The function `doRun` may be useful.

### (25 points): Loading Files

Finally, extend the REPL so that you can `load` a file's definitions 
into the environment, and then write expressions that refer to those
expressions. For example, consider the file: `tests/input/tAdd.hs`
which has two definitions:

```haskell
add =
  let add1 x y = x + y in
      add1
,

sub = 
  let sub1 x y = add x (0 - y) in
      sub1
```

You should be able to do:

```sh
$ make repl
...
------------------------------------------------------------
-------- The NANO Interpreter v.0.0.0.0 --------------------
------------------------------------------------------------

λ [0] :load tests/input/tAdd.hs
definitions: tail head add sub
λ [1] ((sub ((add 10) 20)) 5)
25
λ [2] :quit
Goodbye.
```

That is, typing `:load tests/input/tAdd.hs` adds the definitions
for `add` and `sub` into the top-level environment (which already
had `tail` and `head`). 

As with "evaluating expressions" you can then enter the expression

```haskell
((sub ((add 10) 20)) 5)
```

which should get evaluated and the result `25` is printed out.

**HINT:** You can assume that each `load` wipes out all previous
definitions, except those in `prelude`. The function `doLoad` may
be useful, but to use it you will have to complete the implementation
of `defsEnv`.
