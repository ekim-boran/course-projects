# Diamondback

In this assignment, you'll implement a compiler for a small language with
functions declarations and function calls, conforming to the C stack layout.
You'll also add some static well-formedness checks to the compiler.

This is the best I could do:

- **D**ouble-word
- **I**ntel **A**rchitecture
- **MO**stly dynamic
- **N**ested-expression
- (**D**iamondback supports recursion)
- **B**oolean-tagged
- **A**NF-transformed
- **C**ompiler,
- o**K**?


![A diamondback](https://upload.wikimedia.org/wikipedia/commons/d/d4/Crotalus_ruber_02.jpg)

-------

## Download 

1. Use the _link_ from the github classroom to create your private clone of the starter code.

2. Do `git clone https://github.com/ucsd-cse131/ucsd-cse131-sp21-04-diamondback-XYZ` where `XYZ` is your username.

3. Link your clone to the "upstream" to get any updates


```
$ make upstream
```

after this you can get "updates" (in case we modify the starter code), with 

```
$ make update 
```

## Submission Instructions

Make sure that your code works with the provided test cases by running
the command

``` sh
make test
```

To submit you must:

1. [Fill this form](https://forms.gle/mW9FcrUHRrAr4uUG9) with your information (you need to do this ONCE in the quarter)

2. Add the names of your group members in `COLLABORATORS.md` (leave blank if working individually),

3. `commit` and `push` your code by typing 

```bash
$ make turnin 
```

This will simply do a `git commit` followed by a `git push` to send us your code.

**We will use the _most recent commit_ of your code (on `main` branch) as your submission.**




## The Diamondback Language

As usual, we have concrete and abstract syntaxes, along with a specification
of semantics.

### Concrete Syntax

The major addition to Diamondback are _function declarations_.  Our programs
are now a sequence of zero or more function declarations, followed by a single
_main expression_.

```
<program> :=
  | <decls> <expr>
  | <expr>

<decls> :=
  | <decl>
  | <decl> <decls>

<decl> :=
  | def <identifier>(<ids>): <expr>
  | def <identifier>(): <expr>

<ids> :=
  | <identifier>
  | <identifier> , <ids>

<expr> :=
  | let <bindings> in <expr>
  | if <expr>: <expr> else: <expr>
  | <binop-expr>

<binop-expr> :=
  | <identifier>
  | <number>
  | true
  | false
  | add1(<expr>)
  | sub1(<expr>)
  | isNum(<expr>)
  | isBool(<expr>)
  | print(<expr>)
  | <identifier>(<exprs>)
  | <identifier>()
  | <expr> + <expr>
  | <expr> - <expr>
  | <expr> * <expr>
  | <expr> < <expr>
  | <expr> > <expr>
  | <expr> == <expr>
  | ( <expr> )

<exprs> :=
  | <expr>
  | <expr> , <exprs>

<bindings> :=
  | <identifier> = <expr>
  | <identifier> = <expr>, <bindings>
```

The other addition is **function applications** or **function calls**,
which are written `<identifier>(<exprs>)`, for example `f(1, 2, 3)`.  

### Abstract Syntax

As usual, we have a user-facing syntax and a compiler-facing syntax.

```haskell
-- lib/Language/Diamondback/Types.hs

-- | A Program is a list of declarations and "main" Expr
data Program a = Prog
  { pDecls :: [Decl a]      -- ^ function declarations
  , pBody  :: !(Expr a)     -- ^ "main" expression
  }

-- | Decl are function definitions
data Decl a = Decl
  { fName  :: !(Bind a)     -- ^ name of function
  , fArgs  :: [Bind a]      -- ^ names of parameters
  , fBody  :: !(Expr a)     -- ^ "body"/returned expression
  , fLabel :: a             -- ^ metadata
  }

-- | Expr are single expressions
data Expr a
  = Number  !Integer                       a  -- ^ integer constant
  | Boolean !Bool                          a  -- ^ boolean constant
  | Id      !Id                            a  -- ^ variable
  | Prim1   !Prim1    !(Expr a)            a  -- ^ unary prim-op
  | Prim2   !Prim2    !(Expr a)  !(Expr a) a  -- ^ binary prim-op
  | If      !(Expr a) !(Expr a)  !(Expr a) a  -- ^ conditional
  | Let     !(Bind a) !(Expr a)  !(Expr a) a  -- ^ let-binder
  | App     !Id       [Expr a]             a  -- ^ function call

-- | `Prim1` are unary operations
data Prim1
  = Add1
  | Sub1
  | Print
  | IsNum
  | IsBool

-- | `Prim2` are binary operations
data Prim2
  = Plus
  | Minus
  | Times
  | Less
  | Greater
  | Equal
```

As before we use `AnfExpr` and `ImmExpr` to describe ANF and immediate expressions. We use, `AnfDecl` and `AnfProgram` for declarations and programs that have been converted to ANF.

### Semantics

There are several distinguishing features of `diamondback`

+ **Function Applications** A function application should give
  the answer we'd get if we followed the rules for substituting
  argument values for parameter names. So, for example:

```python
def f(x, y):
  x + y

f(1, 2)
```

Should produce 3.

Your compiler should use the rules for C stacks discussed
in class and at [this assembly guide](http://www.cs.virginia.edu/~evans/cs216/guides/x86.html)
to implement this behavior.

There are a number of **new errors** that can occur now that
we have function declarations and calls.  Your implementation
should catch all of these cases **statically**; i.e. _before_
the program runs:

- A function application with the **wrong number of arguments**
  should signal an error containing the string `"arity"`
- A function application of a **non-existent function**
  should signal an error containing the string `"not defined"`
- An identifier **without a corresponding binding location**
  should report an error containing the string `"unbound"`
- A let binding that **redefines a variable already in scope**
  should report an error containing the string `"shadow binding"`
- A function declaration with **duplicate names in the argument list**
  should report an error containing the string `"duplicate parameter"`
- **Multiple function definitions** with the same name,
  should result in an error containing the string `"duplicate function"`
- A **numeric constant is too large** (as discussed in class),
  should result in an error containing the string `"too large"`

**Static Error Checking**  
These errors should stop the program from compiling,
_not_ happen at runtime.  You can continue to assume
that all identifiers within a function body have different
names, which is a requirement for ANF (we could implement
another pass to rename variables, but we won't do that here).  
See the notes on `well_formed` below for details on how to
implement these static checks.

### Implementation

You shouldn't need any new assembly instructions to
tackle this implementation.  You're free to add your
own new instructions.

There are a few new pieces to the implementation:

1. Static Error Checking `lib/Language/Diamondback/Checker.hs`
2. ANF Conversion `lib/Language/Diamondback/Normalizer.hs`
3. Assembly Generation `lib/Language/Diamondback/Compiler.hs`

#### 1. Static Error Checking

A set of `wellFormed` functions, defined in `Checker.hs`,
which are called prior to performing ANF and are used
to report the errors above.  

```haskell
wellFormed  :: BareProgram -> [UserError]
wellFormedD :: FunEnv -> BareDecl -> [UserError]
wellFormedE :: FunEnv -> Env -> Bare -> [UserError]
```

As before, `BareX` refers to the version of `X` (i.e. `Program`, `Decl` or `Expr`) where the meta-data field is simply `SourceSpan`.


These functions all return a `[UserError]` -- i.e. a list of
`UserError` that represent **all the errors** in the respective
program, declaration or expression. So a program like

```python
def f(x, x):
  y

f(1)
```

would report **three errors**, one for `y` being unbound, one for duplicated `x` parameters, and one for the arity mismatch on the
call to `f`. To ensure that the error-messages are sensible, feel
free to use the supplied constructors -- e.g. `errUnboundVar`, `errDupParam` and `errCallArity`. Your task is to ensure that you
invoke the constructors with the correct parameters, found while
traversing the body of the program, declaration or expression, respectively.

These errors **can be reported in any order**;
in general (and in grading), it's easy to test
for one at a time. Reporting many makes using
the `main` of the compiler much more pleasant,
and is a nice view into the kinds of compiler
ergonomics we should expect from a modern compiler.


#### 2. ANF Conversion

You will have to extend the ANF conversion to account for function
calls, by filling in the definitions for:

```haskell
anf i (App f es l)      = error "fill this in"

imm i (App f es l)      = error "fill this in"
```

which convert a function call expression into **ANF** and **immediate** form, respectively.

#### 3. Compiling Definitions and Calls

Your third major task is to implement the compilation
of programs. When you are done, you should be generating
assembly that looks like:

```nasm
  ;; extern and global stuff
fun_decl1:
  ;; code for fun_decl1, including stack management
fun_decl2:
  ;; code for fun_decl2, including stack management
...
our_code_starts_here:
  ;; main entrypoint, as before, with stack management
internal_error_non_number:
  ;; errors, as before
...
```

To do so, complete the definitions of the functions below.

First, the `APgm`, `ADcl` and `AExp` types  
respectively contain ANF- and tagged- versions of
the top-level program, declaration and expressions.
Fill in code to generate `[Instruction]` for each of the above.

```haskell
-- lib/Language/Diamondback/Compiler.hs

compile :: APgm -> [Instruction]
compile (Prog ds e) = error "fill this in"
```

`compileDecl f xs body` which returns the instructions of `body` of 
a function `f` with parameters `xs` wrapped with code that sets up 
the stack (by allocating space for n local vars) and restores the 
callee's stack prior to return.

```haskell
-- lib/Language/Diamondback/Compiler.hs

compileDecl :: Bind a -> [Bind a] -> AExp -> [Instruction]
compileDecl f xs body = 
    [ ILabel (DefFun (bindId f)) ]
 ++ funEntry n                            -- 1. setup  stack frame RBP/RSP
 ++ [ ILabel (DefFunBody (bindId f)) ] 
 ++ copyArgs xs                           -- 2. copy parameters into stack slots
 ++ compileEnv env body                   -- 3. execute 'body' with result in RAX
 ++ funExit n                             -- 4. teardown stack frame & return 
  where
    env = fromListEnv (zip (bindId <$> xs) [1..])
    n   = countVars body 
```


You will need to fill in the implementation of `funEntry`
and `funExit` which respectively generate the instructions
for setting up stack for `n` local vars and cleaning up
stack prior to returning, and for `copyArgs` which moves
the parameters from `rdi`, `rsi` and the stack `[RBP + 16]...`
to the relevant slots in the function's frame.

```haskell
-- lib/Language/Diamondback/Compiler.hs

funEntry :: Int -> [Instruction]
funEntry n = error "fill this in"

funExit :: [Instruction]
funExit = error "fill this in"
```

Finally, fill in the definition of

```haskell
-- lib/Language/Diamondback/Asm.hs

dynError   :: DynError -> [Instruction]
dynError e = error "fill this in"
```

to contain the labels and code for handling the
three different kinds of run-time errors, namely
type-errors and arithmetic overflow.

Don't forget to copy over your error-handling code 
from `03-cobra`!

```c
// c-bits/main.c
/*

Copy over any error-detection functions here

*/
```



## Tests (5-for-5)

* You can test the well-formedness errors using the
  error tester as usual.

* You **must** add 10 new tests in `yourTests` in order 
  to get **base credit**. Feel free to add more, we'll 
  just take the first 10.

* The creators of the **5 hardest tests** get **5%** 
  of total points as **extra credit**

* A test's "score" is the total number of compilers
  submitted by the entire class on which the test
  produces the **wrong** output.

* That is, the **harder** a test, the **higher** its 
  score.

## Interactive REPL 

```bash
$ stack ghci
```

and then you are inside `ghci` where you can use the function `run` to 
**rapidly** test your code.

```
λ> :t run
run :: FilePath -> Program -> IO Result
```

For example to directly compile and run a string do:

```
λ> run "" (Code "1 + 2") 
*** Exception: fill this in
```

To run a program whose code is in a file `tests/input/file.diamond` do:

```
λ> run "nyi" File
*** Exception: TBD:wellFormedE
```

Of course, neither works now. When you edit your code, instead of having to
rebuild, you can more rapidly type `:reload` in `ghci` and then re-run the 
above commands.


## Valgrind 

If you wish, use `valgrind` (installed in the lab) 
to help debug your code, for example thus:

```
$ make tests/output/nyi.vresult 
```

or in `ghci` as:

```
λ> vrun "" (Code "1 + 2") 
λ> vrun "nyi" File
```