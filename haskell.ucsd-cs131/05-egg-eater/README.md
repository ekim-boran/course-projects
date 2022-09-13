# Egg-Eater

![An adder](https://upload.wikimedia.org/wikipedia/commons/9/97/Dasypeltis_atra.jpg)

This assignment implements a compiler for the Egg-eater language, a small
language with functions, numbers, booleans, and _tuples_.  The name egg-eater
comes from the fact that tuple syntax with parentheses looks a little bit like
an egg, as long as you don't think about it too much.

```
(egg)
```

## Download 

1. Use the _link_ from the github classroom to create your private clone of the starter code.

2. Do `git clone https://github.com/ucsd-cse131/ucsd-cse131-sp21-05-egg-eater-XYZ` where `XYZ` is your username.

3. Link your clone to the "upstream" to get any updates


```
$ make upstream
```

after this you can get "updates" (in case we modify the starter code), with 

```
$ make update 
```

4. Save (and submit) your work with: 

```
$ git commit -a -m MESSAGE
$ git push
```






## Language

Egg-eater starts with the same semantics as Diamondback, and adds support for
tuples.

### Syntax Additions

The main addition in Egg-eater is

* **tuple** expressions,
* an **accessor** expression for getting the contents of tuples,
* a primitive for **checking** if a value is a tuple.  

Tuple expressions are a series of _two or more_ comma-separated
expressions enclosed in parentheses, e.g.

```python
(1, 2)

(1, (2, 3))
```

A tuple access expression is an expression,
followed by another expression enclosed in
square brackets, e.g.

```python
e[1]
```

Finally, `isTuple` is a primitive, like
`isNum` and `isBool` that checks for tuple-ness.

```
expr :=
    [ the same expressions as Diamondback ]
  | (<expr>, <expr>, <expr>, ...)
  | <expr>[<expr>]
  | isTuple(<expr>)
```

For example:

```python
let x = (3, 4, 5) in
x[0]
```

uses a tuple expression in the let binding,
and has a tuple access as the body.

In the `Expr` datatype, these are represented as:

```haskell
data Expr a
  = ...
  | Tuple   [Expr a]            a
  | GetItem !(Expr a) !(Expr a) a

data Prim1
  = ...
  | IsTuple
```

### Semantics and Representation of Tuples

#### Tuple Heap Layout

Tuples expressions should evaluate their sub-expressions 
in order, and store the resulting values on the heap. 
The layout for a tuple on the heap is:

```
  (8 bytes)    (8 bytes)  (8 bytes)          (8 bytes)
--------------------------------------------------------
| # elements | element_0 | element_1 | ... | element_n |
--------------------------------------------------------
```

That is, one word is used to store the **count** of
the number of elements in the tuple, and the subsequent
words are used to store the values themselves.

A **tuple value** is stored in variables and registers
as the address of the first word in the tuple's memory,
but with an additional `1` added to the value to act
as a tag.  So, for example, if the start address of the
above memory were `0x0adadad0`, the tuple value would
be `0x0adadad1`.  With this change, we extend the set
of tag bits to the following:

- **Numbers:** `0` in least significant bit
- **Booleans:** `111` in least three significant bits
- **Tuples:** `001` in least three significant bits

Visualized differently, the value layout is:

```
0xWWWWWWWWWWWWWWW[www0] - Number
0xFFFFFFFFFFFFFFF[1111] - True
0x7FFFFFFFFFFFFFF[1111] - False
0xWWWWWWWWWWWWWWW[w001] - Tuple
```

Where `W` is a "wildcard" nibble (4-bits) and `w` is a "wildcard" bit,
and the leftmost 32-bits are all `w`, i.e. `0` or `1`.

#### Accessing Tuple Contents

In a _tuple access_ expression, like

```python
(6, 7, 8, 9)[1 + 2]
```

The behavior should be:

1.  Evaluate the expression in tuple position
    (before the brackets), then the index
    expression (the one inside the brackets).
2.  Check that the tuple position's value is
    actually a tuple, and signal an error
    containing `"expected a tuple"` if not.
3.  Check that the index value is a number,
    and signal an error containing
    `"expected a number"` if not.
4.  Check that the index number is a valid
    index for the tuple value—that is, it
    is between `0` and the stored number
    of elements in the tuple minus one.  
    Signal an error containing `"index too small"`
    or `"index too large"` as appropriate.
5.  Evaluate to the tuple element at the specified index.

You _can_ do this with just `RAX`, but it is a bit tedious. 
Feel free to generate code that uses both `RAX` and `RBX`
in this case (for example saving the index in `RBX` and 
using `RAX` to store the address of the tuple).  

This can save a number of instructions.  

Note that we will generate code that doesn't
need to use `RCX` or `RAX` beyond the extent
of this one expression, so there is no need
to worry about saving or restoring the old
value from `RCX`.

You also may want to use an extended syntax
for `mov` in order to combine these
values for lookup.  For example, this kind
of arithmetic is allowed inside `mov` instructions:

```
  mov rax, [rbx + rcx * 8]
```

This would access the memory at the location
`rbx + rcx * 8`. So if the value in `rcx` were,
say `2`, this may be part of a scheme for
accessing the first element of a tuple
(there are other details you should think
through here; this is _not_ a complete solution.)

We have provided this as the new `Arg` value `RegIndex r i` which 
corresponds to the assembly `[r + i * 8]`.

Feel free to add additional `Arg` types in `Types.hs`
to support a broader range of `mov` instructions, if it helps.

Neither `RCX` nor anything beyond the typical 
`RegOffset` is _required_ to make this work, 
but you may find it interesting to try different 
shapes of generated instructions.

#### General Heap Layout

The register `R15` has been designated as the heap pointer.  

The provided `main.c` does a large `malloc` call, and passes 
in the resulting address as an argument to `our_code_starts_here`.

The support code provided fetches this value (as a traditional
argument), and stores it in `R15`. 

It also does a bit of arithmetic to make sure that `R15` 
starts at an 8-byte boundary – that is, the last three 
_bits_ of `ESI` are `000`.  

It is up to your code to ensure that:

- The value of `R15` always ends in `000`.  This ensures 
  that the beginning of each allocation happens at an 8-byte boundary, which means that we only need 61 bits of a 64-bit 
  word in order to store addresses. The least significant
  bits are then fair game for the tag.

- The value of `R15` is always the address of the next 
  block of free space (in increasing address order) in 
  the provided block of memory.

(Recall that if `R15` *starts* at a multiple of 8 and is only incremented by multiples of 8 you should be fine...)

#### Interaction with Existing Features

Any time we add a new feature to a language, we need to consider its
interactions with all the existing features.  In the case of Egg-eater, that
means considering:

- If expressions
- Function calls and definitions
- Tuples in binary and unary operators
- Let bindings

We'll take them one at a time.

- **If expressions**:  Since we've decided to only allow booleans in
  conditional position, we simply need to make sure our existing checks for
  boolean-tagged values in if continue to work for tuples.
- **Function calls and definitions**:  Tuple values behave just like other
  values when passed to and returned from functions – the tuple value is just
  a (tagged) address that takes up a single word.
- **Tuples in let bindings**:  As with function calls and returns, tuple values
  take up a single word and act just like other values in let bindings.
- **Tuples in binary operators**:  The arithmetic expressions should
  continue to only allow numbers, and signal errors on tuple values.  There is
  one binary operator that **does not check its types**, however: `==`.  We need to
  decide what the behavior of `==` is on two tuple values.  Note that we have a
  (rather important) choice here.  Clearly, this program should evaluate to
  `true`:

  ```
  let t = (4, 5) in t == t
  ```

  However, we need to decide if

  ```
  (4,5) == (4,5)
  ```

  should evaluate to `true` or `false`.  That is, do we check if the _tuple
  addresses_ are the same to determine equality, or if the _tuple contents_ are
  the same.  For this assignment, we'll take the somewhat simpler route and
  compare _addresses_ of tuples, so the second test should evaluate to `false`.
  (If you have extra time on this assignment, it's worth trying out the
  alternate implementation, where you check the tuple contents.  A useful hint
  is to write a two-argument function `equal` in `main.c` that handles this.
  There is no extra credit for this, just **extra learning**, which is immensely
  more valuable.)

- **Tuples in unary operators**: The behavior of the unary operators is
  straightforward, with the exception that we need to implement `print` for
  tuples.  We could just print the address, but that would be somewhat
  unsatisfying.  Instead, we should recursively print the tuple contents, so
  that the program

  ```
  print((4, (true, 3)))
  ```

  actually prints the string `"(4, (true, 3))"`.  This will require some
  careful work with pointers in `main.c`.  A useful hint is to create a
  recursive helper function for `print` that traverses the nested structure of
  tuples and prints single values.

## Approaching Reality

With the addition of tuples, Egg-eater is dangerously close to a useful
language.  Of course, it still puts no control on memory limits, doesn't have a
module system, and has other major holes.  However, since we have structured
data, we can now, for instance, implement a linked list.  We need to pick a
value to represent `empty` – `false` will do in a pinch.  Then we can write
`link`, which creates a pair of the first with the next link:

```
def link(first, rest):
  (first, rest)

let mylist = link(1, link(2, link(3, false))) in
  mylist[0]
```

Now we can write some list functions:

```
def length(l):
  if l == false: 0
  else:
    1 + length(l[1])
```

Try building on this idea, and writing up a basic list library.  Write at least
`sum`, to add up a numeric list, `append`, which concatenates two lists, and
`reverse`, which reverses a list.

Write more functions if you want, as well, and test them out.


## Recommended TODO List

1. Implement the `Tuple` and `GetItem` cases in ANF.
2. Get tuple creation and access working for tuples containing two elements,
   testing as you go.  This is very similar to the pairs code from lecture.
3. Modify the binary and unary operators to handle tuples appropriately (it may
   be useful to skip `print` at first). Test as you go.
4. Make tuple creation and access work for tuples of any size.  Test as you go.
5. Tackle `print` for tuples if you haven't already.  Test as you go.
6. Write some list library functions (at least the three above) to really
   stress your tuple implementation.  Rejoice in your implementation of the
   core features needed for nontrivial computation (Aside from the pesky issue
   of running out of memory.  More on that in lecture soon.).
7. If you want to try more, implement content-equality rather than
   address-equality for tuples.  And/or, try implementing something more
   ambitious than lists, like a binary search tree, in Egg-eater.  This last
   point is ungraded, but quite rewarding!

A note on support code – a lot is provided, but you can feel free to overwrite
it with your own implementation, if you prefer.


## Tests (5-for-5)

* You can test the well-formedness errors using the
  error tester as usual.

* You **must** add 5 new tests in `yourTests.json` in order
  to get **base credit**. Feel free to add more, we'll
  just take the first 5.

* The creators of the **5 hardest tests** get **5%**
  of total points as **extra credit**

* A test's "score" is the total number of compilers
  submitted by the entire class on which the test
  produces the **wrong** output.

* That is, the **harder** a test, the **higher** 
  its score.

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
```

To run a program whose code is in a file `tests/input/file.egg` do:

```
λ> run "file" File
```

When you edit your code, instead of having to
rebuild, you can more rapidly type `:reload` in
`ghci` and then re-run the above commands.

## Valgrind

If you wish, use `valgrind` (installed in the lab) to help debug your code, for example thus:

```
$ make tests/output/file.vresult
```
