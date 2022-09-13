{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
Sorted Lists
============

In this small excursion, we're going to define an abstract type of *sorted
lists*. A `SortedList a` is just an ordinary list of elements of type `a`, but
ordered according to the ordering on `a`. In order to prevent users from
constructing values of this type which violate this invariant, we're defining
the type and its operations in a separate module (this one) and only exposing
to other importing modules those functions which respect the invariants of the
type.

Abstract types in Haskell
-------------------------

The identifiers after the module name below define exactly what is exported by
this module.
-}

module HW3.SortedList
  ( SortedList, -- the abstract type (and its instances)
    singleton, -- other functions
    toList,
    fromList,
    minimum,
    numDistinct,
    count,
    foldr,
    length,
    filter,
    ten,
    twentyfour,
    whyNoFunctor,
    testListMonoid,
    testSortedList,
    testMinimum,
    testNumDistinct,
    testCount,
  )
where

import qualified Data.List as List
-- you can use List library functions, prefixed with `List.`

{-
In this module, we will define the `SortedList` type (see below), but
*not* export the data constructor (called `SL`) for this type (see export list above).

What that means is that within this module, we can use the data constructor to
make any `SortedList` we want, even a bad one. But outside this module, we
ensure that users can construct only sorted `SortedList`s by only providing
functions that are guaranteed to construct sorted lists.

For example, because the one-element
list is always sorted, we can safely expose the `singleton` function (see below).

What other operations should this module provide for constructing `SortedList`s?
We will need to provide more than just `singleton`.  Because clients of this module
don't have access to the `SL` data constructor, they won't be able create new
`SortedList`s out of regular lists. Therefore, they *only way* they will be able
to produce a `SortedList`s will be to use the operations provided here.

Let's focus on two operations in particular. We should provide a way to construct a
`SortedList` with zero elements and we should provide a way to construct a new `SortedList`
by combining two existing `SortedList`s together.

Many data structures have a general notion of "make an empty structure"
and "combine two structures together". As a result,  the Haskell standard library
includes two type classes (`Semigroup` and `Monoid`) that capture this general idea.

Semigroups and Monoids
----------------------
-}

import Data.Semigroup -- also re-exports much of Data.Monoid
-- also re-exports much of Data.Monoid
import Test.HUnit (Test (TestList), runTestTT, (~?=))
import Prelude hiding (filter, foldr, length, minimum)

{-
The `Semigroup` and `Monoid` typeclasses may be the first examples of something
you've seen which does not
easily align to overloading interfaces defined in your (previous) favorite
programming language. We're going to use these typeclasses to define the
remainder of our interface to sorted lists.

The `Semigroup` type class defines what it means to "combine structures
together".  In particular, it includes the following binary operator, which must
be defined when making a type an instance of this class.

< (<>) :: a -> a -> a

This operation can be any binary operation on the type, with one important
caveat: it must be *ASSOCIATIVE*.  In other words, it shouldn't matter in your
code if you type `a <> (b <> c)` or `(a <> b) <> c`.  Either expression should
produce the same result.

You've probably seen some examples of types with associative operations
before.  For example, list concatenation is associative, so the standard
library includes the following instance that says that when we are using lists
as semigroups, we should interpret `(<>)` to mean `(++)`.

< instance Semigroup [a] where
<    (<>) = (++)

Because this instance is in the standard library, you don't actually need to
use the specialized `(++)` for lists; you can use the more general `(<>)`
whenever you please (as long as it is scope, and Haskell can tell you are
using lists).

The `Monoid` typeclass extends `Semigroup` class with a designated value,
called `mempty`. A type must first be declared to be an instance of the
`Semigroup` class before it can be declared to be a `Monoid`.

< mempty  :: Monoid a => a

The intended meaning is that `mempty` is some value of the type such that
when `<>`ed to anything, it does nothing. In other words, for any type that
has an associative binary operation (`<>`) with an identity element (`mempty`)
is a `Monoid`.

Now, that's all very abstract! Let's look at some instances. We can extend our
list `Semigroup` by adding an identity element for list concatenation.

< instance Monoid [a] where
<   mempty  = []

In fact, lists are the canonical example of a `Monoid` -- they can be combined
together with `(++)`, and the empty list, when combined with any other list
via `(++)`, gives that other list as a result.

Furthermore, the test case below demonstrates that lists satisfy the required
properties of monoids: the empty list is a left and right identity for append,
and concatenation is an associative operation.
-}

testListMonoid :: Test
testListMonoid =
  let t1, t2, t3 :: [Int]
      t1 = [1, 2]
      t2 = [3, 4]
      t3 = [1, 2, 3, 4]
   in TestList
        [ mempty <> t1 ~?= t1, -- left identity
          t1 <> mempty ~?= t1, -- right identity
          (t1 <> t2) <> t3 ~?= t1 <> (t2 <> t3) -- associativity
        ]

{-
What else? Another example that may jump to mind is numbers. Any integer can
be added to any other, with zero being the identity element. So you might
expect that the standard library would have an instance like this:

< instance Semigroup Integer where
<   (<>) = (+)

< instance Monoid Integer where
<   mempty  = 0

But it does not. After all, you could just as well realize that integers can
be combined by *multiplying* them, with *one* being the identity element! In
that case, we'd write an instance like this:

< instance Semigroup Integer where
<   (<>) = (*)

< instance Monoid Integer where
<   mempty  = 1

Who's to say which monoidal interpretation of integers is "more right"?

In cases like this, we usually use a `newtype` to differentiate between which
interpretation we want to use. That is, we can instead say:

< newtype Sum     a = Sum     { getSum     :: a }
< newtype Product a = Product { getProduct :: a }
<
< instance Num a => Semigroup (Sum a) where
<   x <> y = Sum $ getSum x + getSum y
< instance Num a => Monoid (Sum a) where
<   mempty = Sum 0
<
< instance Num a => Semigroup (Product a) where
<   x <> y = Product $ getProduct x * getProduct y
< instance Num a => Monoid (Product a) where
<   mempty = Product 1
<

Notice that in the above, these instances require a `Num` instance for the
types they're wrapping. `Num`, as you have seen in class, is a typeclass which
provides overloading for numeric operations and literals -- so using it as a
superclass of our instance allows us to be generic over what kind of numbers
we're manipulating, rather than fixing a particular type of number.

For example, we can calculate the sum and product of a list of integers
by coercing the elements to type `Sum Int` and `Product Int` respectively.
-}

foldList :: Monoid b => [b] -> b
foldList = List.foldr (<>) mempty

ten :: Int
ten = getSum (foldList (map Sum [1, 2, 3, 4]))

twentyfour :: Int
twentyfour = getProduct (foldList (map Product [1, 2, 3, 4]))

---------------------------------------------------------

{-
Sorted lists
------------

As described the above, we need to define our abstract type as a wrapper around ordinary
lists. For this, we use Haskell's `newtype` keyword, which creates a new type
much like `data`, but with the guarantee that our access to the wrapped type
will be with zero runtime overhead.
-}

newtype SortedList a = SL [a] deriving (Eq, Show)

{-
We can use pattern matching to convert the sorted list into a regular list.
-}

-- | convert to a regular list. The elements should be produced in order.
toList :: SortedList a -> [a]
toList (SL as) = as

-- | convert from a regular list.
fromList :: Ord a => [a] -> SortedList a
fromList = foldList . map singleton

{-
Some of the operations that we define for sorted lists just delegate
to the version for regular lists.
-}

-- | construct a sorted list containing a single element
singleton :: a -> SortedList a
singleton a = SL [a]

-- | reduce a SortedList in order
foldr :: (a -> b -> b) -> b -> SortedList a -> b
foldr f b (SL xs) = List.foldr f b xs

-- | decide which elements of the sorted list to keep
filter :: (a -> Bool) -> SortedList a -> SortedList a
filter f (SL xs) = SL (List.filter f xs)

-- | count the number of elements in the sorted list
length :: SortedList a -> Int
length (SL xs) = List.length xs

{-
However, the `Monoid` instance can take advantage of the
sortedness of this data structure.

Now, fill in the `Monoid` instance for `SortedList`s. You should ensure that the
list is always sorted with smaller elements (according to `(<=)` coming
before larger elements.)

Hint: keep in mind the properties of sorted lists when writing this
instance. This invariant lets you write faster code than you would otherwise
be able to do.
-}

merge [] l2 = l2
merge l1 [] = l1
merge (x : xs) (y : ys)
  | x < y = x : (xs `merge` (y : ys))
  | x > y = y : ((x : xs) `merge` ys)
  | otherwise = x : y : (xs `merge` ys)

instance Ord a => Semigroup (SortedList a) where
  (SL xs) <> (SL ys) = SL (merge xs ys)

instance Ord a => Monoid (SortedList a) where
  mempty = SL []

{-
Make sure that your implementation only produces sorted lists, and also
satisfies the properties of monoids!
-}

testSortedList :: Test
testSortedList =
  let t1, t2, t3 :: SortedList Int
      t1 = SL [2, 4]
      t2 = SL [1, 5]
      t3 = SL [2, 3]
   in TestList
        [ t1 <> t3 ~?= SL [2, 2, 3, 4], -- <> preserves sorting
          mempty <> t1 ~?= t1, -- left identity
          t1 <> mempty ~?= t1, -- right identity
          (t1 <> t2) <> t3 ~?= t1 <> (t2 <> t3) -- associativity
        ]

-- >>> runTestTT testSortedList
-- Cases: 4  Tried: 0  Errors: 0  Failures: 0
-- Cases: 4  Tried: 1  Errors: 0  Failures: 0
-- Cases: 4  Tried: 2  Errors: 0  Failures: 0
-- Cases: 4  Tried: 3  Errors: 0  Failures: 0
--
-- Cases: 4  Tried: 4  Errors: 0  Failures: 0
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}
--

---------------------------------------------------------

{-
Invariant-sensitive Operations
------------------------------

Note that we didn't *have* to define `foldr`, `filter`, and `length` for `SortedList`s.
The clients of the module could have also defined these operations themselves by using
`toNormalList` and the `Monoid` operations.

However, by defining operations in this module, we
While merely the operations defined above are sufficient to define the analogues
of most list functions for `SortedList`s also, implementing a replica of the
list library only in terms of the above abstraction would necessarily come at a
performance cost; it would necessitate conversion to and from the `SortedList`
representation, which requires computational work.

On the other hand, if we were to implement these functions *here*, we could
take advantage of the internal sorted-ness invariant of the list in order to
make certain operations *faster*. Let's do that.

A first example: `minimum`. (Note: this definition does not have the same type
as the `minimum` function in the standard library.)
-}

minimum :: SortedList a -> Maybe a
minimum (SL []) = Nothing
minimum (SL (x : xs)) = Just x

testMinimum :: Test
testMinimum =
  let t1, t2, t3 :: SortedList Int
      t1 = SL [1, 3, 5]
      t2 = SL []
      t3 = SL [1, error "kaboom!"] <> SL [2]
   in TestList
        [ minimum t1 ~?= Just 1, -- the minimum of a non-empty sorted list
          minimum t2 ~?= Nothing, -- the minimum of an empty sorted list
          minimum t3 ~?= Just 1 -- minimum need not examine whole list
        ]

-- >>> runTestTT testMinimum
-- Cases: 3  Tried: 0  Errors: 0  Failures: 0
-- Cases: 3  Tried: 1  Errors: 0  Failures: 0
-- Cases: 3  Tried: 2  Errors: 0  Failures: 0
--
-- Cases: 3  Tried: 3  Errors: 0  Failures: 0
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}
--

{-
In the above test cases, you will get an error if your implementation does not
take advantage of the sorted-ness invariant to avoid extra computation.

Another operation which can be made more efficient for `SortedList`s is
calculating the number of distinct values in the list.
-}

numDistinct :: Ord a => SortedList a -> Int
numDistinct (SL (x : xs)) = 1 + numDistinct (SL r)
  where
    (l, r) = break (> x) xs
numDistinct (SL []) = 0

testNumDistinct :: Test
testNumDistinct =
  TestList
    [ numDistinct (SL [1 :: Int, 1, 3, 3, 5]) ~?= 3,
      numDistinct (SL ([] :: [Int])) ~?= 0
    ]

count :: Eq a => SortedList a -> SortedList (a, Integer)
count (SL (x : xs)) = SL ((x, fromIntegral $ 1 + List.length l) : rest)
  where
    (l, r) = break (/= x) xs
    (SL rest) = count (SL r)
count (SL []) = (SL [])

{-
Your implementation of `count` should result in another genuine, legal
`SortedList`. Convince yourself that it does before moving on, keeping in mind
the `Ord` instances for tuples are left-to-right lexicographic orderings,
dependent on the underlying `Ord` instances of the tuple's elements.
-}

testCount :: Test
testCount =
  let xs = SL "abbcccdddd"
   in count xs ~?= SL [('a', 1), ('b', 2), ('c', 3), ('d', 4)]

-- >>> runTestTT testCount
-- Cases: 1  Tried: 0  Errors: 0  Failures: 0
--
-- Cases: 1  Tried: 1  Errors: 0  Failures: 0
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}
--

{-
At this point, one important typeclass seems to have been left out in our
interface to the `SortedList` type: `Functor`. It seems natural that we should
be able to map a function over a `SortedList`, just like we can over an ordinary
list. This doesn't work, though. Why?
-}

whyNoFunctor :: String
whyNoFunctor = "sortedness invariant can be broken"

{-
At this point, we have finished defining the internal implementation of
`SortedList`s. Because all the operations we expose to the user of this module
respect the sorted-ness property of `SortedList`s, we know that any value of
this type must be sorted. So, once we go back to the file `Main.lhs`, we will be
we are prevented from making "illegal" values of `SortedList`s.

-}
