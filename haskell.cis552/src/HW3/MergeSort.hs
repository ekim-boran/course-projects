{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
Merge Sort
==========
-}

module HW3.MergeSort where

import qualified Data.List as List
import Data.Monoid
import HW3.SortedList (SortedList)
import qualified HW3.SortedList as SL
import Test.HUnit

{-
A warm-up exercise: write the function `insert`, which takes an element and a
sorted list and inserts the element into the list at the first position where
it is less than or equal to the next element. For this definition, do not use
any functions from the `Data.List` library (which, indeed, contains such a
function).
-}

-- >>> insert 3 [1, 2, 12]
-- [1,2,3,12]
--

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

{-
Using this function, we can define the *insertion sort* algorithm over
lists. Insertion sort, like several other sorting algorithms, is *incremental*
-- it works by processing a single element of the input unsorted list at a
time, and when it finishes processing the input, its work is done. Using the
`insert` function above, write a function which takes a list and returns the
list sorted. You must express your answer in terms of a fold.
-}

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []

{-
Keep in mind that this sorting algorithm, although succinct, is not efficient
-- it has O(N ^ 2) asymptotic complexity.

You may have noticed that the `insert` function above has an invariant
attached to its description; namely, that its list argument is already
sorted. If that invariant holds, then `insert` guarantees that its output will
also be sorted; otherwise, there is no such guarantee.

A leading question: what if we could keep track of the "sorted-ness" of a list
using the type system? You already know that we can -- that's precisely what
we did in `SortedList.lhs`.
-}

-------------------------------------------------------------

{-
SortedLists to the Rescue
-------------------------

I previously promised that the interface we built for `SortedList`s would be
sufficient to do useful things with them.  One particularly obvious useful
thing to do with `SortedList`s is to construct them from arbitrary lists --
that is, to sort an arbitrary list and produce a `SortedList` with the result.
(UPDATE: this function is already defined in the `SortedList` module as `fromList`. You
can just use that definition here.)
-}

sortedFromList :: Ord a => [a] -> SortedList a
sortedFromList = foldr ((<>) . SL.singleton) mempty

{-
By projecting out the underlying list, we get a sorting function, that we'll
call `sortedListSort`.
-}

sortedListSort :: Ord a => [a] -> [a]
sortedListSort = SL.toList . sortedFromList

testSortedFromList :: Test
testSortedFromList =
  let unsorted = [51, 67, 89, 95, 14, 31, 28, 87, 0, 25]
      sorted = [0, 14, 25, 28, 31, 51, 67, 87, 89, 95]
   in sortedListSort unsorted ~?= sorted

{-
One thing you may have noticed while writing the above function is that there
is only one place you could have made reference to the `SortedList` type
specifically: in the use of the `singleton` operation. Indeed, this operation
is the only `SortedList`-specific way to create new `SortedList`s -- any other
way comes through the `Monoid` instance. Perhaps there's some common pattern
here that we could abstract! (There is.) Let's express it by making the
`singleton` function into a parameter of a new function, that we will call
`foldMapList`, so that we can rewrite the algorithm above like so:
-}

sortedFromList' :: Ord a => [a] -> SortedList a
sortedFromList' = foldMapList SL.singleton

{-
Again, we can project out the underlying list to get a list sorting function.
-}

sortedListSort' :: Ord a => [a] -> [a]
sortedListSort' = SL.toList . sortedFromList'

testSortedFromList' :: Test
testSortedFromList' =
  let unsorted :: [Int]
      unsorted = [47, 80, 28, 47, 45, 76, 1, 35, 19, 1]
   in sortedListSort' unsorted ~?= sortedListSort unsorted -- old & new agree

{-
In order to make this work, you need to define the `foldMapList` combinator.
-}

foldMapList :: Monoid m => (a -> m) -> [a] -> m
foldMapList f = foldr ((<>) . f) mempty

{-
The type of `foldMapList` is very general --- we can use this function to
combine arbitrary lists by providing a function that maps their contents to
some particular `Monoid` (such as `SortedList`). For instance, `foldMapList
Sum` gives us the sum of the numbers in the list; `foldMap Product` gives us
their product.

A small exercise: using `foldMapList` and the `Sum` and `Product` newtypes you
learned about in `SortedList.lhs`, implement the following function, which takes
a doubly-nested list of numbers as input, and returns the sum of the product of
each of the inner lists. In your solution, do not explicitly use any of the
ordinary numeric operations like `(+)`, `(*)`, `sum`, or `product`, and eschew
explicit recursion.
-}

sumOfProducts :: Num a => [[a]] -> a
sumOfProducts = getSum . foldMapList (Sum . getProduct . foldMapList Product)

testSumOfProducts :: Test
testSumOfProducts = sumOfProducts [[1], [2, 3], [4, 5, 6], [7, 8, 9, 10]] ~?= (5167 :: Int)

-- >>> sumOfProducts [[1], [2, 3], [4, 5, 6], [7, 8, 9, 10]]
-- 5167
--
{-
Like Merge Sort, the `sortedListSort` function is based on merging sorted
lists together.  This merge-sort-like algorithm has a flaw, though: it's
quadratic in runtime. Why?
-}

benchmark :: IO ()
benchmark = (print . last . sortedListSort') ([10000, 9999 .. 0] :: [Int])

-- 10000
--
-- (0.00 secs, 673,048 bytes)
-- 10000
-- (0.00 secs, 673,048 bytes)
-- 10000
-- (0.00 secs, 643,480 bytes)
--
-- (21.16 secs, 24,151,886,984 bytes)
--
-- >>> :set +s
-- >>> benchmark

{-
(Try it yourself by setting  `:set +s` in ghci. On my machine, it take 26.61 secs
and allocates 17,604,539,672 bytes))

For any singleton `SortedList [a]` and any other `SortedList as`, computing
`SortedList [a] <> SortedList as` is identical not only in resultant value,
but also in algorithmic structure to computing the result of `insert a
as`. The definition of `foldMapList` linearly scans across its input list,
successively combining values using `(<>)` -- and so, like insertion sort, the
whole whole algorithm ends up executing a quadratic number of comparisons.

A real merge sort algorithm, as you likely know, divides its input more
intelligently than the one we've written above in terms of `foldMapList`. By
dividing its input roughly in half every iteration, it only has to do a
logarithmic amount of merging.

To make our merge sort do this, we need to use a different kind of `foldMap`!

The Foldable Typeclass
----------------------

At this point, I'd like to point something out: `foldMapList` can itself be
even further generalized. We already know that lists are not the only data
structures which support folding -- we've seen folds for trees of various kinds
and for other data structures as well. As a result, it makes sense to allow
some kind of `foldMap` operation for those structures also. In the standard
library, we therefore have:

< foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

That is to say, `foldMap` is a method of yet another type class, `Foldable`, of
which the type constructor `[]` is an instance. Implementing this interface
roughly corresponds to saying, "this data structure contains some elements, and
I know how to do a fold across them." To implement the `Foldable` class for some
type, we just need to implement `foldMap`.

Implement the `Functor` and `Foldable` instances for the `Crispy` datatype.
Remember to keep in mind a guiding principle: when you are confused, don't think
about what things are supposed to mean; just follow the types and see where they
take you.
-}

data Crispy a
  = Snap a [a] a
  | Crackle [[Crispy a]]
  | Pop Integer
  deriving (Eq, Show)

instance Functor Crispy where
  fmap f (Snap a as b) = Snap (f a) (f <$> as) (f b)
  fmap f (Crackle xss) = Crackle ((fmap . fmap . fmap) f xss)
  fmap f (Pop x) = Pop x

instance Foldable Crispy where
  foldMap f (Snap a as b) = (f a) <> (foldMap f as) <> (f b)
  foldMap f (Crackle xss) = ((foldMap . foldMap . foldMap) f xss)
  foldMap f (Pop x) = mempty

testCrispy :: Test
testCrispy =
  let c1, c2, c3, c5 :: Crispy Integer
      c1 = fmap (+ 1) (Snap 0 [1, 2, 3] 4)
      c2 = Snap 700 [] 600
      c3 = Pop 1234567890
      c5 = fmap (subtract 1) (Crackle [[c1, c2], [c1, c3]])
   in TestList
        [ 15 ~?= getSum (foldMap Sum c1),
          1 ~?= getProduct (foldMap Product c3),
          "0123469959901234" ~?= foldMap show c5
        ]

-- >>> runTestTT testCrispy
-- (0.00 secs, 673,056 bytes)
-- <BLANKLINE>
-- Cases: 3  Tried: 0  Errors: 0  Failures: 0
-- Cases: 3  Tried: 1  Errors: 0  Failures: 0
-- Cases: 3  Tried: 2  Errors: 0  Failures: 0
--
-- Cases: 3  Tried: 3  Errors: 0  Failures: 0
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}
-- (0.00 secs, 894,064 bytes)
--

-------------------------------------------------------------

{-
Back to Sorting
---------------

In order to express an efficient merge sort in terms of `foldMap`, we need to
design a data structure that represents a sequence of elements (just like a
list), but whose `Foldable` instance uses a divide-and-conquer strategy, rather
than the `[]` instance's linear fold pattern of recursion.
-}

newtype DivideList a = DivideList {getDivideList :: [a]} deriving (Eq, Show)

{-
That means that we need `DivideList`s to be `Foldable`, but in a
different way. First, implement the `divide` function, which splits a
`DivideList` in its middle, returning the result of the split.
-}

divide :: DivideList a -> (DivideList a, DivideList a)
divide (DivideList xs) = (DivideList (take l xs), DivideList (drop l xs))
  where
    l = length xs `div` 2

testDivide :: Test
testDivide =
  TestList
    [ divide (DivideList "abcd")
        ~?= (DivideList "ab", DivideList "cd"),
      divide (DivideList "abcde")
        ~?= (DivideList "ab", DivideList "cde"),
      divide (DivideList "")
        ~?= (DivideList "", DivideList "")
    ]

-- >>> runTestTT testDivide
-- (0.00 secs, 672,192 bytes)
-- <BLANKLINE>
-- Cases: 3  Tried: 0  Errors: 0  Failures: 0
-- Cases: 3  Tried: 1  Errors: 0  Failures: 0
-- Cases: 3  Tried: 2  Errors: 0  Failures: 0
--
-- Cases: 3  Tried: 3  Errors: 0  Failures: 0
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}
-- (0.00 secs, 884,632 bytes)
--
{-
Using this function, we can define the `Foldable` instance for `DivideList`s.
Note that this definition is trickier than it seems. If you encounter an
infinite loop, it means that you have not covered one of a particular set of
slightly non-trivial edge cases.
-}

instance Foldable DivideList where
  foldMap f xs =
    case divide xs of
      (DivideList [], DivideList []) -> mempty
      (DivideList [], DivideList [b]) -> f b
      (DivideList [], DivideList bs) -> foldMap f (DivideList bs)
      (DivideList [a], DivideList []) -> f a
      (DivideList as, DivideList []) -> foldMap f as
      (DivideList as, DivideList bs) -> foldMap f (DivideList as) <> foldMap f (DivideList bs)

testDivideList :: Test
testDivideList =
  let xs = DivideList [1, 2, 3]
      ys = DivideList []
   in TestList
        [ Product (6 :: Int) ~?= foldMap Product xs,
          Sum (0 :: Int) ~?= foldMap Sum ys
        ]

-- >>> runTestTT testDivideList
-- (0.00 secs, 672,184 bytes)
-- <BLANKLINE>
-- Cases: 2  Tried: 0  Errors: 0  Failures: 0
-- Cases: 2  Tried: 1  Errors: 0  Failures: 0
--
-- Cases: 2  Tried: 2  Errors: 0  Failures: 0
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}
-- (0.00 secs, 845,504 bytes)
--

{-
Now that we know how general the `foldMap` function is, have a look at the
implementation of `sortedListSort'` above -- does its input type need to only be
a list? Generalize its type signature so that it outputs a list of sorted
elements located inside an arbitrary `Foldable` structure.
-}

foldSort :: (Foldable f, Ord a) => f a -> [a]
foldSort xs = SL.toList (foldMap SL.singleton xs)

{-
By parameterizing over any `Foldable` container, what we've done is to *factor
out the folding strategy* into the choice of original container! To pick a
different divide-and-conquer strategy, we need only specify a different
container type, and give it a `Foldable` instance that folds along different
creases.

So, while our `sortedListSort` was O(N ^ 2), we can produce a differently
structured algorithm by instead folding over a `DivideList` instead:
-}

realMergeSort :: Ord a => [a] -> [a]
realMergeSort = foldSort . DivideList

{-
If you've done everything correctly, this main function should return rather
quickly. This is much faster than the example above. On my machine it takes
(0.07 secs, 41,595,632 bytes).
-}

-- >>> (print . last . realMergeSort) ([10000, 9999 .. 0] :: [Int])
-- (0.00 secs, 673,048 bytes)
-- 10000
-- (0.07 secs, 47,424,248 bytes)
--

main :: IO ()
main = (print . last . realMergeSort) ([10000, 9999 .. 0] :: [Int])

{-
Concluding Thoughts About This Exercise
---------------------------------------

The important takeaway here is this: `foldMap` defines once and for all a
universal "divide-and-conquer" algorithm -- all we need to do to use it is to
provide a way to "divide" an input container (i.e. give a `Foldable instance`),
then give a way to compute on those elements (i.e. the mapped function `a -> m`)
and a way to combine ("conquer") the results of that computation (i.e. a
`Monoid` instance for the result type).

Almost any divide-and-conquer algorithm can be fit into this framework, and that
means we can avoid repeating ourselves when writing such programs. We can reuse
the division strategy of `DivideList` when writing some other algorithm, and
likewise for the sorted-merging combination strategy of `SortedList`.
-}