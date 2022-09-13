{-# LANGUAGE TupleSections #-}

{-# OPTIONS -Wall -fwarn-tabs #-}
module HW1 where

import Control.Monad (void)
import qualified Data.Char as Char
import Data.Foldable (minimumBy)
import Data.List (takeWhile)
import qualified Data.List as List
import Data.Maybe (isJust)
import qualified Data.Maybe as Maybe
import Test.HUnit
import qualified Text.Read as Read
import Prelude hiding (all, concat, reverse, takeWhile, zip)

-- >>> main

main :: IO ()
main = void $ runTestTT $ TestList [testStyle, testLists, testWeather, testSoccer]

testStyle :: Test
testStyle = "testStyle" ~: TestList [tabc, tarithmetic, treverse, tzip]

abc :: Bool -> Bool -> Bool -> Bool
abc x y z = x && (y || (x && z))

tabc :: Test
tabc =
  "abc"
    ~: TestList
      [ abc True False True ~?= True,
        abc True False False ~?= False,
        abc False True True ~?= False
      ]

arithmetic :: ((Int, Int), Int) -> ((Int, Int), Int) -> (Int, Int, Int)
arithmetic ((a, b), c) ((d, e), f) =
  ((b * f) - (c * e), (c * d) - (a * f), (a * e) - (b * d))

tarithmetic :: Test
tarithmetic =
  "arithmetic"
    ~: TestList
      [ arithmetic ((1, 2), 3) ((4, 5), 6) ~?= (-3, 6, -3),
        arithmetic ((3, 2), 1) ((4, 5), 6) ~?= (7, -14, 7)
      ]

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

treverse :: Test
treverse =
  "reverse"
    ~: TestList
      [ reverse [3, 2, 1] ~?= [1 :: Int, 2, 3],
        reverse [1] ~?= [1 :: Int]
      ]

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

tzip :: Test
tzip =
  "zip"
    ~: TestList
      [ zip "abc" [True, False, True] ~?= [('a', True), ('b', False), ('c', True)],
        zip "abc" [True] ~?= [('a', True)],
        zip [] [] ~?= ([] :: [(Int, Int)])
      ]

--------------------------------------------------------------------------------

testLists :: Test
testLists =
  "testLists"
    ~: TestList
      [tintersperse, tinvert, ttranspose, tconcat, tcountSub]

-- The intersperse function takes an element and a list
-- and intersperses that element between the elements of the list.
-- For example,
--    intersperse ',' "abcde" == "a,b,c,d,e"
--
-- intersperse is defined in Data.List, and you can test your solution against
-- that one.

-- >>> List.intersperse ',' []
-- ""
--

intersperse :: a -> [a] -> [a]
intersperse c [] = []
intersperse c [x] = [x]
intersperse c (x : xs) = x : c : intersperse c xs

tintersperse :: Test
tintersperse =
  "intersperse"
    ~: TestList
      [ intersperse ',' [] ~?= List.intersperse ',' [],
        intersperse ',' "asd" ~?= List.intersperse ',' "asd"
      ]

-- invert lst returns a list with each pair reversed.
-- for example:
--   invert [("a",1),("a",2)]     returns [(1,"a"),(2,"a")]
--   invert ([] :: [(Int,Char)])  returns []

--   note, you need to add a type annotation to test invert with []
--

invert :: [(b, a)] -> [(a, b)]
invert [] = []
invert ((a, b) : xs) = (b, a) : invert xs

tinvert :: Test
tinvert =
  "invert"
    ~: TestList
      [ invert ([] :: [(Int, String)]) ~?= [],
        invert [(1 :: Int, 2 :: Int), (3, 4)] ~?= [(2, 1), (4, 3)]
      ]

-- concat

-- The concatenation of all of the elements of a list of lists
-- for example:
--    concat [[1,2,3],[4,5,6],[7,8,9]] returns [1,2,3,4,5,6,7,8,9]
--
-- NOTE: remember you cannot use any functions from the Prelude or Data.List for
-- this problem, even for use as a helper function.

concat :: [[a]] -> [a]
concat [] = []
concat (x : xs) = append x (concat xs)
  where
    append [] ys = ys
    append (a : as) ys = a : append as ys

tconcat :: Test
tconcat =
  "concat"
    ~: [ concat ([] :: [[Int]]) ~?= [],
         concat [[1, 2, 3, 4 :: Int], [1, 2, 3, 4]] ~?= List.concat ([[1, 2, 3, 4], [1, 2, 3, 4]])
       ]

-- transpose  (WARNING: this one is tricky!)

-- The transpose function transposes the rows and columns of its argument.
-- If the inner lists are not all the same length, then the extra elements
-- are ignored. Note, this is *not* the same behavior as the library version
-- of transpose.

-- for example:
--    transpose [[1,2,3],[4,5,6]] returns [[1,4],[2,5],[3,6]]
--    transpose  [[1,2],[3,4,5]] returns [[1,3],[2,4]]
--    transpose  [[]] returns []
-- transpose is defined in Data.List

-- >>> transpose [[1, 2, 3], [4, 5, 6]]
-- [[1,4],[2,5],[3,6]]
--

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose [xs] = fmap return xs
transpose (as : ass) = zipWith (:) as (transpose ass)

ttranspose :: Test
ttranspose =
  "transpose"
    ~: [ transpose [[1, 2, 3], [4, 5, 6]] ~?= [[1, 4], [2, 5], [3, 6]],
         transpose [[1, 2], [3, 4, 5]] ~?= [[1, 3], [2, 4]],
         transpose ([[]] :: [[Int]]) ~?= []
       ]

-- countSub sub str

-- Return the number of (potentially overlapping) occurrences of substring sub
-- found in the string str.
-- for example:
--      countSub "aa" "aaa" returns 2

-- >>> countSub "aa" "aaa"
-- 2
--
-- >>> countSub "aa" "a"
-- 0
--

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf (_ : _) [] = False
isPrefixOf [] _ = True
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

countSub xs [] = 0
countSub xs (y : ys) = if xs `isPrefixOf` (y : ys) then 1 + countSub xs ys else countSub xs ys

tcountSub :: Test
tcountSub = "countSub" ~: [countSub "aa" "aaa" ~?= 2, countSub "aa" "a" ~?= 0]

--------------------------------------------------------------------------------

-- Part One: Hottest Day

weatherInput = "./data/HW1/jul17.dat"

-- >>> weatherProgram
-- "6"
--

ord :: (a, (Int, Int)) -> (b, (Int, Int)) -> Ordering
ord w1 w2 = compare (spread w1) (spread w2)
  where
    spread (a, (x, y)) = abs $ x - y

takeValid :: [Maybe a] -> [a]
takeValid [] = []
takeValid (Nothing : xs) = takeValid xs
takeValid ((Just x) : xs) = x : takeValid xs

weather :: String -> String
weather = fst . minimumBy ord . takeValid . fmap (parse . words) . drop 18 . lines
  where
    parse :: [String] -> Maybe (String, (Int, Int))
    parse (a : b : c : _) = (a,) <$> ((,) <$> readInt b <*> readInt c)
    parse _ = Nothing

weatherProgram :: IO ()
weatherProgram = do
  str <- readFile weatherInput
  putStrLn (weather str)

readInt :: String -> Maybe Int
readInt = Read.readMaybe

testWeather :: Test
testWeather =
  "weather" ~: do
    str <- readFile weatherInput
    weather str @?= "6"

--------

-- Part Two: Soccer League Table

-- >>> soccerProgram
-- Aston_Villa
--

soccerInput = "./data/HW1/soccer.dat"

soccer :: String -> String
soccer = fst . minimumBy ord . takeValid . fmap parse . fmap (words) . lines
  where
    parse :: [String] -> Maybe (String, (Int, Int))
    parse (_ : name : _ : _ : _ : _ : for : _ : aga : _) = (name,) <$> ((,) <$> readInt for <*> readInt aga)
    parse _ = Nothing

soccerProgram :: IO ()
soccerProgram = do
  str <- readFile soccerInput
  putStrLn (soccer str)

testSoccer :: Test
testSoccer =
  "soccer" ~: do
    str <- readFile soccerInput
    soccer str @?= "Aston_Villa"

-- Part Three: DRY Fusion

execute f input = do
  str <- readFile input
  putStrLn (f str)

weather2 = execute weather weatherInput

soccer2 = execute soccer soccerInput
