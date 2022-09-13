-- {-# OPTIONS -fdefer-type-errors  #-}

module HW2.HW2 where

-- unit test support

-- support file for XML problem (provided)
-- support file for XML problem (provided)

import Control.Applicative ((<|>))
import qualified Data.List as List
import HW2.Play
import HW2.XMLTypes
import Test.HUnit
import Prelude hiding (all, concat, takeWhile)

-- >>> runTestTT $ testTree
-- Cases: 10  Tried: 0  Errors: 0  Failures: 0
-- Cases: 10  Tried: 1  Errors: 0  Failures: 0
-- Cases: 10  Tried: 2  Errors: 0  Failures: 0
-- Cases: 10  Tried: 3  Errors: 0  Failures: 0
-- Cases: 10  Tried: 4  Errors: 0  Failures: 0
-- Cases: 10  Tried: 5  Errors: 0  Failures: 0
-- Cases: 10  Tried: 6  Errors: 0  Failures: 0
-- Cases: 10  Tried: 7  Errors: 0  Failures: 0
--

-- >>> main
-- (0.00 secs, 673,048 bytes)
-- <BLANKLINE>
-- Cases: 38  Tried: 0  Errors: 0  Failures: 0
-- Cases: 38  Tried: 1  Errors: 0  Failures: 0
-- Cases: 38  Tried: 2  Errors: 0  Failures: 0
-- Cases: 38  Tried: 3  Errors: 0  Failures: 0
-- Cases: 38  Tried: 4  Errors: 0  Failures: 0
-- Cases: 38  Tried: 5  Errors: 0  Failures: 0
-- Cases: 38  Tried: 6  Errors: 0  Failures: 0
-- Cases: 38  Tried: 7  Errors: 0  Failures: 0
-- Cases: 38  Tried: 8  Errors: 0  Failures: 0
-- Cases: 38  Tried: 9  Errors: 0  Failures: 0
-- Cases: 38  Tried: 10  Errors: 0  Failures: 0
-- Cases: 38  Tried: 11  Errors: 0  Failures: 0
-- Cases: 38  Tried: 12  Errors: 0  Failures: 0
-- Cases: 38  Tried: 13  Errors: 0  Failures: 0
-- Cases: 38  Tried: 14  Errors: 0  Failures: 0
-- Cases: 38  Tried: 15  Errors: 0  Failures: 0
-- Cases: 38  Tried: 16  Errors: 0  Failures: 0
-- Cases: 38  Tried: 17  Errors: 0  Failures: 0
-- Cases: 38  Tried: 18  Errors: 0  Failures: 0
-- Cases: 38  Tried: 19  Errors: 0  Failures: 0
-- Cases: 38  Tried: 20  Errors: 0  Failures: 0
-- Cases: 38  Tried: 21  Errors: 0  Failures: 0
-- Cases: 38  Tried: 22  Errors: 0  Failures: 0
-- Cases: 38  Tried: 23  Errors: 0  Failures: 0
-- Cases: 38  Tried: 24  Errors: 0  Failures: 0
-- Cases: 38  Tried: 25  Errors: 0  Failures: 0
-- Cases: 38  Tried: 26  Errors: 0  Failures: 0
-- Cases: 38  Tried: 27  Errors: 0  Failures: 0
-- Cases: 38  Tried: 28  Errors: 0  Failures: 0
-- Cases: 38  Tried: 29  Errors: 0  Failures: 0
-- Cases: 38  Tried: 30  Errors: 0  Failures: 0
-- Cases: 38  Tried: 31  Errors: 0  Failures: 0
-- Cases: 38  Tried: 32  Errors: 0  Failures: 0
-- Cases: 38  Tried: 33  Errors: 0  Failures: 0
-- Cases: 38  Tried: 34  Errors: 0  Failures: 0
-- Cases: 38  Tried: 35  Errors: 0  Failures: 0
-- Cases: 38  Tried: 36  Errors: 0  Failures: 0
-- Cases: 38  Tried: 37  Errors: 0  Failures: 0
--
-- Cases: 38  Tried: 38  Errors: 0  Failures: 0
-- (0.02 secs, 2,310,504 bytes)
--

doTests :: IO ()
doTests = do
  _ <- runTestTT $ TestList [testHO, testFoldr, testTree, testFoldTree, testXML]
  return ()

main :: IO ()
main = do
  doTests
  return ()

testHO :: Test
testHO = TestList [ttakeWhile, tfind, tall, tmap2, tmapMaybe]

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f = foldr (\x xs -> if f x then x : xs else []) []

ttakeWhile :: Test
ttakeWhile =
  "takeWhile"
    ~: [ takeWhile (< 3) [1, 2, 2, 4, 0] ~?= [1, 2, 2],
         takeWhile (< 3) [] ~?= [],
         takeWhile (< 5) [1, 2, 2, 4, 1] ~?= [1, 2, 2, 4, 1]
       ]

find :: (a -> Bool) -> [a] -> Maybe a
find f = foldl (\xs x -> xs <|> if f x then Just x else Nothing) Nothing

tfind :: Test
tfind =
  "find"
    ~: [ find (== 0) [1, 2, 2, 4, 0] ~?= Just 0,
         find (< 3) [] ~?= Nothing,
         find (< 5) [1, 2, 2, 4, 1] ~?= Just 1
       ]

-- all pred lst returns False if any element of lst
-- fails to satisfy pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False

all :: (a -> Bool) -> [a] -> Bool
all f = foldl (flip ((&&) . f)) True

tall :: Test
tall = "all" ~: [all (< 5) [1, 2, 2, 4, 1] ~?= True, all (< 5) [1, 2, 2, 4, 6] ~?= False]

-- map2 f xs ys returns the list obtained by applying f to
-- to each pair of corresponding elements of xs and ys. If
-- one list is longer than the other, then the extra elements
-- are ignored.
-- i.e.
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1]
--        returns [f x1 y1, f x2 y2, ..., f xn yn]
--
-- NOTE: map2 is called zipWith in the Prelude

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f as bs = uncurry f <$> zip as bs

tmap2 :: Test
tmap2 = "map2" ~: [map2 (+) [1 .. 3] [3 .. 5] ~?= [4, 6, 8]]

-- mapMaybe

-- Map a partial function over all the elements of the list
-- for example:
--    mapMaybe root [0.0, -1.0, 4.0] == [0.0,2.0]

root :: Double -> Maybe Double
root d = if d < 0.0 then Nothing else Just $ sqrt d

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr (\x xs -> case f x of Nothing -> xs; (Just a) -> a : xs) []

tmapMaybe :: Test
tmapMaybe = "mapMaybe" ~: [mapMaybe root [0.0, -1.0, 4.0] ~?= [0.0, 2.0]]

----------------------------------------------------------------------

testFoldr :: Test
testFoldr = TestList [tinvert, tintersperse, tconcat, tstartsWith, tcountSub]

swap (a, b) = (b, a)

invert :: [(a, b)] -> [(b, a)]
invert = foldr ((:) . swap) []

tinvert :: Test
tinvert = "invert" ~: [invert [(1, 2)] ~?= [(2, 1)], invert ([] :: [(Int, Int)]) ~?= ([] :: [(Int, Int)])]

intersperse :: a -> [a] -> [a]
intersperse sep [] = []
intersperse sep (x : xs) = x : foldr (\x xs -> sep : x : xs) [] xs

tintersperse :: Test
tintersperse =
  "intersperse"
    ~: [ intersperse ',' [] ~?= List.intersperse ',' [],
         intersperse ',' "asd" ~?= List.intersperse ',' "asd"
       ]

-- concat

concat :: [[a]] -> [a]
concat = foldr (++) []

tconcat :: Test
tconcat =
  "concat"
    ~: [ concat ([] :: [[Int]]) ~?= [],
         concat [[1, 2, 3, 4 :: Int], [1, 2, 3, 4]] ~?= [1, 2, 3, 4] ++ [1, 2, 3, 4]
       ]

-- >>> startsWith "aa" "aa"
-- True
--

startsWith :: String -> String -> Bool
startsWith phrase xs = foldr f null xs phrase
  where
    f x h [] = True
    f x h (a : as) = x == a && h as

tstartsWith =
  "tstartsWith"
    ~: [ startsWith "asd" "asdasd" ~?= List.isPrefixOf "asd" "asdasd",
         startsWith "x" "asdasd" ~?= List.isPrefixOf "x" "asdasd",
         startsWith "" "asdasd" ~?= List.isPrefixOf "" "asdasd",
         startsWith "asd" "" ~?= List.isPrefixOf "asd" ""
       ]

para :: (a -> [a] -> b -> b) -> b -> [a] -> b
para f acc [] = acc
para f acc (x : xs) = f x xs (para f acc xs)

countSub :: String -> String -> Int
countSub as = para f 0
  where
    f x xs rest = rest + (if startsWith as (x : xs) then 1 else 0)

-- >>> countSub "aa" "aaa"
-- 2
--

tcountSub =
  "countSub"
    ~: [ countSub "" "asdas" ~?= 5,
         countSub "aa" "aaa" ~?= 2,
         countSub "aa" "a" ~?= 0
       ]

----------------------------------------------------------------------

testTree :: Test
testTree =
  TestList
    [ tappendTree,
      tinvertTree,
      ttakeWhileTree,
      tallTree,
      tmap2Tree,
      tinfixOrder1,
      tinfixOrder2
    ]

-- | a basic tree data structure
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Empty = Empty
mapTree f (Branch x t1 t2) = Branch (f x) (mapTree f t1) (mapTree f t2)

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree _ e Empty = e
foldTree f e (Branch a n1 n2) = f a (foldTree f e n1) (foldTree f e n2)

-- The appendTree function takes two trees and replaces all of the 'Empty'
-- constructors in the first with the second tree.  For example:
--     appendTree (Branch 'a' Empty Empty) (Branch 'b' Empty Empty) returns
--        Branch 'a' (Branch 'b' Empty Empty) (Branch 'b' Empty Empty)

appendTree :: Tree a -> Tree a -> Tree a
appendTree a b = foldTree Branch b a

tappendTree :: Test
tappendTree =
  "appendTree"
    ~: [ appendTree Empty (Branch 'a' Empty Empty) ~?= Branch 'a' Empty Empty,
         appendTree (Branch 'a' Empty Empty) (Branch 'b' Empty Empty) ~?= Branch 'a' (Branch 'b' Empty Empty) (Branch 'b' Empty Empty)
       ]

-- The invertTree function takes a tree of pairs and returns a new tree
-- with each pair reversed.  For example:
--     invertTree (Branch ("a",1) Empty Empty) returns Branch (1,"a") Empty Empty

invertTree :: Tree (a, b) -> Tree (b, a)
invertTree = mapTree swap

tinvertTree :: Test
tinvertTree = "invertTree" ~: (invertTree (Branch ("a", 1) Empty Empty) ~?= Branch (1, "a") Empty Empty)

-- takeWhileTree, applied to a predicate p and a tree t,
-- returns the largest prefix tree of t  (possibly empty)
-- where all elements satisfy p.
-- For example, given the following tree

tree1 :: Tree Int
tree1 = Branch 1 (Branch 2 Empty Empty) (Branch 3 Empty Empty)

--     takeWhileTree (< 3) tree1  ~?= Branch 1 (Branch 2 Empty Empty) Empty
--     takeWhileTree (< 9) tree1  ~?= tree1
--     takeWhileTree (< 0) tree1  ~?= Empty

takeWhileTree :: (a -> Bool) -> Tree a -> Tree a
takeWhileTree f = foldTree (\a l r -> if f a then Branch a l r else Empty) Empty

ttakeWhileTree :: Test
ttakeWhileTree =
  "takeWhileTree"
    ~: [ takeWhileTree (< 3) tree1 ~?= Branch 1 (Branch 2 Empty Empty) Empty,
         takeWhileTree (< 9) tree1 ~?= tree1,
         takeWhileTree (< 0) tree1 ~?= Empty
       ]

-- allTree pred tree returns False if any element of tree
-- fails to satisfy pred and True otherwise.
-- for example:
--    allTree odd tree1 returns False

allTree :: (a -> Bool) -> Tree a -> Bool
allTree f = foldTree (\x b c -> f x && b && c) True

tallTree :: Test
tallTree = "allTree" ~: (allTree odd tree1 ~?= False)

-- WARNING: This one is a bit tricky!  (Hint: the value

-- * returned* by foldTree can itself be a function.)

-- map2Tree f xs ys returns the tree obtained by applying f to
-- to each pair of corresponding elements of xs and ys. If
-- one branch is longer than the other, then the extra elements
-- are ignored.
-- for example:
--    map2Tree (+) (Branch 1 Empty (Branch 2 Empty Empty)) (Branch 3 Empty Empty)
--        should return (Branch 4 Empty Empty)

map2Tree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2Tree f = foldTree g (const Empty)
  where
    g a l r (Branch b l' r') = Branch (f a b) (l l') (r r')
    g a l r Empty = Empty

-- >>> map2Tree (+) (Branch 1 Empty (Branch 2 Empty Empty)) (Branch 3 Empty Empty)

tmap2Tree :: Test
tmap2Tree = "map2Tree" ~: (map2Tree (+) (Branch 1 Empty (Branch 2 Empty Empty)) (Branch 3 Empty Empty) ~?= Branch 4 Empty Empty)

----------------------------------------------------------------------

testFoldTree :: Test
testFoldTree = TestList [tinfixOrder1, tinfixOrder2, trevOrder, tfoldrTree', tfoldlTree']

infixOrder :: Tree a -> [a]
infixOrder Empty = []
infixOrder (Branch x l r) = infixOrder l ++ [x] ++ infixOrder r

exTree :: Tree Int
exTree =
  Branch
    5
    (Branch 2 (Branch 1 Empty Empty) (Branch 4 Empty Empty))
    (Branch 9 Empty (Branch 7 Empty Empty))

testInfixOrder = "infixOrder" ~: infixOrder exTree ~?= [1, 2, 4, 5, 9, 7]

infixOrder1 :: Tree a -> [a]
infixOrder1 = foldTree f []
  where
    f a l r = l ++ [a] ++ r

-- >>> infixOrder2 exTree
-- [1,2,4,5,9,7]
--

tinfixOrder1 = "infixOrder2" ~: infixOrder1 exTree ~?= [1, 2, 4, 5, 9, 7]

foldrTree :: (a -> b -> b) -> b -> Tree a -> b
foldrTree _ e Empty = e
foldrTree f e (Branch k l r) = foldrTree f (f k (foldrTree f e r)) l

infixOrder2 :: Tree a -> [a]
infixOrder2 = foldrTree (:) []

tinfixOrder2 = "infixOrder2" ~: infixOrder2 exTree ~?= [1, 2, 4, 5, 9, 7]

foldlTree :: (b -> a -> b) -> b -> Tree a -> b
foldlTree f e Empty = e
foldlTree f e (Branch k l r) = foldlTree f (f (foldlTree f e l) k) r

revOrder :: Tree a -> [a]
revOrder = foldlTree (flip (:)) []

-- >>> revOrder exTree
-- [7,9,5,4,2,1]
--

trevOrder = "revOrder" ~: revOrder exTree ~?= [7, 9, 5, 4, 2, 1]

foldrTree' :: (a -> b -> b) -> b -> Tree a -> b
foldrTree' f acc tree = foldTree g id tree acc
  where
    g a l r x = l (f a (r x))

-- >>> foldrTree' (+) 0 tree1
-- 6
--
tfoldrTree' :: Test
tfoldrTree' = TestList ["foldrTree'" ~: foldrTree' (+) 0 tree1 ~?= 6]

foldlTree' :: (b -> a -> b) -> b -> Tree a -> b
foldlTree' f acc tree = foldTree g id tree acc
  where
    g a l r x = r (f (l x) a)

-- >>> foldlTree' (+) 0 tree1
-- 6
--
-- >>> foldlTree (flip (:)) [] tree1
-- [3,1,2]
--
tfoldlTree' :: Test
tfoldlTree' = TestList ["foldlTree'" ~: foldlTree (+) 0 tree1 ~?= 6]

answer1 :: String
answer1 = "infixOrder2"

----------------------------------------------------------------------
br = [Element "br" []]

formatPlay :: SimpleXML -> SimpleXML
formatPlay (Element "PLAY" children) =
  Element "html" [Element "body" (concatMap (f 1) children)]
formatPlay _ = error "formatPlay: not a PLAY element"

 
f :: Int -> SimpleXML -> [SimpleXML]
f n x@(PCDATA _) = [x]
f n (Element "TITLE" t) = [Element ("h" ++ show n) t]
f n (Element "SPEAKER" s) = Element "b" (concatMap (f n) s) : br
f n (Element x s) | x == "PERSONA" || x == "LINE" = s ++ br
f n (Element "PERSONAE" xs) = concatMap (f (n + 1)) (Element "TITLE" [PCDATA "Dramatis Personae"] : xs)
f n (Element _ xs) = concatMap (f (n + 1)) xs -- ACT SCENE SPEECH

firstDiff :: Eq a => [a] -> [a] -> Maybe ([a], [a])
firstDiff [] [] = Nothing
firstDiff (c : cs) (d : ds)
  | c == d = firstDiff cs ds
  | otherwise = Just (c : cs, d : ds)
firstDiff cs ds = Just (cs, ds)

-- | Test the two files character by character, to determine whether
-- they match.
testResults :: String -> String -> IO ()
testResults file1 file2 = do
  f1 <- readFile file1
  f2 <- readFile file2
  case firstDiff f1 f2 of
    Nothing -> return ()
    Just (cs, ds) -> assertFailure msg
      where
        msg =
          "Results differ: '" ++ take 20 cs
            ++ "' vs '"
            ++ take 20 ds

testXML :: Test
testXML = TestCase $ do
  writeFile "dream.html" (xml2string (formatPlay play))
  testResults "dream.html" "./data/HW2/sample.html"

-- Cases: 1  Tried: 0  Errors: 0  Failures: 0
--
-- Cases: 1  Tried: 1  Errors: 0  Failures: 0
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}
--
-- >>> runTestTT testXML

-----------------------------------------------------------------------------

answer2 :: String
answer2 = undefined
