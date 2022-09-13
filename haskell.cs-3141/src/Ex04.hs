{-# LANGUAGE FlexibleContexts #-}
module Ex04 where
import           Control.Monad.State
import           Data.Char
import           Data.List
import           System.Environment
import           System.IO                      ( IOMode(ReadMode, WriteMode)
                                                , hGetLine
                                                , hIsEOF
                                                , hPutStrLn
                                                , withFile
                                                )
import           System.Random
import           Test.QuickCheck
import           Text.Read                      ( readMaybe )
onlyAlphabetic :: FilePath -> FilePath -> IO ()
onlyAlphabetic i o = filter (isAlpha) <$> (readFile i) >>= writeFile o


-- >>> fileProduct "input.txt" "output.txt"
--

fileProduct :: FilePath -> FilePath -> IO ()
fileProduct i o = withFile i ReadMode (withFile o WriteMode . processFile)
 where
  processFile i o = do
    b <- hIsEOF i
    if b
      then return ()
      else
        hGetLine i
        >>= (hPutStrLn o . show . product . fmap read . words)
        >>  processFile i o


data Player m = Player
  { guess :: m Int
  , wrong :: Answer -> m ()
  }
data Answer = Lower | Higher

guessingGame :: (Monad m) => Int -> Int -> Player m -> m Bool
guessingGame x n p = go n
 where
  go 0 = pure False
  go n = do
    x' <- guess p
    case compare x x' of
      LT -> wrong p Lower >> go (n - 1)
      GT -> wrong p Higher >> go (n - 1)
      EQ -> pure True

human :: Player IO
human = Player { guess = guess, wrong = wrong }
 where
  guess = do
    putStrLn "Enter a number (1-100):"
    x <- getLine
    case readMaybe x of
      Nothing -> guess
      Just i  -> pure i

  wrong Lower  = putStrLn "Lower!"
  wrong Higher = putStrLn "Higher!"

play :: IO ()
play = do
  x <- randomRIO (1, 100)
  b <- guessingGame x 5 human
  putStrLn (if b then "You got it!" else "You ran out of guesses!")


midpoint :: Int -> Int -> Int
midpoint lo hi | lo <= hi  = lo + div (hi - lo) 2
               | otherwise = midpoint hi lo

avg (l, h) = (l + h) `div` 2

ai :: Player (State (Int, Int))
ai = Player { guess = guess, wrong = wrong }
 where
  guess = gets avg
  wrong Lower  = modify (\(l, h) -> (l, avg (l, h) - 1))
  wrong Higher = modify (\(l, h) -> (avg (l, h) + 1, h))


-- >>> quickCheck prop_basic
-- +++ OK, passed 100 tests.
--
-- >>>  quickCheck  prop_optimality
-- +++ OK, passed 100 tests.
--
prop_basic (Positive n) =
  forAll (choose (1, n)) $ \x -> evalState (guessingGame x n ai) (1, n)

prop_optimality (Positive n) = forAll (choose (1, n))
  $ \x -> evalState (guessingGame x (bound n) ai) (1, n)
  where bound n = ceiling (logBase 2 (fromIntegral n)) + 1




prop1 :: String -> Bool
prop1 l = l == (unwords . words) l


prop1_pre :: [String] -> Property
prop1_pre l =
  all (\w -> not (any isSpace w) && w /= []) l ==> words (unwords l) == l

-- >>> quickCheck prop1_pre
-- *** Gave up! Passed only 59 tests; 1000 discarded tests.
--
