module Ex05 where

import           Text.Read                      ( readMaybe )

data Token = Number Int | Operator (Int -> Int -> Int)

parseToken :: String -> Maybe Token
parseToken "+" = Just (Operator (+))
parseToken "-" = Just (Operator (-))
parseToken "/" = Just (Operator div)
parseToken "*" = Just (Operator (*))
parseToken str = fmap Number (readMaybe str)

tokenise :: String -> Maybe [Token]
tokenise = traverse parseToken . words



newtype Calc a = C ([Int] -> Maybe ([Int], a))


pop :: Calc Int
pop = C $ \xs -> if null xs then Nothing else Just (tail xs, head xs)

push :: Int -> Calc ()
push i = C $ \xs -> Just (i : xs, ())


instance Functor Calc where
  fmap f (C sa) = C $ \s -> case sa s of
    Nothing      -> Nothing
    Just (s', a) -> Just (s', f a)

instance Applicative Calc where
  pure x = C (\s -> Just (s, x))
  C sf <*> C sx = C $ \s -> case sf s of
    Nothing      -> Nothing
    Just (s', f) -> case sx s' of
      Nothing       -> Nothing
      Just (s'', x) -> Just (s'', f x)

instance Monad Calc where
  return = pure
  C sa >>= f = C $ \s -> case sa s of
    Nothing      -> Nothing
    Just (s', a) -> unwrapCalc (f a) s'
    where unwrapCalc (C a) = a


action :: Token -> Calc ()
action (Number   a) = push a
action (Operator f) = do
  v <- (flip f) <$> pop <*> pop
  push v

evaluate :: [Token] -> Calc Int
evaluate ts = traverse action ts >> pop

calculate :: String -> Maybe Int
calculate = (>>= runCalc) . fmap evaluate . tokenise

runCalc :: Calc b -> Maybe b
runCalc (C f) = snd <$> f []
-- >>> calculate "3 4 +"
-- >>> calculate "3 4 - 5 +"
-- >>> calculate "3 4 2 / *"
-- >>> calculate "3 4 2 / * +"
-- Just 7
-- Just 4
-- Just 6
-- Nothing
--
