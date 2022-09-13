module S03_Expression
  () where

v1 :: Integer
v1 = 42 + 666

v2 :: Integer
v2 = v1 * 37

v3 :: Integer
v3 = if True then 42 else 666

v4 :: Maybe Integer
v4 = case [42, 666] of
  []         -> Nothing
  (head : _) -> Just head

complex1 :: Integer -- top-level variable also has names.
complex1 = a + b    -- what's a and b?
 where             -- mind the spaces.
  a = 1 + 2       -- too complex to inline!
  b = a * 3       -- too complex to inline!

complex2 :: Integer
complex2 =
  let a = 1 + 2     -- another way to give names.
      b = a * 3
  in  a + b

complex3 :: Integer
complex3 = a + b
 where
  a = (1 + 2) :: Integer
  b = (a * 3) :: Integer

bot :: t  -- meaning it can be any type, including the "Never"
bot = bot

ones :: [Integer]
ones = 1 : ones

empties :: [Integer]
empties = if True then [] else 1 : empties

ones_ten :: [Integer]
ones_ten = take 10 ones

bot_ten :: [Integer]
bot_ten = take 10 bot

onetwo1 :: [Integer]
onetwo1 = 1 : onetwo2

onetwo2 :: [Integer]
onetwo2 = 2 : onetwo1

onetwo :: [Integer]
onetwo = 1 : twoone where twoone = 2 : onetwo

onetwoShort :: [Integer]
onetwoShort = 1 : 2 : onetwoShort

integers = go 0 where go n = n : go (n + 1)

takeEven 0 _             = []
takeEven n (hd : _ : tl) = hd : takeEven (n - 1) tl
