{-# LANGUAGE ExistentialQuantification #-}

module S07_Combinator
  () where

(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
f >>> g = \x -> g (f x)

(|>) :: a -> (a -> b) -> b
v |> f = f v

count1 :: (t -> Bool) -> [t] -> Int
count1 cond = filter cond >>> length

count2 :: (t -> Bool) -> [t] -> Int
count2 cond l = l |> filter cond |> length
