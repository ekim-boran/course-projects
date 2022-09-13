--{-# LANGUAGE RankNTypes  #-}
--
--
--newtype Number = Number { run :: (forall a. (a -> a) -> a -> a ) }
--type MyBool = (forall a. a -> a -> a)
--
---- basic arithmetic
--zero :: Number
--zero = Number$  \f x  -> x 
--
--one :: Number
--one = Number $ \ f x  -> f x 
--
--add :: Number -> Number -> Number
--add num1 num2  = Number$  \f x -> run num1 f ((run num2) f x )
--
--multiply num1 num2 = Number$  \f x -> run num1 (run num2 f)  x
--
--
--
--two =  add one one  
--four = add two two 
--
--testAdd = (run $ add two two) (+1) 0
--testMul = (run $ multiply four four) (+1) 0
--
---- boolean 
--false :: MyBool
--false x y = y 
--true :: MyBool
--true x y = x 
--
--and' :: MyBool -> MyBool -> MyBool
--and' b1 b2 = b1 b2 false 
--
--or' b1 b2 = b1 true b2
--
--
--
--isZero :: Number -> MyBool
--isZero num = (run num) (\_ -> false) true
--
--
--
--testIsZero = isZero zero 1 2 
--testIsZero1 = isZero one 1 2 
--
--
---- pairs 
--
--
--type Pair a = ( (a -> a -> a) -> a)
--
--mkPair :: a -> a -> Pair a 
--mkPair a b f = f a b 
--
--fst' :: Pair a -> a 
--fst' p = p true 
--snd' p = p false
--
--
--testPair = fst' $ mkPair 1 2 
--
---- 
--
--dec :: Number -> Number
--dec num1  = Number $ \f x -> snd' $ (run num1) (\a -> mkPair (f (fst' a)) (fst' a)) (mkPair x x )
--
--testDec = run (dec four) (+1) 0
--
--sub :: Number -> Number -> Number
--sub num1 num2  = Number $ \f x -> (run a) f x   where 
--    a = (run num2) dec num1    
--
--subTest = run (sub four two) (+1) 0
--
--
--myeq num1 num2 = and' (isZero (sub num1 num2)) (isZero (sub   num2 num1)) 
--
--
--myeqtest1 = myeq four one True False
--myeqtest2 = myeq  one four  True False
--myeqtest3 = myeq four four True False


main :: IO ()
main = return ()



--newtype Church = Church { unChurch :: forall a. (a -> a) -> (a -> a) }
--predChurch = \n -> Church $ 
--    \f -> \x -> unChurch n (\g -> \h -> h (g f)) (\u -> x) (\u -> u)
--subChurch = \m -> \n -> unChurch n predChurch m

 