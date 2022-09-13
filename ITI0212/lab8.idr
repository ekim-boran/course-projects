
import Data.List
data GaussianInteger : Type where
    Gauss : Integer -> Integer -> GaussianInteger

implementation Cast Integer GaussianInteger where
  cast x = Gauss x 0
implementation Num GaussianInteger where
  (+) (Gauss a b) (Gauss c d) = Gauss (a + c) (b + d)
  (*) (Gauss a b) (Gauss c d) = Gauss (a * c - b * d) (b * c + a * d)
  fromInteger n = cast n




implementation Neg GaussianInteger where 
    negate (Gauss a b) = Gauss (-a) (-b)
    (-) (Gauss a b) (Gauss c d) = Gauss (a - c) (b - d)

implementation Eq GaussianInteger where
    (== )(Gauss a b) (Gauss c d) = a == c && d == b
implementation [lex] Ord GaussianInteger where
    compare (Gauss a b) (Gauss c d) = 
        case compare a c of
            EQ => compare b d
            x => x
            
implementation [mag] Ord GaussianInteger where
    compare (Gauss a b) (Gauss c d) = compare (a * a + b *b) (c * c + d * d)
       
            
implementation [setwise] Eq a => Eq (List a) where  
    (==) xs ys = all (\x => elem x ys) xs && all (\y => elem y xs) ys 


implementation [multisetwise] Eq a => Eq (List a)  where
    (==) [] [] = True
    (==) (x::xs) ys  = xs == delete x ys
    (==) _ _ = False
  

a : Bool
a = (==) @{multisetwise} [1,2,3] [3,2,1]
b : Bool

b = (==) @{multisetwise} [1,2,3] [1,2,3,3]
c : Bool

c = (==) @{multisetwise} [1,2,3] [1,2,4]
 