import Data.String
import Data.Vect
ack : Nat -> Nat -> Nat
ack Z n = n + 1 
ack (S k) Z = ack k 1
ack (S m) (S n) = ack m (ack (S m) n)

diag : a -> Pair a a
diag a = (a, a)
anyway : Either a a -> a
anyway (Left x) = x
anyway (Right x) = x
assocr : Pair (Pair a b) c -> Pair a (Pair b c)
assocr ((a, b), c) = (a, (b, c))
distl : Pair a (Either b c) -> Either (Pair a b) (Pair a c)
distl (a, (Left x))  = Left (a, x)
distl (a, (Right x))  = Right (a, x)


consolidate : List(Maybe a) -> Maybe (List a)
consolidate xs = foldr (\x, xs => map (::) x <*> xs) (Just []) xs   
 

transform : (a -> a) -> (index : Nat) -> List a -> List a 
transform _ _ [] = []
transform f Z (x::xs) = (f x) ::xs 
transform f (S k) (x::xs) = x :: (transform  f k xs)


capitilize_first_word : String -> String 
capitilize_first_word  = unwords . map pack . map f  . map unpack . words where 
    f : List Char -> List Char 
    f [] = []
    f (x::xs) = toUpper x :: xs

testC : String
testC = capitilize_first_word "asdasd asd asqweqw asdas xzcxzc"

Matrix : Nat -> Nat -> Type -> Type
Matrix m n t = Vect m (Vect n t)

add : Matrix m n Integer -> Matrix m n Integer -> Matrix m n Integer
add (x::xs) (y::ys) = (zipWith (+) x y) :: (add xs ys) where 
add [] [] = []

m1 :  Matrix 2 2 Integer
m1 = add [[1,2], [3, 4]] [[4,5], [6, 7]]