import Data.Vect

data Shape : Type where
    Circle : (r: Nat) -> Shape
    Rectangle : (w: Nat) -> (h: Nat) -> Shape
    Polygon : (sides: Nat) -> (len: Nat) -> Shape  --sidelen sides
    IsoTriangle  : (base:Nat) -> (height : Nat) -> Shape -- base - height
    Star : (sides: Nat) -> (len: Nat) -> (base:Nat) -> (height : Nat) -> Shape


area : Shape -> Double
area (Circle r) =  pi * (cast r) * (cast r)
area (Rectangle w h) =  cast(w*h)
area (Polygon sides len) = let 
  p = cast(len*sides)
  a = cast(len) / (2 * tan( pi / cast(sides) ))   in
    p * a / 2   

area (IsoTriangle base height) = cast (base * height) / 2
area (Star sides len base height) = area(Polygon sides len) + (area(IsoTriangle base height) * cast(sides))  


indexList : (index : Nat) -> List a -> Maybe a
indexList _ [] = Nothing
indexList Z (x::xs) = Just x
indexList (S k) (x::xs) = indexList k xs

indexVect : (index : Fin n) -> Vect n a -> a
indexVect FZ (x::xs) =   x
indexVect (FS k) (x::xs) = indexVect k xs

data Tree : Type -> Type where
  Leaf : Tree a
  Node : (l : Tree a) -> (x : a) -> (r : Tree a) -> Tree a

zipTree : (a -> b -> c) -> Tree a -> Tree b -> Tree c
zipTree f Leaf t2 = Leaf
zipTree f _ Leaf = Leaf
zipTree f (Node l x r) (Node l' x' r') = Node (zipTree f l l') (f x x') (zipTree f r r')

foldMaybe : (f : a -> b) -> (n : b) -> Maybe a -> b
foldMaybe f n Nothing=n
foldMaybe f n (Just x)= f x

mapMaybe : (a -> b) -> Maybe a -> Maybe b
mapMaybe f x = foldMaybe (Just . f) Nothing x

tryIOs : List (IO (Either error Unit)) -> IO (Maybe error)
tryIOs [] = pure Nothing
tryIOs (x::xs) = do 
  e <- x
  case e of 
    Left e => pure (Just e)
    _ => tryIOs xs
    
filterEither : List(Either a b) -> List a
filterEither [] = []
filterEither (x::xs) = case x of
    (Left e) => e :: filterEither xs
    _ =>  filterEither xs


batchIOs : List (IO (Either error Unit)) -> IO (List error)
batchIOs xs =  map (filterEither)  (traverse id xs)   
 

