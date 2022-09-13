beep : (Pair a b -> c) -> (a -> b -> c)
beep  f a b = f (a, b)
boop : (a -> b -> c) -> (Pair a b -> c)
boop f (a, b)= f a b

conjunction : Bool -> Bool -> Bool
conjunction  True x = x 
conjunction  _ _ = False

disjunction : Bool -> Bool -> Bool
disjunction  False x = x
disjunction  _ _ = True


foldList : (a -> b -> b) -> b -> List a -> b
foldList f b []  = b
foldList f b (x :: xs)  = f (x) (foldList f b xs)

conj : List Bool -> Bool
conj = foldList (conjunction) True

disj : List Bool -> Bool
disj = foldList (conjunction) False

filterList : (a -> Bool) -> List a -> List a
filterList p xs = foldList (\x, xs => if p x then x::xs else xs ) []  xs

-- question has wrong type for binary tree
data Tree : Type -> Type where
  Leaf : Tree a
  Node : Tree a -> a -> Tree a -> Tree a 

foldTree : (a -> b -> b) -> b -> Tree a -> b
foldTree f b Leaf = b
foldTree f b (Node l x r) = foldTree f (f x (foldTree f b r)) l
 
foldTree2 : (b-> a -> b -> b) -> b -> Tree a -> b
foldTree2 f b Leaf = b
foldTree2 f b (Node l x r) = f (foldTree2 f b l) x (foldTree2 f b r)

mapTree : (a -> b) -> Tree a -> Tree b 
mapTree f tree =  foldTree2 (\l, x, r => Node l (f x) r) Leaf tree 

-- TODO write maptree with foldTree

sumTree : Tree Nat -> Nat
sumTree   =  foldTree (+) 0 


mapTest : Tree Int
mapTest = mapTree (+1) $ Node (Node Leaf 1 Leaf) 3 (Node (Node Leaf 5 Leaf) 6 (Node Leaf 12 Leaf))